# Targeted diagnostic sensitivity atlas. The applicable checks mirror the
# stable portions of scripts/04-diagnostic-worked-example.R.

# Collapse diagnostic rows that describe the same scenario into one, recording
# every slice they belong to. Shared by the forced-choice and within-family
# blocks, which differ only in which columns identify a duplicate.
atlas_dedupe_diagnostic <- function(rows, signature_columns) {
  key <- atlas_signature(rows, signature_columns)
  groups <- split(seq_len(nrow(rows)), factor(key, levels = unique(key)))
  out <- do.call(rbind, lapply(groups, function(index) {
    first <- rows[index[1], , drop = FALSE]
    first$diagnostic_slice_membership <-
      paste(unique(rows$diagnostic_source_slice[index]), collapse = ";")
    first$diagnostic_paper_anchor <- any(rows$diagnostic_paper_anchor[index])
    first
  }))
  out$diagnostic_slice_membership <- ifelse(
    out$diagnostic_paper_anchor,
    paste0("diagnostic_anchor;", out$diagnostic_slice_membership),
    out$diagnostic_slice_membership
  )
  rownames(out) <- NULL
  out
}

atlas_diagnostic_plan <- function(grid) {
  make_rows <- function(base, changes, slice, anchor_test) {
    lapply(seq_len(nrow(changes)), function(i) {
      out <- base
      for (nm in names(changes)) out[[nm]] <- changes[[nm]][i]
      out$diagnostic_source_slice <- slice
      out$diagnostic_paper_anchor <- isTRUE(anchor_test(out))
      out
    })
  }

  forced_base <- grid[grid$family == "forced_choice" & grid$paper_anchor &
                        abs(grid$beta_intercept + 0.8) < 1e-12, , drop = FALSE]
  forced_sample <- data.frame(N = c(100L, 250L, 500L, 1000L))
  forced_severity <- data.frame(beta_intercept = c(-1.50, -0.80, 0, 0.80, 1.50))
  forced_anchor <- function(x) x$N == 250L && abs(x$beta_intercept + 0.8) < 1e-12
  forced <- atlas_dedupe_diagnostic(
    do.call(rbind, c(
      make_rows(forced_base, forced_sample, "sample_size", forced_anchor),
      make_rows(forced_base, forced_severity, "severity", forced_anchor)
    )),
    c("N", "k_trials", "chance", "age_min", "age_max", "age_center",
      "beta_intercept", "beta_age", "beta_group", "beta_age_group")
  )
  forced$scenario_id <- sprintf("D-FC-%03d", seq_len(nrow(forced)))
  forced$wrong_fitted_link <- "logit"
  forced$wrong_model_label <- "Standard binomial logit"
  forced$diagnostic_note <- "Paper framework: chance-corrected logit generated, standard logit fitted."

  within_base <- grid[grid$family == "within_family" & grid$paper_anchor &
                        grid$generating_link == "probit", , drop = FALSE]
  within_base$n_subjects <- 300L # exact scripts/04 diagnostic anchor
  within_sample <- data.frame(n_subjects = c(200L, 300L, 700L, 1200L))
  within_severity <- data.frame(reference_beta_intercept = c(0, 0.75, 1.50, 2.25))
  within_anchor <- function(x) x$n_subjects == 300L &&
    abs(x$reference_beta_intercept - 1.50) < 1e-12
  within <- do.call(rbind, c(
    make_rows(within_base, within_sample, "sample_size", within_anchor),
    make_rows(within_base, within_severity, "severity", within_anchor)
  ))
  # Reference-scale location changes must update the actual probit coefficient.
  within$beta_intercept <- within$reference_beta_intercept
  within <- atlas_dedupe_diagnostic(
    within,
    c("n_subjects", "k_trials", "target_icc", "generating_link", "beta_intercept",
      "beta_group", "beta_condition", "beta_group_condition")
  )
  within$scenario_id <- sprintf("D-WF-%03d", seq_len(nrow(within)))
  within$wrong_fitted_link <- "logit"
  within$wrong_model_label <- "Binomial logit GLMM"
  within$diagnostic_note <- "Exact paper diagnostic anchor uses 300 subjects; probit generated, logit fitted."

  # The paper diagnostic framework does not define a same-response, same-formula
  # diagnostic comparison for the sum-score models. Retain one explicit row so
  # the app reports structural inapplicability instead of silently omitting it.
  sum_base <- grid[grid$family == "sum_scores" & grid$paper_anchor &
                     abs(grid$threshold_shift) < 1e-12, , drop = FALSE]
  sum_base$scenario_id <- "D-SS-001"
  sum_base$diagnostic_source_slice <- "diagnostic_not_defined"
  sum_base$diagnostic_slice_membership <- "diagnostic_not_defined"
  sum_base$diagnostic_paper_anchor <- FALSE
  sum_base$wrong_fitted_link <- NA_character_
  sum_base$wrong_model_label <- "No comparable paper diagnostic"
  sum_base$diagnostic_note <- paste(
    "The paper does not compare diagnostics for the sum-score models because",
    "their response variables/scales do not support a same-likelihood AIC comparison."
  )

  plan <- atlas_bind_rows(list(forced, sum_base, within))
  plan$paper_anchor <- plan$diagnostic_paper_anchor
  plan$slice <- plan$diagnostic_source_slice
  plan$slice_membership <- plan$diagnostic_slice_membership
  if (anyDuplicated(plan$scenario_id)) stop("Diagnostic scenario_id is not unique.", call. = FALSE)
  if (any(plan$generating_interaction != 0)) stop("Diagnostic generating interactions must be zero.", call. = FALSE)
  rownames(plan) <- NULL
  plan
}

atlas_safe_aic <- function(fit) {
  if (is.null(fit)) return(NA_real_)
  if (inherits(fit, "atlas_chance_binomial")) return(fit$aic)
  value <- try(stats::AIC(fit), silent = TRUE)
  if (inherits(value, "try-error") || length(value) != 1 || !is.finite(value)) NA_real_ else unname(value)
}

atlas_test_p <- function(expr) {
  value <- try(suppressMessages(suppressWarnings(expr)), silent = TRUE)
  if (inherits(value, "try-error") || is.null(value$p.value) || length(value$p.value) != 1) {
    NA_real_
  } else {
    unname(value$p.value)
  }
}

atlas_dharma_empty <- function() {
  data.frame(
    dharma_uniformity_p = NA_real_,
    dharma_dispersion_p = NA_real_,
    dharma_quantile_fitted_p = NA_real_,
    dharma_quantile_predictor_p = NA_real_,
    dharma_categorical_design_p = NA_real_,
    stringsAsFactors = FALSE
  )
}

atlas_dharma_diagnostics <- function(fit, data, focal, n_sim, family) {
  empty <- atlas_dharma_empty()
  if (is.null(fit) || !requireNamespace("DHARMa", quietly = TRUE)) return(empty)
  # Only run checks this family declares applicable. The summarizer discards
  # the rest, and testQuantiles over fitted values is by far the most expensive
  # check, so computing an inapplicable one is pure waste.
  applicability <- atlas_diagnostic_applicability(family)
  applicable <- function(name) {
    isTRUE(applicability$applicable[match(name, applicability$diagnostic)])
  }
  simulated <- try(suppressMessages(suppressWarnings(DHARMa::simulateResiduals(
    fittedModel = fit, n = n_sim, plot = FALSE, seed = NULL
  ))), silent = TRUE)
  if (inherits(simulated, "try-error")) return(empty)
  result <- empty
  if (applicable("DHARMa uniformity")) {
    result$dharma_uniformity_p <- atlas_test_p(DHARMa::testUniformity(simulated, plot = FALSE))
  }
  if (applicable("DHARMa dispersion")) {
    result$dharma_dispersion_p <- atlas_test_p(DHARMa::testDispersion(simulated, plot = FALSE))
  }
  if (applicable("DHARMa residual quantiles over fitted values")) {
    fitted <- try(as.numeric(stats::predict(fit, type = "response")), silent = TRUE)
    if (!inherits(fitted, "try-error") &&
        length(unique(round(fitted[is.finite(fitted)], 10))) >= 8) {
      result$dharma_quantile_fitted_p <- atlas_test_p(DHARMa::testQuantiles(simulated, plot = FALSE))
    }
  }
  if (applicable("DHARMa residual quantiles over focal predictor") &&
      focal == "continuous_age" && length(unique(round(data$age_c, 10))) >= 8) {
    result$dharma_quantile_predictor_p <- atlas_test_p(
      DHARMa::testQuantiles(simulated, predictor = data$age_c, plot = FALSE)
    )
  }
  if (applicable("DHARMa residual distribution across design cells") &&
      focal == "categorical_design") {
    design_cell <- interaction(data$group, data$condition, drop = TRUE)
    residual <- simulated$scaledResiduals
    if (length(residual) == length(design_cell) && nlevels(design_cell) >= 2) {
      result$dharma_categorical_design_p <- atlas_test_p(stats::kruskal.test(residual ~ design_cell))
    }
  }
  result
}

# Pregibon-style added-term link check. Mirrors pregibon_added_term_p() in
# scripts/04-diagnostic-worked-example.R, which refits the augmented formula
# with glmmTMB for mixed models, so the check applies to the within-family
# GLMM scenarios as well as to the forced-choice GLMs.
atlas_pregibon_p <- function(fit, data) {
  if (is.null(fit)) return(NA_real_)
  eta <- try(stats::predict(fit, type = "link"), silent = TRUE)
  if (inherits(eta, "try-error") || length(eta) != nrow(data)) return(NA_real_)
  augmented <- data
  augmented$eta_hat_sq <- as.numeric(eta)^2
  augmented_formula <- try(
    stats::update.formula(stats::formula(fit), . ~ . + eta_hat_sq), silent = TRUE
  )
  if (inherits(augmented_formula, "try-error")) return(NA_real_)
  captured <- if (inherits(fit, "glmmTMB")) {
    atlas_capture(glmmTMB::glmmTMB(
      augmented_formula, data = augmented, family = stats::family(fit)
    ))
  } else if (inherits(fit, "glm")) {
    atlas_capture(stats::glm(
      augmented_formula, data = augmented, family = stats::family(fit),
      start = c(stats::coef(fit), eta_hat_sq = 0)
    ))
  } else {
    return(NA_real_)
  }
  if (is.null(captured$value)) return(NA_real_)
  sm <- if (inherits(captured$value, "glmmTMB")) {
    try(summary(captured$value)$coefficients$cond, silent = TRUE)
  } else {
    try(stats::coef(summary(captured$value)), silent = TRUE)
  }
  if (inherits(sm, "try-error") || is.null(rownames(sm)) ||
      !"eta_hat_sq" %in% rownames(sm)) return(NA_real_)
  p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(p_col)) NA_real_ else unname(sm["eta_hat_sq", p_col])
}

simulate_within_diagnostic <- function(scenario) {
  if (scenario$n_subjects %% 2 != 0) stop("n_subjects must be even.", call. = FALSE)
  n_per_group <- scenario$n_subjects / 2
  id <- rep(seq_len(scenario$n_subjects), each = 2 * scenario$k_trials)
  group_by_subject <- rep(c(0, 1), each = n_per_group)
  data <- data.frame(
    id = factor(id),
    group_num = rep(group_by_subject, each = 2 * scenario$k_trials),
    condition_num = rep(rep(c(0, 1), each = scenario$k_trials),
                        times = scenario$n_subjects)
  )
  random_effect <- stats::rnorm(scenario$n_subjects, sd = within_random_intercept_sd(scenario))
  eta <- within_fixed_eta(scenario, data$group_num, data$condition_num) +
    random_effect[as.integer(data$id)]
  probability <- within_inverse_link(eta, scenario$generating_link)
  data$y <- stats::rbinom(nrow(data), 1, probability)
  data$group <- factor(data$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  data$condition <- factor(data$condition_num, levels = c(0, 1),
                           labels = c("Condition 0", "Condition 1"))
  data
}

fit_within_diagnostic <- function(data, link) {
  atlas_capture(glmmTMB::glmmTMB(
    y ~ group * condition + (1 | id), data = data,
    family = stats::binomial(link = link)
  ))
}

run_atlas_diagnostic_replication <- function(scenario, replication, dharma_n_sim) {
  set.seed(atlas_seed(scenario$family, scenario$scenario_id, replication, stream = "diagnostic"))
  if (scenario$family == "forced_choice") {
    data <- simulate_forced_choice(scenario)
    wrong <- fit_forced_model(data, scenario, "Standard binomial logit")
    correct <- fit_forced_model(data, scenario, "Chance-corrected binomial logit")
    focal <- "continuous_age"
  } else if (scenario$family == "within_family") {
    data <- simulate_within_diagnostic(scenario)
    wrong <- fit_within_diagnostic(data, "logit")
    correct <- fit_within_diagnostic(data, "probit")
    focal <- "categorical_design"
  } else {
    stop("No simulation diagnostics are defined for family: ", scenario$family, call. = FALSE)
  }
  wrong_fit <- wrong$value
  correct_fit <- correct$value
  interaction <- atlas_interaction_stats(wrong_fit)
  # NA replication count means DHARMa was deferred to a separate pass.
  dharma_computed <- !is.na(dharma_n_sim)
  dharma <- if (dharma_computed) {
    atlas_dharma_diagnostics(wrong_fit, data, focal, dharma_n_sim, scenario$family)
  } else {
    atlas_dharma_empty()
  }
  aic_correct <- atlas_safe_aic(correct_fit)
  aic_wrong <- atlas_safe_aic(wrong_fit)
  wrong_problem <- atlas_fit_problem(wrong_fit)
  applicability <- atlas_diagnostic_applicability(scenario$family)
  pregibon_applicable <- isTRUE(applicability$applicable[
    match("Pregibon-style added-term link check", applicability$diagnostic)
  ])
  problems <- unique(c(atlas_problem_text(wrong, wrong_fit),
                       atlas_problem_text(correct, correct_fit)))
  data.frame(
    scenario_id = scenario$scenario_id,
    family = scenario$family,
    replication = replication,
    replication_seed = atlas_seed(scenario$family, scenario$scenario_id, replication,
                                  stream = "diagnostic"),
    interaction_p = interaction$p,
    interaction_coef = interaction$coef,
    fit_success = is.finite(interaction$p) && !wrong_problem,
    convergence_problem = wrong_problem || atlas_fit_problem(correct_fit),
    problem_message = paste(problems[nzchar(problems)], collapse = "; "),
    aic_generating = aic_correct,
    aic_wrong = aic_wrong,
    aic_favors_generating = if (is.finite(aic_correct) && is.finite(aic_wrong))
      aic_correct <= aic_wrong else NA,
    dharma,
    dharma_computed = dharma_computed,
    pregibon_p = if (pregibon_applicable) atlas_pregibon_p(wrong_fit, data) else NA_real_,
    stringsAsFactors = FALSE
  )
}

atlas_diagnostic_applicability <- function(family) {
  diagnostics <- c(
    "AIC favors generating link",
    "DHARMa uniformity",
    "DHARMa dispersion",
    "DHARMa residual quantiles over fitted values",
    "DHARMa residual quantiles over focal predictor",
    "DHARMa residual distribution across design cells",
    "Pregibon-style added-term link check"
  )
  applicable <- switch(
    family,
    forced_choice = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    # Pregibon applies here too: the manuscript refits the augmented formula
    # with glmmTMB for the mixed-model scenarios.
    within_family = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
    sum_scores = rep(FALSE, length(diagnostics)),
    stop("Unknown diagnostic family.", call. = FALSE)
  )
  data.frame(diagnostic = diagnostics, applicable = applicable, stringsAsFactors = FALSE)
}
