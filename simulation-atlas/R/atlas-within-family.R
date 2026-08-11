# Within-family link atlas: local adaptation of
# scripts/03b-simulation-within-family-links.R.

WITHIN_LINKS <- c("logit", "probit")

atlas_within_family_rows <- function() {
  make <- function(slice, label, generating_link, paper_anchor = FALSE,
                   n_subjects = 700L, k_trials = 15L, target_icc = 0.30,
                   reference_beta_intercept = 1.50,
                   reference_beta_group = -1.00,
                   reference_beta_condition = -1.00,
                   reference_beta_group_condition = 0,
                   beta_main_1_multiplier = 1,
                   beta_main_2_multiplier = 1,
                   varied_parameter = NA_character_, varied_value = NA_real_) {
    coefficient_scale <- if (generating_link == "logit") 1.65 else 1
    data.frame(
      family = "within_family",
      .source_slice = slice,
      paper_anchor = paper_anchor,
      scenario_label = label,
      generating_model = "Random-intercept binomial",
      generating_link = generating_link,
      fitted_model = "Binomial logit;Binomial probit",
      fitted_link = "logit;probit",
      base_seed = ATLAS_BASE_SEED,
      seed_rule = ATLAS_SEED_RULE,
      B = 300L,
      alpha = ATLAS_ALPHA,
      n_subjects = as.integer(n_subjects),
      k_trials = as.integer(k_trials),
      target_icc = target_icc,
      reference_beta_intercept = reference_beta_intercept,
      reference_beta_group = reference_beta_group,
      reference_beta_condition = reference_beta_condition,
      reference_beta_group_condition = reference_beta_group_condition,
      logit_probit_scale = 1.65,
      coefficient_scale = coefficient_scale,
      beta_intercept = coefficient_scale * reference_beta_intercept,
      beta_group = coefficient_scale * reference_beta_group,
      beta_condition = coefficient_scale * reference_beta_condition,
      beta_group_condition = coefficient_scale * reference_beta_group_condition,
      generating_interaction = 0,
      beta_main_1_multiplier = beta_main_1_multiplier,
      beta_main_2_multiplier = beta_main_2_multiplier,
      varied_parameter = varied_parameter,
      varied_value = varied_value,
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  for (link in WITHIN_LINKS) {
    rows[[length(rows) + 1L]] <- make(
      "paper_anchor", paste0(tools::toTitleCase(link), "-generated manuscript scenario"),
      link, paper_anchor = TRUE
    )
  }

  surface <- atlas_main_effect_surface()
  for (link in WITHIN_LINKS) {
    for (i in seq_len(nrow(surface))) {
      m1 <- surface$beta_main_1_multiplier[i]
      m2 <- surface$beta_main_2_multiplier[i]
      rows[[length(rows) + 1L]] <- make(
        "main_effect_surface",
        sprintf("%s generated: group x %.2f; condition x %.2f", link, m1, m2),
        link,
        reference_beta_group = -1 * m1,
        reference_beta_condition = -1 * m2,
        beta_main_1_multiplier = m1,
        beta_main_2_multiplier = m2,
        varied_parameter = "beta_group_multiplier;beta_condition_multiplier"
      )
    }
  }

  for (link in WITHIN_LINKS) {
    # Denser below the manuscript anchor (700). The paper notes that N was
    # chosen to make the logit/probit discrepancy detectable rather than to be
    # typical, so small-N behaviour is the more informative region here.
    # Must stay even: simulate_within_family() splits subjects into two groups.
    for (value in c(100L, 200L, 300L, 400L, 700L, 1200L, 2000L)) {
      rows[[length(rows) + 1L]] <- make(
        "sample_size", paste0(link, " generated: subjects = ", value), link,
        n_subjects = value, varied_parameter = "n_subjects", varied_value = value
      )
    }
    for (value in c(5L, 15L, 30L)) {
      rows[[length(rows) + 1L]] <- make(
        "trial_count", paste0(link, " generated: trials per cell = ", value), link,
        k_trials = value, varied_parameter = "k_trials", varied_value = value
      )
    }
    for (value in c(0, 0.75, 1.50, 2.25)) {
      rows[[length(rows) + 1L]] <- make(
        "latent_location", paste0(link, " generated: reference intercept = ", value), link,
        reference_beta_intercept = value,
        varied_parameter = "reference_beta_intercept", varied_value = value
      )
    }
    for (value in c(0, 0.15, 0.30, 0.45)) {
      rows[[length(rows) + 1L]] <- make(
        "icc", paste0(link, " generated: target ICC = ", value), link,
        target_icc = value, varied_parameter = "target_icc", varied_value = value
      )
    }
  }

  rows <- do.call(rbind, rows)
  atlas_dedupe_family(
    rows,
    c("n_subjects", "k_trials", "target_icc", "generating_link",
      "reference_beta_intercept", "reference_beta_group",
      "reference_beta_condition", "reference_beta_group_condition",
      "logit_probit_scale", "beta_intercept", "beta_group", "beta_condition",
      "beta_group_condition", "generating_model"),
    family = "within_family"
  )
}

within_inverse_link <- function(eta, link) {
  switch(link, logit = stats::plogis(eta), probit = stats::pnorm(eta),
         stop("Unknown within-family link: ", link, call. = FALSE))
}

within_forward_link <- function(probability, link) {
  probability <- atlas_clip_probability(probability)
  switch(link, logit = stats::qlogis(probability), probit = stats::qnorm(probability),
         stop("Unknown within-family link: ", link, call. = FALSE))
}

within_random_intercept_sd <- function(scenario) {
  if (!is.finite(scenario$target_icc) || scenario$target_icc < 0 || scenario$target_icc >= 1) {
    stop("target_icc must be in [0, 1).", call. = FALSE)
  }
  if (scenario$target_icc == 0) return(0)
  residual_variance <- if (scenario$generating_link == "logit") pi^2 / 3 else 1
  sqrt(scenario$target_icc * residual_variance / (1 - scenario$target_icc))
}

within_fixed_eta <- function(scenario, group_num, condition_num) {
  scenario$beta_intercept + scenario$beta_group * group_num +
    scenario$beta_condition * condition_num +
    scenario$beta_group_condition * group_num * condition_num
}

within_deterministic_quantities <- function(scenario, fitted_link) {
  grid <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  probability <- within_inverse_link(
    within_fixed_eta(scenario, grid$group_num, grid$condition_num),
    scenario$generating_link
  )
  fitted_value <- within_forward_link(probability, fitted_link)
  list(
    deterministic_pseudo_interaction = atlas_zero_small(atlas_difference_in_differences(
      fitted_value, grid$group_num, grid$condition_num
    )),
    deterministic_response_scale_did = atlas_difference_in_differences(
      probability, grid$group_num, grid$condition_num
    )
  )
}

simulate_within_family <- function(scenario, aggregate = TRUE) {
  if (scenario$n_subjects %% 2 != 0) stop("n_subjects must be even.", call. = FALSE)
  id <- rep(seq_len(scenario$n_subjects), each = 2)
  group_subject <- rep(c(0, 1), each = scenario$n_subjects / 2)
  group_num <- rep(group_subject, each = 2)
  condition_num <- rep(c(0, 1), times = scenario$n_subjects)
  random_effect <- stats::rnorm(scenario$n_subjects, sd = within_random_intercept_sd(scenario))
  eta <- within_fixed_eta(scenario, group_num, condition_num) + random_effect[id]
  probability <- within_inverse_link(eta, scenario$generating_link)
  if (any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Within-family generating probabilities are outside [0, 1].", call. = FALSE)
  }
  successes <- stats::rbinom(length(probability), scenario$k_trials, probability)
  data.frame(
    id = factor(id),
    group_num = group_num,
    condition_num = condition_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    condition = factor(condition_num, levels = c(0, 1),
                       labels = c("Condition 0", "Condition 1")),
    successes = successes,
    k = scenario$k_trials,
    y = successes / scenario$k_trials,
    stringsAsFactors = FALSE
  )
}

fit_within_model <- function(data, scenario, fitted_link) {
  if (scenario$target_icc == 0) {
    atlas_capture(stats::glm(
      cbind(successes, k - successes) ~ group * condition,
      data = data, family = stats::binomial(link = fitted_link)
    ))
  } else {
    atlas_capture(glmmTMB::glmmTMB(
      cbind(successes, k - successes) ~ group * condition + (1 | id),
      data = data, family = stats::binomial(link = fitted_link)
    ))
  }
}

within_fixed_coefficients <- function(fit) {
  if (is.null(fit)) return(NULL)
  if (inherits(fit, "glmmTMB")) return(glmmTMB::fixef(fit)$cond)
  stats::coef(fit)
}

within_fitted_did <- function(fit, fitted_link) {
  coefficients <- try(within_fixed_coefficients(fit), silent = TRUE)
  if (inherits(coefficients, "try-error") || is.null(coefficients) ||
      any(!is.finite(coefficients))) return(c(response = NA_real_, outcome = NA_real_))
  grid <- expand.grid(
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")),
    condition = factor(c("Condition 0", "Condition 1"),
                       levels = c("Condition 0", "Condition 1"))
  )
  design <- stats::model.matrix(~ group * condition, data = grid)
  common <- intersect(colnames(design), names(coefficients))
  if (length(common) != ncol(design)) return(c(response = NA_real_, outcome = NA_real_))
  probability <- within_inverse_link(as.vector(design[, common, drop = FALSE] %*%
                                                 coefficients[common]), fitted_link)
  did <- atlas_difference_in_differences(
    probability, as.integer(grid$group == "Group 1"),
    as.integer(grid$condition == "Condition 1")
  )
  c(response = did, outcome = did)
}

run_within_family_replication <- function(scenario, replication) {
  set.seed(atlas_seed("within_family", scenario$scenario_id, replication))
  data <- simulate_within_family(scenario)
  rows <- lapply(WITHIN_LINKS, function(fitted_link) {
    captured <- fit_within_model(data, scenario, fitted_link)
    fit <- captured$value
    statistics <- atlas_interaction_stats(fit)
    problem <- atlas_fit_problem(fit)
    did <- within_fitted_did(fit, fitted_link)
    deterministic <- within_deterministic_quantities(scenario, fitted_link)
    data.frame(
      scenario_id = scenario$scenario_id,
      family = "within_family",
      replication = replication,
      replication_seed = atlas_seed("within_family", scenario$scenario_id, replication),
      model_label = paste("Binomial", fitted_link),
      fitted_link = fitted_link,
      fit_structure = if (scenario$target_icc == 0) "GLM" else "random-intercept GLMM",
      interaction_p = statistics$p,
      interaction_coef = statistics$coef,
      interaction_se = statistics$se,
      response_scale_did = unname(did["response"]),
      outcome_scale_did = unname(did["outcome"]),
      deterministic_pseudo_interaction = deterministic$deterministic_pseudo_interaction,
      deterministic_response_scale_did = deterministic$deterministic_response_scale_did,
      fit_success = is.finite(statistics$p) && !problem,
      convergence_problem = problem,
      problem_message = atlas_problem_text(captured, fit),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
