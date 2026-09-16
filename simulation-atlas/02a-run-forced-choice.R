# forced_choice: same DGP and model calculations as scripts/03a-simulation-forced-choice.R.
# Run from the repository root after 01-build-scenario-grid.R.
# Replications are parallel; scenarios are visited in declared grid order.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
MODE <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (MODE == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK",
      max(1, parallel::detectCores() - 1))))
OVERWRITE <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)
grid <- utils::read.csv("simulation-atlas/data/scenario-grid.csv", stringsAsFactors = FALSE)
scenarios <- grid[grid$family == "forced_choice", ]
if (MODE == "smoke") scenarios <- scenarios[scenarios$scenario_id == "FC-002", ]

capture_fit <- function(expr) {
  warning_text <- character()
  error_text <- ""
  fit <- tryCatch(
    withCallingHandlers(expr, warning = function(w) {
      warning_text <<- c(warning_text, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      error_text <<- conditionMessage(e)
      NULL
    }
  )
  list(fit = fit, warnings = unique(warning_text), error = error_text)
}

extract_mixed_fit <- function(captured, newdata, gaussian = FALSE) {
  targeted_pattern <- paste(
    "failed to converge", "identif", "Hessian",
    "positive definite", "degenerate", sep = "|"
  )
  if (is.null(captured$fit)) {
    return(data.frame(
      interaction_p = NA_real_, interaction_coef = NA_real_, interaction_se = NA_real_,
      response_scale_did = NA_real_, fit_problem = TRUE,
      problem_message = captured$error, singular = NA,
      warning_message = paste(captured$warnings, collapse = " | "),
      stringsAsFactors = FALSE
    ))
  }

  fit <- captured$fit
  lme4_messages <- fit@optinfo$conv$lme4$messages
  if (is.null(lme4_messages)) lme4_messages <- character()
  singular <- lme4::isSingular(fit, tol = 1e-4)
  extraction_warnings <- character()
  extraction_error <- ""
  coefficient_table <- tryCatch(
    withCallingHandlers(
      stats::coef(summary(fit)),
      warning = function(w) {
        extraction_warnings <<- c(extraction_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      extraction_error <<- conditionMessage(e)
      NULL
    }
  )
  term <- "age_c:groupGroup 1"
  estimate <- if (!is.null(coefficient_table) && term %in% rownames(coefficient_table)) coefficient_table[term, 1] else NA_real_
  standard_error <- if (!is.null(coefficient_table) && term %in% rownames(coefficient_table)) coefficient_table[term, 2] else NA_real_
  p_value <- if (is.finite(estimate) && is.finite(standard_error) && standard_error > 0) {
    2 * stats::pnorm(abs(estimate / standard_error), lower.tail = FALSE)
  } else {
    NA_real_
  }
  predictions <- try(
    if (gaussian) {
      stats::predict(fit, newdata = newdata, re.form = NA)
    } else {
      stats::predict(fit, newdata = newdata, type = "response", re.form = NA)
    },
    silent = TRUE
  )
  did <- if (inherits(predictions, "try-error") || any(!is.finite(predictions))) {
    NA_real_
  } else {
    predictions[4] - predictions[2] - predictions[3] + predictions[1]
  }
  targeted_warnings <- c(captured$warnings, extraction_warnings)
  targeted_warnings <- targeted_warnings[
    grepl(targeted_pattern, targeted_warnings, ignore.case = TRUE)
  ]
  invalid_inference <- !is.finite(estimate) || !is.finite(standard_error) ||
    standard_error <= 0 || !is.finite(p_value)
  fit_problem <- length(lme4_messages) > 0 || isTRUE(singular) ||
    length(targeted_warnings) > 0 || nzchar(extraction_error) || invalid_inference
  problem_parts <- c(
    lme4_messages,
    if (isTRUE(singular)) "singular fit" else character(),
    targeted_warnings,
    extraction_error,
    if (invalid_inference) "non-finite or non-positive interaction inference" else character()
  )
  problem_parts <- problem_parts[!is.na(problem_parts) & nzchar(problem_parts)]
  data.frame(
    interaction_p = unname(p_value),
    interaction_coef = unname(estimate),
    interaction_se = unname(standard_error),
    response_scale_did = unname(did),
    fit_problem = fit_problem,
    problem_message = paste(unique(problem_parts), collapse = " | "),
    singular = singular,
    warning_message = paste(
      unique(c(captured$warnings, extraction_warnings)), collapse = " | "
    ),
    stringsAsFactors = FALSE
  )
}

run_one_replication <- function(replication, scenario, deterministic) {
  settings <- as.list(scenario)
  replication_seed <- as.integer((20260807 + 1000000 +
        as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  settings$age_range <- c(scenario$age_min, scenario$age_max)
  settings$sigma_u <- sqrt(
    settings$target_icc * (pi^2 / 3) / (1 - settings$target_icc)
  )

  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  u <- stats::rnorm(settings$N, mean = 0, sd = settings$sigma_u)
  # The product term is zero on the conditional chance-corrected-logit scale.
  eta <- settings$beta_intercept + settings$beta_age * age_c +
    settings$beta_group * group_num + settings$beta_age_group * age_c * group_num + u
  d <- data.frame(
    id = factor(rep(seq_len(settings$N), each = settings$k_trials)),
    age = rep(age, each = settings$k_trials),
    age_c = rep(age_c, each = settings$k_trials),
    group = factor(
      rep(group_num, each = settings$k_trials),
      levels = c(0, 1), labels = c("Group 0", "Group 1")
    ),
    stringsAsFactors = FALSE
  )
  d$eta <- rep(eta, each = settings$k_trials)
  d$p <- settings$chance + (1 - settings$chance) * stats::plogis(d$eta)
  d$correct <- stats::rbinom(nrow(d), size = 1, prob = d$p)

  fit_gaussian <- capture_fit(lme4::lmer(
    correct ~ age_c * group + (1 | id), data = d
  ))
  fit_logit <- capture_fit(lme4::glmer(
    correct ~ age_c * group + (1 | id),
    family = stats::binomial("logit"), data = d
  ))
  fit_probit <- capture_fit(lme4::glmer(
    correct ~ age_c * group + (1 | id),
    family = stats::binomial("probit"), data = d
  ))
  fitted_alternatives <- as.integer(round(1 / settings$chance))
  fit_chance <- capture_fit(lme4::glmer(
    correct ~ age_c * group + (1 | id),
    family = stats::binomial(psyphy::mafc.logit(fitted_alternatives)), data = d
  ))

  nd <- expand.grid(
    age = settings$age_range,
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  nd$age_c <- nd$age - settings$age_center
  extracted <- rbind(
    extract_mixed_fit(fit_gaussian, nd, gaussian = TRUE),
    extract_mixed_fit(fit_logit, nd),
    extract_mixed_fit(fit_probit, nd),
    extract_mixed_fit(fit_chance, nd)
  )
  extracted$outcome_scale_did <- extracted$response_scale_did * settings$k_trials
  extracted$scenario_id <- scenario$scenario_id
  extracted$family <- "forced_choice"
  extracted$replication <- replication
  extracted$replication_seed <- replication_seed
  extracted$model_label <- c(
    "Gaussian identity", "Standard binomial logit", "Standard binomial probit",
    "Chance-corrected binomial logit"
  )
  extracted$fitted_link <- c("identity", "logit", "probit", "chance-corrected logit")
  extracted$deterministic_pseudo_interaction <- deterministic$pseudo
  extracted$deterministic_response_scale_did <- deterministic$response_did
  extracted$fit_success <- is.finite(extracted$interaction_p) & !extracted$fit_problem
  extracted$convergence_problem <- extracted$fit_problem
  extracted[, c(
    "scenario_id", "family", "replication", "replication_seed", "model_label",
    "fitted_link", "interaction_p", "interaction_coef", "interaction_se",
    "response_scale_did", "outcome_scale_did", "deterministic_pseudo_interaction",
    "deterministic_response_scale_did", "fit_success", "convergence_problem",
    "fit_problem", "problem_message", "singular", "warning_message"
  )]
}

cluster <- NULL
if (n_cores > 1 && .Platform$OS.type != "unix") {
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(cluster, c("capture_fit", "extract_mixed_fit"))
}
for (i in seq_len(nrow(scenarios))) {
  scenario <- scenarios[i, ]
  output_file <- file.path("simulation-atlas/raw",
    sprintf("core-%s-%s-B%d.rds", scenario$scenario_id, MODE, B))
  if (file.exists(output_file) && !OVERWRITE) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")
  # Deterministic geometry is conditional at random intercept = 0.
  nd <- expand.grid(age = c(scenario$age_min, scenario$age_max), group_num = c(0, 1))
  eta <- scenario$beta_intercept + scenario$beta_age * (nd$age - scenario$age_center) +
    scenario$beta_group * nd$group_num +
    scenario$beta_age_group * (nd$age - scenario$age_center) * nd$group_num
  probability <- scenario$chance + (1 - scenario$chance) * stats::plogis(eta)
  p <- pmin(pmax(probability, 1e-10), 1 - 1e-10)
  q <- pmin(pmax((probability - scenario$chance) / (1 - scenario$chance), 1e-10), 1 - 1e-10)
  fitted_values <- cbind(probability, stats::qlogis(p), stats::qnorm(p), stats::qlogis(q))
  pseudo <- fitted_values[4, ] - fitted_values[2, ] - fitted_values[3, ] + fitted_values[1, ]
  pseudo[is.finite(pseudo) & abs(pseudo) < 1e-12] <- 0
  deterministic <- list(
    pseudo = unname(pseudo),
    response_did = probability[4] - probability[2] - probability[3] + probability[1]
  )
  if (n_cores > 1 && .Platform$OS.type == "unix") {
    result <- parallel::mclapply(seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic,
      mc.cores = n_cores, mc.set.seed = FALSE)
  } else if (!is.null(cluster)) {
    result <- parallel::parLapply(cluster, seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic)
  } else {
    result <- lapply(seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic)
  }
  result <- do.call(rbind, result)
  result$B_requested <- B
  result$run_type <- MODE
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
