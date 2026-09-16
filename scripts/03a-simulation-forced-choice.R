# scripts/03a-simulation-forced-choice.R
# Parallelized version of Simulation 1: forced-choice accuracy with a non-zero chance floor.
#
# This script first shows the deterministic scenario, then simulates data,
# then quantifies nominal rejection on the known generating scale and
# pseudo-interaction detection on alternative fitted scales. Scenario-specific
# parameters stay here, not in R/.

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
rm(list = ls())

library(ggplot2)
library(lme4)
library(psyphy)

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------
# Run from the repository root. Publication runs use 3000 replications.
B <- as.integer(Sys.getenv("N_SIM", "3000"))
default_alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
default_dpi <- 300
figure_width <- 7.2

for (path in c("tables", "figs", "outputs", "outputs/inspection")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
group_difference <- function(group0, group1) group1 - group0
change_in_group_difference <- function(low_group0, low_group1, high_group0, high_group1) {
  (high_group1 - high_group0) - (low_group1 - low_group0)
}

set.seed(20260525)

cat("\n", "Simulation 1: forced-choice accuracy with chance floor (Parallel)", "\n")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
settings <- list(
  N = 250,
  k_trials = 20,
  chance = 0.50,
  age_range = c(6, 10),
  age_center = 8,
  beta_age = 0.60,
  beta_group = -0.90,
  beta_age_group = 0.00,
  target_icc = 0.30,
  generating_link = "logit", # Describes the explicit DGP below.
  B = B,
  n_cores = as.integer(Sys.getenv(
      "N_CORES",
      Sys.getenv("SLURM_CPUS_PER_TASK", max(1, parallel::detectCores() - 1))
  )),
  alpha = default_alpha,
  output_scenario_table = "tables/scenario-table-forced-choice.csv",
  output_summary_table = "tables/simulation-summary-forced-choice.csv",
  output_figure_base = "figs/forced-choice-simulation",
  output_inspection_base = "outputs/inspection/forced-choice-effect-size-inspection",
  output_rds = "outputs/simulation-forced-choice.rds"
)
settings$sigma_u <- sqrt(
  settings$target_icc * (pi^2 / 3) / (1 - settings$target_icc)
)

# Derived local values. These belong to this script because they describe
# this specific simulation and its tables/plots.
settings$age_summary_values <- c(
  settings$age_range[1],
  settings$age_center,
  settings$age_range[2]
)
settings$age_plot_values <- seq(
  settings$age_range[1],
  settings$age_range[2],
  length.out = 200
)
settings$age_bin_breaks <- seq(
  settings$age_range[1],
  settings$age_range[2],
  length.out = 9
)

scenarios <- data.frame(
  scenario = c("Lower performance", "Middle performance", "Higher performance"),
  beta_intercept = c(-0.80, 0.00, 0.80),
  interpretation = c(
    "Predicted accuracies are closer to the .50 chance floor, so observed-scale compression is more visible.",
    "Predicted accuracies mostly remain in the middle of the admissible above-chance range.",
    "Predicted accuracies are higher, so the chance-floor problem is less dominant but the link still matters."
  ),
  stringsAsFactors = FALSE
)

model_names <- c(
  "Gaussian identity",
  "Standard binomial logit",
  "Standard binomial probit",
  "Chance-corrected binomial logit"
)

cat("\n", "Scenario parameters", "\n")
print(settings)
cat("\nScenario-specific intercepts:\n")
print(scenarios)

# ---------------------------------------------------------------------
# 2. Data-generating functions
# ---------------------------------------------------------------------
eta_fun <- function(age, group_num, beta_intercept, u = 0) {
  age_c <- age - settings$age_center
  beta_intercept +
    settings$beta_age * age_c +
    settings$beta_group * group_num +
    settings$beta_age_group * age_c * group_num + u
}

p_fun <- function(age, group_num, beta_intercept, u = 0) {
  age_c <- age - settings$age_center
  eta <- beta_intercept + settings$beta_age * age_c +
    settings$beta_group * group_num + settings$beta_age_group * age_c * group_num + u
  settings$chance + (1 - settings$chance) * stats::plogis(eta)
}

# ---------------------------------------------------------------------
# 3. Compact scenario table
# ---------------------------------------------------------------------
scenario_values <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      g <- expand.grid(
        age = settings$age_summary_values,
        group_num = c(0, 1)
      )
      g$scenario <- s$scenario
      g$group <- ifelse(g$group_num == 0, "Group 0", "Group 1")
      g$linear_predictor <- eta_fun(g$age, g$group_num, s$beta_intercept)
      g$expected_accuracy <- p_fun(g$age, g$group_num, s$beta_intercept)
      g$curve_condition <- "random intercept = 0"
      g$expected_correct_out_of_k_trials <- g$expected_accuracy *
      settings$k_trials

      g[, c(
          "scenario",
          "age",
          "group",
          "linear_predictor",
          "expected_accuracy",
          "expected_correct_out_of_k_trials",
          "curve_condition"
      )]
  })
)

scenario_contrasts <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      age_low <- settings$age_range[1]
      age_high <- settings$age_range[2]

      p00 <- p_fun(age_low, 0, s$beta_intercept)
      p01 <- p_fun(age_low, 1, s$beta_intercept)
      p10 <- p_fun(age_high, 0, s$beta_intercept)
      p11 <- p_fun(age_high, 1, s$beta_intercept)

      response_scale_values <- c(
        group_difference(p00, p01),
        group_difference(p10, p11),
        p10 - p00,
        p11 - p01,
        change_in_group_difference(p00, p01, p10, p11),
        NA_real_
      )

      data.frame(
        scenario = s$scenario,
        contrast = c(
          "Group difference at youngest age: Group 1 minus Group 0",
          "Group difference at oldest age: Group 1 minus Group 0",
          "Age-related change in Group 0: oldest minus youngest",
          "Age-related change in Group 1: oldest minus youngest",
          "Change in group difference from youngest to oldest age",
          "Generating link-scale age-by-group product term"
        ),
        value_probability_points = response_scale_values,
        value_correct_out_of_k_trials = response_scale_values * settings$k_trials,
        link_scale_value = c(
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          NA_real_,
          settings$beta_age_group
        ),
        stringsAsFactors = FALSE
      )
  })
)

scenario_table <- rbind(
  data.frame(
    table_part = "implied_values",
    scenario_values,
    contrast = NA_character_,
    value_probability_points = NA_real_,
    value_correct_out_of_k_trials = NA_real_,
    link_scale_value = NA_real_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    table_part = "derived_contrasts",
    scenario = scenario_contrasts$scenario,
    age = NA_real_,
    group = NA_character_,
    linear_predictor = NA_real_,
    expected_accuracy = NA_real_,
    expected_correct_out_of_k_trials = NA_real_,
    curve_condition = NA_character_,
    contrast = scenario_contrasts$contrast,
    value_probability_points = scenario_contrasts$value_probability_points,
    value_correct_out_of_k_trials = scenario_contrasts$value_correct_out_of_k_trials,
    link_scale_value = scenario_contrasts$link_scale_value,
    stringsAsFactors = FALSE
  )
)

utils::write.csv(
  scenario_table,
  settings$output_scenario_table,
  row.names = FALSE
)

cat("\n", "Implied scenario values", "\n")
print(scenario_values)
cat("\n", "Derived contrasts implied by each scenario", "\n")
print(scenario_contrasts)
cat("\nGroup gaps are Group 1 minus Group 0; changes are high minus low.\n")

# ---------------------------------------------------------------------
# 4. Deterministic plotting data
# ---------------------------------------------------------------------
plot_grid <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      g <- expand.grid(
        age = settings$age_plot_values,
        group_num = c(0, 1)
      )
      g$scenario <- s$scenario
      g$group <- factor(
        g$group_num,
        levels = c(0, 1),
        labels = c("Group 0", "Group 1")
      )
      g$age_c <- g$age - settings$age_center
      g$eta <- eta_fun(g$age, g$group_num, s$beta_intercept)
      g$expected_accuracy <- settings$chance + (1 - settings$chance) * stats::plogis(g$eta)
      g
  })
)
plot_grid$scenario <- factor(plot_grid$scenario, levels = scenarios$scenario)

gap_data <- do.call(
  rbind,
  lapply(split(plot_grid, plot_grid$scenario), function(dat) {
      wide0 <- dat[dat$group_num == 0, c("scenario", "age", "expected_accuracy")]
      wide1 <- dat[dat$group_num == 1, c("scenario", "age", "expected_accuracy")]
      gap <- group_difference(wide0$expected_accuracy, wide1$expected_accuracy)

      data.frame(
        scenario = wide0$scenario,
        age = wide0$age,
        group_gap = gap,
        group_difference_correct_out_of_k_trials = gap * settings$k_trials,
        stringsAsFactors = FALSE
      )
  })
)
gap_data$scenario <- factor(gap_data$scenario, levels = scenarios$scenario)

# ---------------------------------------------------------------------
# 5. One illustrative dataset per scenario
# ---------------------------------------------------------------------
example_data <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
      beta_intercept <- scenarios$beta_intercept[i]
      group_num <- stats::rbinom(settings$N, 1, 0.5)
      age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
      age_c <- age - settings$age_center
      id <- factor(rep(seq_len(settings$N), each = settings$k_trials))
      u <- stats::rnorm(settings$N, mean = 0, sd = settings$sigma_u)
      eta <- beta_intercept + settings$beta_age * age_c +
        settings$beta_group * group_num + settings$beta_age_group * age_c * group_num + u
      d <- data.frame(
        id = id,
        age = rep(age, each = settings$k_trials),
        age_c = rep(age_c, each = settings$k_trials),
        group_num = rep(group_num, each = settings$k_trials),
        u = rep(u, each = settings$k_trials),
        stringsAsFactors = FALSE
      )
      d$eta <- rep(eta, each = settings$k_trials)
      d$p <- settings$chance + (1 - settings$chance) * stats::plogis(d$eta)
      d$correct <- stats::rbinom(nrow(d), size = 1, prob = d$p)

      d$group <- factor(
          d$group_num,
          levels = c(0, 1),
          labels = c("Group 0", "Group 1")
      )
      d$scenario <- scenarios$scenario[i]
      d
  })
)
example_data$scenario <- factor(
  example_data$scenario,
  levels = scenarios$scenario
)

example_data$age_bin <- cut(
  example_data$age,
  breaks = settings$age_bin_breaks,
  include.lowest = TRUE
)
example_binned <- stats::aggregate(
  correct ~ scenario + age_bin + group,
  example_data,
  mean
)
example_binned$age_mid <- {
  bin_levels <- levels(example_binned$age_bin)
  bin_labels <- gsub("\\[|\\]|\\(|\\)", "", bin_levels)
  midpoints <- vapply(strsplit(bin_labels, ","), function(z) mean(as.numeric(z)), numeric(1))
  midpoints[as.integer(example_binned$age_bin)]
}

cat("\n", "One example dataset per scenario", "\n")
cat("Observed mean accuracy by scenario and group:\n")
print(stats::aggregate(correct ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 6. One replication: trial-level DGP and four mixed-model fits
# ---------------------------------------------------------------------
capture_fit <- function(expr) {
  warning_text <- character()
  error_text <- ""
  fit <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warning_text <<- c(warning_text, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
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
      interaction_coef = NA_real_, interaction_se = NA_real_, p_value = NA_real_,
      fit_problem = TRUE, problem_message = captured$error, singular = NA,
      warning_message = paste(captured$warnings, collapse = " | "),
      change_in_group_difference_response_scale = NA_real_,
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
    (predictions[4] - predictions[2]) - (predictions[3] - predictions[1])
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
    interaction_coef = unname(estimate),
    interaction_se = unname(standard_error),
    p_value = unname(p_value),
    fit_problem = fit_problem,
    problem_message = paste(unique(problem_parts), collapse = " | "),
    singular = singular,
    warning_message = paste(
      unique(c(captured$warnings, extraction_warnings)), collapse = " | "
    ),
    change_in_group_difference_response_scale = unname(did),
    stringsAsFactors = FALSE
  )
}

run_replication <- function(replication, scenario_label, beta_intercept) {
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  u <- stats::rnorm(settings$N, mean = 0, sd = settings$sigma_u)
  # The true product term is zero on the conditional chance-corrected-logit
  # scale, with the subject random intercept held fixed.
  eta <- beta_intercept + settings$beta_age * age_c +
    settings$beta_group * group_num + settings$beta_age_group * age_c * group_num + u

  d <- data.frame(
    id = factor(rep(seq_len(settings$N), each = settings$k_trials)),
    age = rep(age, each = settings$k_trials),
    age_c = rep(age_c, each = settings$k_trials),
    group_num = rep(group_num, each = settings$k_trials),
    group = factor(
      rep(group_num, each = settings$k_trials),
      levels = c(0, 1),
      labels = c("Group 0", "Group 1")
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
  fit_chance <- capture_fit(lme4::glmer(
    correct ~ age_c * group + (1 | id),
    family = stats::binomial(psyphy::mafc.logit(2)), data = d
  ))

  nd <- expand.grid(age = settings$age_range,
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")))
  nd$age_c <- nd$age - settings$age_center
  extracted <- rbind(
    extract_mixed_fit(fit_gaussian, nd, gaussian = TRUE),
    extract_mixed_fit(fit_logit, nd),
    extract_mixed_fit(fit_probit, nd),
    extract_mixed_fit(fit_chance, nd)
  )
  extracted$scenario <- scenario_label
  extracted$replication <- replication
  extracted$model <- model_names
  extracted$change_in_group_difference_outcome_units <-
    extracted$change_in_group_difference_response_scale * settings$k_trials
  extracted[, c(
    "scenario", "replication", "model", "interaction_coef", "interaction_se",
    "p_value", "fit_problem", "problem_message", "singular", "warning_message",
    "change_in_group_difference_response_scale",
    "change_in_group_difference_outcome_units"
  )]
}

# ---------------------------------------------------------------------
# 7. Repeated simulation
# ---------------------------------------------------------------------
cat("\n", "Monte Carlo simulation", "\n")
cat(
  "Running B = ",
  settings$B,
  " replications per scenario (n_cores = ",
  settings$n_cores,
  ").\n",
  sep = ""
)
cat(
  "This can be changed with Sys.setenv(N_SIM = '...') or Sys.setenv(N_CORES = '...').\n"
)

# N_CORES overrides the SLURM allocation; otherwise use SLURM_CPUS_PER_TASK.
# Parallelize replications only. Each worker uses one BLAS/OpenMP thread.
cluster <- NULL
if (settings$n_cores > 1 && .Platform$OS.type != "unix") {
  cluster <- parallel::makeCluster(settings$n_cores)
  parallel::clusterSetRNGStream(cluster, iseed = 20260525)
  parallel::clusterExport(
    cluster,
    c("settings", "model_names", "capture_fit", "extract_mixed_fit")
  )
}
scenario_results <- list()
for (i in seq_len(nrow(scenarios))) {
  s <- scenarios[i, ]
  if (settings$n_cores > 1 && .Platform$OS.type == "unix") {
    replications <- parallel::mclapply(seq_len(settings$B), run_replication,
      scenario_label = s$scenario, beta_intercept = s$beta_intercept, mc.cores = settings$n_cores, mc.set.seed = TRUE)
  } else if (!is.null(cluster)) {
    replications <- parallel::parLapply(cluster, seq_len(settings$B), run_replication,
      scenario_label = s$scenario, beta_intercept = s$beta_intercept)
  } else {
    replications <- lapply(seq_len(settings$B), run_replication, scenario_label = s$scenario, beta_intercept = s$beta_intercept)
  }
  scenario_results[[length(scenario_results) + 1L]] <- do.call(rbind, replications)
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
simulation_results <- do.call(rbind, scenario_results)

wilson_interval <- function(rejections, n) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(0.975)
  rate <- rejections / n
  denominator <- 1 + z^2 / n
  center <- (rate + z^2 / (2 * n)) / denominator
  half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / denominator
  c(center - half, center + half)
}

simulation_summary <- do.call(rbind, lapply(
  split(
    simulation_results,
    list(simulation_results$scenario, simulation_results$model),
    drop = TRUE
  ),
  function(dat) {
    finite_ok <- is.finite(dat$p_value) & !dat$fit_problem
    finite_problem <- is.finite(dat$p_value) & dat$fit_problem
    n_ok <- sum(finite_ok)
    n_problem <- sum(finite_problem)
    reject_ok <- sum(dat$p_value[finite_ok] < settings$alpha)
    reject_problem <- sum(dat$p_value[finite_problem] < settings$alpha)
    ci_ok <- wilson_interval(reject_ok, n_ok)
    ci_problem <- wilson_interval(reject_problem, n_problem)
    finite_coef <- dat$interaction_coef[is.finite(dat$interaction_coef)]
    finite_did <- dat$change_in_group_difference_outcome_units[
      is.finite(dat$change_in_group_difference_outcome_units)
    ]

    data.frame(
      scenario = dat$scenario[1],
      model = dat$model[1],
      n_total = nrow(dat),
      n_fit_ok = sum(!dat$fit_problem),
      n_fit_problem = sum(dat$fit_problem),
      fit_problem_rate = mean(dat$fit_problem),
      n_p_finite_ok = n_ok,
      n_rejections_ok = reject_ok,
      rejection_rate_ok = if (n_ok > 0) reject_ok / n_ok else NA_real_,
      ci_low_ok = ci_ok[1],
      ci_high_ok = ci_ok[2],
      n_p_finite_problem = n_problem,
      n_rejections_problem = reject_problem,
      rejection_rate_problem = if (n_problem > 0) reject_problem / n_problem else NA_real_,
      ci_low_problem = ci_problem[1],
      ci_high_problem = ci_problem[2],
      median_interaction_coef = if (length(finite_coef)) stats::median(finite_coef) else NA_real_,
      mean_interaction_coef = if (length(finite_coef)) mean(finite_coef) else NA_real_,
      sd_interaction_coef = if (length(finite_coef) > 1) stats::sd(finite_coef) else NA_real_,
      median_change_in_group_difference_outcome_units = if (length(finite_did)) stats::median(finite_did) else NA_real_,
      mean_change_in_group_difference_outcome_units = if (length(finite_did)) mean(finite_did) else NA_real_,
      sd_change_in_group_difference_outcome_units = if (length(finite_did) > 1) stats::sd(finite_did) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
))
simulation_summary$scenario <- factor(
  simulation_summary$scenario,
  levels = scenarios$scenario
)
simulation_summary$model <- factor(
  simulation_summary$model,
  levels = model_names
)
simulation_summary$rate_type <- ifelse(
  as.character(simulation_summary$model) == "Chance-corrected binomial logit",
  "Nominal rejection rate",
  "Pseudo-interaction detection rate"
)
simulation_summary <- simulation_summary[
  order(simulation_summary$scenario, simulation_summary$model),
]

utils::write.csv(
  simulation_summary,
  settings$output_summary_table,
  row.names = FALSE
)

cat("\n", "Simulation summary", "\n")
print(simulation_summary)
cat(
  "\nThe generating age-by-group product term is zero on the conditional chance-corrected logit scale.\n"
)
cat("rejection_rate_ok is primary and uses finite p-values from fits passing the minimal lme4 checks.\n")
cat("Finite p-values from flagged fits are retained in rejection_rate_problem.\n")
cat(
  "Models fitted on alternative scales supply pseudo-interaction detection rates.\n"
)
cat(
  "\nInterpretation aid: median_change_in_group_difference_outcome_units is NOT an observed count.\n"
)
cat(
  "It is the median model-implied change in the predicted group difference from age ",
  settings$age_range[1],
  " to age ",
  settings$age_range[2],
  ",\n",
  sep = ""
)
cat(
  "expressed as correct-response units out of ",
  settings$k_trials,
  " trials.\n",
  sep = ""
)

# ---------------------------------------------------------------------
# 8. Figure panels
# ---------------------------------------------------------------------
floor_band <- data.frame(
  scenario = factor(scenarios$scenario, levels = scenarios$scenario),
  xmin = settings$age_range[1],
  xmax = settings$age_range[2],
  ymin = 0,
  ymax = settings$chance
)

chance_annotation <- data.frame(
  scenario = factor(scenarios$scenario, levels = scenarios$scenario),
  age = settings$age_range[2] - 0.05,
  expected_accuracy = settings$chance + 0.025,
  label = paste0("chance floor = ", sprintf("%.2f", settings$chance))
)

eta_grid <- seq(-5, 5, by = 1)

chance_link_grid <- data.frame(
  yintercept = settings$chance + (1 - settings$chance) * stats::plogis(eta_grid)
)

pA <- ggplot2::ggplot(
  plot_grid,
  ggplot2::aes(age, expected_accuracy, linetype = group, color = group)
) +
  ggplot2::geom_rect(
  data = floor_band,
  ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  inherit.aes = FALSE,
  fill = "grey92",
  color = NA
) +
  ggplot2::geom_hline(
  data = chance_link_grid,
  ggplot2::aes(yintercept = yintercept),
  inherit.aes = FALSE,
  color = "grey82",
  linewidth = 0.35
) +
  ggplot2::geom_hline(
  yintercept = settings$chance,
  linetype = "dashed",
  color = "grey35"
) +
  ggplot2::geom_line(linewidth = 0.95) +
  ggplot2::geom_text(
  data = chance_annotation,
  ggplot2::aes(age, expected_accuracy, label = label),
  inherit.aes = FALSE,
  hjust = 1,
  size = 3,
  color = "grey25"
) +
  ggplot2::facet_wrap(~scenario) +
  ggplot2::coord_cartesian(ylim = c(settings$chance - 0.05, 1.00)) +
  ggplot2::scale_y_continuous(
  labels = function(x) paste0(round(100 * x / 1) * 1, "%"),
  breaks = seq(settings$chance, 1, by = 0.10)
) +
  ggplot2::scale_color_manual(values = c(
    "Group 0" = "#0072B2",
    "Group 1" = "#D55E00",
    "Identity" = "#009E73",
    "Gaussian identity" = "#009E73",
    "Standard logit" = "#0072B2",
    "Standard probit" = "#CC79A7",
    "Standard binomial logit" = "#0072B2",
    "Chance-corrected logit" = "#D55E00",
    "Chance-corrected binomial" = "#D55E00",
    "Chance-corrected binomial link" = "#D55E00",
    "Observed data" = "grey30",
    "Generating model" = "black"
  ), name = NULL) +
  ggplot2::scale_linetype_manual(values = c(
    "Group 0" = "solid",
    "Group 1" = "longdash",
    "Identity" = "solid",
    "Gaussian identity" = "solid",
    "Standard logit" = "solid",
    "Standard probit" = "dotdash",
    "Standard binomial logit" = "solid",
    "Chance-corrected logit" = "longdash",
    "Chance-corrected binomial" = "longdash",
    "Chance-corrected binomial link" = "longdash",
    "Observed data" = "blank",
    "Generating model" = "solid"
  ), name = NULL) +
  ggplot2::labs(
  title = "A. Scenario curves generated above a chance floor",
  subtitle = "Curves are conditional at random intercept = 0; horizontal lines mark equal link-scale steps",
  x = "Age",
  y = "Expected accuracy",
  color = NULL,
  linetype = NULL
) +
  (ggplot2::theme_minimal(base_size = (10), base_family = ("")) +
    ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = (10) + 1,
      margin = ggplot2::margin(b = 3)
    ),
    plot.subtitle = ggplot2::element_text(
      size = (10) - 1,
      color = "grey25",
      margin = ggplot2::margin(b = 6)
    ),
    axis.title = ggplot2::element_text(size = (10)),
    axis.text = ggplot2::element_text(size = (10) - 1, color = "grey20"),
    strip.text = ggplot2::element_text(face = "bold", size = (10) - 1),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = (10) - 1),
    legend.text = ggplot2::element_text(size = (10) - 1),
    legend.key.width = grid::unit(1.25, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey88"),
    panel.spacing = grid::unit(0.9, "lines"),
    plot.margin = ggplot2::margin(6, 8, 6, 8)
)) +
  ggplot2::theme(
  panel.grid.major.y = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank()
)

pB <- ggplot2::ggplot(
  simulation_summary,
  ggplot2::aes(x = model, y = rejection_rate_ok, shape = model)
) +
  ggplot2::geom_hline(
  yintercept = settings$alpha,
  linetype = "dashed",
  color = "grey35"
) +
  ggplot2::geom_pointrange(
  ggplot2::aes(ymin = ci_low_ok, ymax = ci_high_ok),
  linewidth = 0.45
) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~scenario) +
  ggplot2::scale_y_continuous(
  labels = function(x) paste0(round(100 * x / 1) * 1, "%"),
  breaks = seq(0, 1, by = 0.25),
  limits = c(0, 1)
) +
  ggplot2::scale_shape_manual(
  values = c(
    "Gaussian identity" = 16,
    "Standard binomial logit" = 15,
    "Standard binomial probit" = 18,
    "Chance-corrected binomial logit" = 17
  )
) +
  ggplot2::labs(
  title = "B. Product-term rejection rate",
  subtitle = "Primary rates use fits passing minimal lme4 checks; dashed line: nominal alpha",
  x = NULL,
  y = "Replications rejecting the age-by-group product term",
  shape = NULL
) +
  (ggplot2::theme_minimal(base_size = (9), base_family = ("")) +
    ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = (9) + 1,
      margin = ggplot2::margin(b = 3)
    ),
    plot.subtitle = ggplot2::element_text(
      size = (9) - 1,
      color = "grey25",
      margin = ggplot2::margin(b = 6)
    ),
    axis.title = ggplot2::element_text(size = (9)),
    axis.text = ggplot2::element_text(size = (9) - 1, color = "grey20"),
    strip.text = ggplot2::element_text(face = "bold", size = (9) - 1),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = (9) - 1),
    legend.text = ggplot2::element_text(size = (9) - 1),
    legend.key.width = grid::unit(1.25, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey88"),
    panel.spacing = grid::unit(0.9, "lines"),
    plot.margin = ggplot2::margin(6, 8, 6, 8)
)) +
  ggplot2::theme(legend.position = "none")

# Write the same panel layout to PDF and PNG.
plots <- list(pA, pB)
plot_columns <- 1
plot_rows <- ceiling(length(plots) / plot_columns)
dir.create(dirname(settings$output_figure_base), recursive = TRUE, showWarnings = FALSE)
for (plot_format in c("pdf", "png")) {
  if (plot_format == "pdf") {
    grDevices::pdf(paste0(settings$output_figure_base, ".pdf"), width = figure_width, height = 5.9)
  } else {
    grDevices::png(paste0(settings$output_figure_base, ".png"), width = figure_width, height = 5.9, units = "in", res = default_dpi)
  }
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(plot_rows, plot_columns)))
  for (panel in seq_along(plots)) {
    plot_row <- ceiling(panel / plot_columns)
    plot_column <- panel - (plot_row - 1) * plot_columns
    print(plots[[panel]], vp = grid::viewport(layout.pos.row = plot_row, layout.pos.col = plot_column))
  }
  grid::popViewport()
  grDevices::dev.off()
}

p_effect <- ggplot2::ggplot(
  simulation_summary,
  ggplot2::aes(
    x = model,
    y = median_change_in_group_difference_outcome_units,
    shape = model
  )
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~scenario) +
  ggplot2::scale_shape_manual(
  values = c(
    "Gaussian identity" = 16,
    "Standard binomial logit" = 15,
    "Standard binomial probit" = 18,
    "Chance-corrected binomial logit" = 17
  )
) +
  ggplot2::labs(
  title = "Inspection: median model-implied change in the group difference",
  subtitle = "Values are contrasts, not possible observed counts",
  x = NULL,
  y = paste0("Change in group gap from ", (settings$age_range[1]), " to ", (settings$age_range[2]), ", correct responses out of ", (settings$k_trials)),
  shape = NULL
) +
  (ggplot2::theme_minimal(base_size = (10), base_family = ("")) +
    ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = (10) + 1,
      margin = ggplot2::margin(b = 3)
    ),
    plot.subtitle = ggplot2::element_text(
      size = (10) - 1,
      color = "grey25",
      margin = ggplot2::margin(b = 6)
    ),
    axis.title = ggplot2::element_text(size = (10)),
    axis.text = ggplot2::element_text(size = (10) - 1, color = "grey20"),
    strip.text = ggplot2::element_text(face = "bold", size = (10) - 1),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = (10) - 1),
    legend.text = ggplot2::element_text(size = (10) - 1),
    legend.key.width = grid::unit(1.25, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "grey88"),
    panel.spacing = grid::unit(0.9, "lines"),
    plot.margin = ggplot2::margin(6, 8, 6, 8)
)) +
  ggplot2::theme(legend.position = "none")

dir.create(dirname(settings$output_inspection_base), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(paste0(settings$output_inspection_base, ".pdf"), plot = p_effect, width = 8, height = 4.5)
ggplot2::ggsave(paste0(settings$output_inspection_base, ".png"), plot = p_effect, width = 8, height = 4.5, dpi = default_dpi)

saveRDS(
  list(
    settings = settings,
    scenarios = scenarios,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    example_dataset = example_data,
    example_binned = example_binned,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = settings$output_rds
)

cat("\n", "Saved files", "\n")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_summary_table, "\n", sep = "")
cat("- ", settings$output_figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_inspection_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
