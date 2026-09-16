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
  generating_link = "logit", # Describes the explicit DGP below.
  B = B,
  n_cores = as.integer(Sys.getenv(
      "N_CORES",
      Sys.getenv("SLURM_CPUS_PER_TASK", max(1, parallel::detectCores() - 1))
  )),
  alpha = default_alpha,
  output_scenario_table = "tables/scenario-table-forced-choice.csv",
  output_summary_table = "tables/simulation-summary-forced-choice.csv",
  output_common_convergence_table = "tables/simulation-summary-forced-choice-common-convergence.csv",
  output_figure_base = "figs/forced-choice-simulation",
  output_inspection_base = "outputs/inspection/forced-choice-effect-size-inspection",
  output_rds = "outputs/simulation-forced-choice.rds"
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
eta_fun <- function(age, group_num, beta_intercept) {
  age_c <- age - settings$age_center
  beta_intercept +
    settings$beta_age * age_c +
    settings$beta_group * group_num +
    settings$beta_age_group * age_c * group_num
}

p_fun <- function(age, group_num, beta_intercept) {
  age_c <- age - settings$age_center
  eta <- beta_intercept + settings$beta_age * age_c +
    settings$beta_group * group_num + settings$beta_age_group * age_c * group_num
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
      g$expected_correct_out_of_k_trials <- g$expected_accuracy *
      settings$k_trials

      g[, c(
          "scenario",
          "age",
          "group",
          "linear_predictor",
          "expected_accuracy",
          "expected_correct_out_of_k_trials"
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
      eta <- beta_intercept + settings$beta_age * age_c +
        settings$beta_group * group_num + settings$beta_age_group * age_c * group_num
      p <- settings$chance + (1 - settings$chance) * stats::plogis(eta)
      y <- stats::rbinom(settings$N, size = settings$k_trials, prob = p)

      d <- data.frame(
        age = age,
        age_c = age_c,
        group_num = group_num,
        group = factor(
          group_num,
          levels = c(0, 1),
          labels = c("Group 0", "Group 1")
        ),
        y = y,
        k = settings$k_trials,
        accuracy = y / settings$k_trials,
        stringsAsFactors = FALSE
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
  accuracy ~ scenario + age_bin + group,
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
print(stats::aggregate(accuracy ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 6. One replication: DGP, explicit fits, likelihood, interaction tests
# ---------------------------------------------------------------------
run_replication <- function(replication, scenario_label, beta_intercept) {
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  eta <- beta_intercept + settings$beta_age * age_c +
    settings$beta_group * group_num + settings$beta_age_group * age_c * group_num
  p <- settings$chance + (1 - settings$chance) * stats::plogis(eta)
  y <- stats::rbinom(settings$N, size = settings$k_trials, prob = p)

  d <- data.frame(
    age = age,
    age_c = age_c,
    group_num = group_num,
    group = factor(
      group_num,
      levels = c(0, 1),
      labels = c("Group 0", "Group 1")
    ),
    y = y,
    k = settings$k_trials,
    accuracy = y / settings$k_trials,
    stringsAsFactors = FALSE
  )

  fit_gaussian <- try(stats::lm(accuracy ~ age_c * group, data = d), silent = TRUE)
  fit_logit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
      family = stats::binomial("logit"), data = d), silent = TRUE)
  fit_probit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
      family = stats::binomial("probit"), data = d), silent = TRUE)

  # Chance-corrected binomial logit: likelihood, three starts, and Wald test.
  X <- stats::model.matrix(~ age_c * group, data = d)
  y <- d$y
  k <- d$k
  chance <- settings$chance
  nll <- function(beta) {
    eta <- drop(X %*% beta)
    p <- chance + (1 - chance) * stats::plogis(eta)
    p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }
  gradient <- function(beta) {
    eta <- drop(X %*% beta)
    q <- stats::plogis(eta)
    p <- chance + (1 - chance) * q
    p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
    weight <- ((y - k * p) / (p * (1 - p))) * (1 - chance) * q * (1 - q)
    -drop(crossprod(X, weight))
  }
  zero <- stats::setNames(rep(0, ncol(X)), colnames(X))
  from_standard <- zero
  start_fit <- try(stats::glm.fit(X, cbind(y, k - y),
      family = stats::binomial("logit")), silent = TRUE)
  if (!inherits(start_fit, "try-error") && all(is.finite(stats::coef(start_fit)))) {
    from_standard[] <- stats::coef(start_fit)
  }
  above <- (y / k - chance) / (1 - chance)
  above <- pmin(pmax(above, 0.02), 0.98)
  from_above <- zero
  above_fit <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
  if (!inherits(above_fit, "try-error") && all(is.finite(stats::coef(above_fit)))) {
    from_above[] <- stats::coef(above_fit)
  }
  candidates <- list()
  for (start in list(zero, from_standard, from_above)) {
    candidate <- try(stats::optim(start, nll, gr = gradient, method = "BFGS",
        control = list(maxit = 1500, reltol = 1e-10)),
      silent = TRUE)
    if (!inherits(candidate, "try-error") && is.finite(candidate$value) &&
        all(is.finite(candidate$par))) {
      candidates[[length(candidates) + 1L]] <- candidate
    }
  }
  chance_coef <- chance_se <- chance_p <- rep(NA_real_, ncol(X))
  chance_vcov <- matrix(NA_real_, ncol(X), ncol(X))
  chance_usable <- FALSE
  chance_nll <- NA_real_
  if (length(candidates)) {
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "value"))]]
    chance_coef <- best$par
    chance_nll <- best$value
    hessian <- try(stats::optimHess(best$par, nll, gr = gradient), silent = TRUE)
    well_conditioned <- FALSE
    if (!inherits(hessian, "try-error") && all(is.finite(hessian))) {
      hess_sym <- (hessian + t(hessian)) / 2
      eig <- try(eigen(hess_sym, symmetric = TRUE, only.values = TRUE)$values,
        silent = TRUE)
      if (!inherits(eig, "try-error")) {
        well_conditioned <- all(is.finite(eig)) && min(eig) > 1e-7 &&
          min(eig) / max(eig) > sqrt(.Machine$double.eps)
      }
      if (well_conditioned) {
        V <- try(solve(hess_sym), silent = TRUE)
        if (!inherits(V, "try-error") && all(is.finite(V))) chance_vcov <- V
      }
    }
    variances <- diag(chance_vcov)
    variances[!is.finite(variances) | variances <= 0] <- NA_real_
    chance_se <- sqrt(variances)
    z <- chance_coef / chance_se
    chance_p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    chance_usable <- isTRUE(best$convergence == 0) && well_conditioned &&
      all(is.finite(chance_vcov)) && all(is.finite(chance_se)) && all(is.finite(chance_p))
    if (!chance_usable) chance_p[] <- NA_real_
  }
  names(chance_coef) <- names(chance_se) <- names(chance_p) <- colnames(X)

  nd <- expand.grid(age = settings$age_range,
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")))
  nd$age_c <- nd$age - settings$age_center
  p_values <- coefficients <- did <- rep(NA_real_, 4)
  if (!inherits(fit_gaussian, "try-error")) {
    sm <- summary(fit_gaussian)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[1] <- sm["age_c:groupGroup 1", 4]
      coefficients[1] <- sm["age_c:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_gaussian, newdata = nd, type = "response")
    did[1] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (!inherits(fit_logit, "try-error")) {
    sm <- summary(fit_logit)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[2] <- sm["age_c:groupGroup 1", 4]
      coefficients[2] <- sm["age_c:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_logit, newdata = nd, type = "response")
    did[2] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (!inherits(fit_probit, "try-error")) {
    sm <- summary(fit_probit)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[3] <- sm["age_c:groupGroup 1", 4]
      coefficients[3] <- sm["age_c:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_probit, newdata = nd, type = "response")
    did[3] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  p_values[4] <- chance_p["age_c:groupGroup 1"]
  coefficients[4] <- chance_coef["age_c:groupGroup 1"]
  pred <- chance + (1 - chance) * stats::plogis(drop(stats::model.matrix(~ age_c * group, nd) %*% chance_coef))
  did[4] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  data.frame(scenario = scenario_label, replication = replication, model = model_names,
    p_value = p_values, interaction_coef = coefficients,
    change_in_group_difference_response_scale = did,
    change_in_group_difference_outcome_units = did * settings$k_trials,
    stringsAsFactors = FALSE)
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
  parallel::clusterExport(cluster, c("settings", "model_names"))
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

simulation_summary <- do.call(
  rbind,
  lapply(
    split(
      simulation_results,
      list(simulation_results$scenario, simulation_results$model),
      drop = TRUE
    ),
    function(dat) {
      ok <- is.finite(dat$p_value)
      n <- sum(ok)
      sig <- sum(dat$p_value[ok] < settings$alpha)
      z <- stats::qnorm(0.975)
      rate <- if (n == 0) NA_real_ else sig / n
      denom <- 1 + z^2 / n
      center <- (rate + z^2 / (2 * n)) / denom
      half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / denom
      ci <- if (n == 0) c(NA_real_, NA_real_) else c(center - half, center + half)

      sm <- data.frame(
        n_successful_fits = n,
        n_rejections = sig,
        rejection_rate = rate,
        ci_low = ci[1],
        ci_high = ci[2],
        median_interaction_coef = if (all(!is.finite(dat$interaction_coef))) NA_real_ else stats::median(dat$interaction_coef, na.rm = TRUE),
        mean_interaction_coef = if (any(is.finite(dat$interaction_coef))) mean((dat$interaction_coef)[is.finite(dat$interaction_coef)]) else NA_real_,
        sd_interaction_coef = stats::sd((dat$interaction_coef)[is.finite(dat$interaction_coef)]),
        q25_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.25, na.rm = TRUE)),
        q75_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.75, na.rm = TRUE)),
        q025_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.025, na.rm = TRUE)),
        q975_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.975, na.rm = TRUE)),
        median_abs_coef_significant = if (all(!is.finite(abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value < settings$alpha])))) NA_real_ else stats::median(
          abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value < settings$alpha])
          , na.rm = TRUE),
        median_abs_coef_nonsignificant = if (all(!is.finite(abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value >= settings$alpha])))) NA_real_ else stats::median(
          abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value >= settings$alpha])
          , na.rm = TRUE),
        median_change_in_group_difference_response_scale = if (all(!is.finite(dat$change_in_group_difference_response_scale))) NA_real_ else stats::median(dat$change_in_group_difference_response_scale, na.rm = TRUE),
        median_change_in_group_difference_outcome_units = if (all(!is.finite(dat$change_in_group_difference_outcome_units))) NA_real_ else stats::median(dat$change_in_group_difference_outcome_units, na.rm = TRUE),
        mean_change_in_group_difference_outcome_units = if (any(is.finite(dat$change_in_group_difference_outcome_units))) mean((dat$change_in_group_difference_outcome_units)[is.finite(dat$change_in_group_difference_outcome_units)]) else NA_real_,
        sd_change_in_group_difference_outcome_units = stats::sd((dat$change_in_group_difference_outcome_units)[is.finite(dat$change_in_group_difference_outcome_units)]),
        stringsAsFactors = FALSE
      )
      data.frame(
        scenario = dat$scenario[1],
        model = dat$model[1],
        sm,
        stringsAsFactors = FALSE
      )
    }
  )
)
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
  "\nThe generating age-by-group product term is zero on the chance-corrected logit scale.\n"
)
cat("The matching chance-corrected model supplies nominal rejection rates.\n")
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
# 7b. Common-convergence subset
# ---------------------------------------------------------------------
common_convergence_results <- do.call(
  rbind,
  lapply(
    split(simulation_results, simulation_results$scenario, drop = TRUE),
    function(dat) {
      usable <- stats::aggregate(
        list(all_models_converged = is.finite(dat$p_value)),
        by = list(replication = dat$replication),
        FUN = all
      )
      keep <- usable$replication[usable$all_models_converged]
      dat[dat$replication %in% keep, , drop = FALSE]
    }
  )
)

common_convergence_summary <- do.call(
  rbind,
  lapply(
    split(
      common_convergence_results,
      list(
        common_convergence_results$scenario,
        common_convergence_results$model
      ),
      drop = TRUE
    ),
    function(dat) {
      ok <- is.finite(dat$p_value)
      n <- sum(ok)
      sig <- sum(dat$p_value[ok] < settings$alpha)
      z <- stats::qnorm(0.975)
      rate <- if (n == 0) NA_real_ else sig / n
      denom <- 1 + z^2 / n
      center <- (rate + z^2 / (2 * n)) / denom
      half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / denom
      ci <- if (n == 0) c(NA_real_, NA_real_) else c(center - half, center + half)

      sm <- data.frame(
        n_successful_fits = n,
        n_rejections = sig,
        rejection_rate = rate,
        ci_low = ci[1],
        ci_high = ci[2],
        median_interaction_coef = if (all(!is.finite(dat$interaction_coef))) NA_real_ else stats::median(dat$interaction_coef, na.rm = TRUE),
        mean_interaction_coef = if (any(is.finite(dat$interaction_coef))) mean((dat$interaction_coef)[is.finite(dat$interaction_coef)]) else NA_real_,
        sd_interaction_coef = stats::sd((dat$interaction_coef)[is.finite(dat$interaction_coef)]),
        q25_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.25, na.rm = TRUE)),
        q75_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.75, na.rm = TRUE)),
        q025_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.025, na.rm = TRUE)),
        q975_interaction_coef = unname(stats::quantile((dat$interaction_coef)[is.finite(dat$interaction_coef)], 0.975, na.rm = TRUE)),
        median_abs_coef_significant = if (all(!is.finite(abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value < settings$alpha])))) NA_real_ else stats::median(
          abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value < settings$alpha])
          , na.rm = TRUE),
        median_abs_coef_nonsignificant = if (all(!is.finite(abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value >= settings$alpha])))) NA_real_ else stats::median(
          abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value >= settings$alpha])
          , na.rm = TRUE),
        median_change_in_group_difference_response_scale = if (all(!is.finite(dat$change_in_group_difference_response_scale))) NA_real_ else stats::median(dat$change_in_group_difference_response_scale, na.rm = TRUE),
        median_change_in_group_difference_outcome_units = if (all(!is.finite(dat$change_in_group_difference_outcome_units))) NA_real_ else stats::median(dat$change_in_group_difference_outcome_units, na.rm = TRUE),
        mean_change_in_group_difference_outcome_units = if (any(is.finite(dat$change_in_group_difference_outcome_units))) mean((dat$change_in_group_difference_outcome_units)[is.finite(dat$change_in_group_difference_outcome_units)]) else NA_real_,
        sd_change_in_group_difference_outcome_units = stats::sd((dat$change_in_group_difference_outcome_units)[is.finite(dat$change_in_group_difference_outcome_units)]),
        stringsAsFactors = FALSE
      )
      data.frame(
        scenario = dat$scenario[1],
        model = dat$model[1],
        n_common_convergence_replications = length(unique(dat$replication)),
        sm,
        stringsAsFactors = FALSE
      )
    }
  )
)
common_convergence_summary$scenario <- factor(
  common_convergence_summary$scenario,
  levels = scenarios$scenario
)
common_convergence_summary$model <- factor(
  common_convergence_summary$model,
  levels = model_names
)
common_convergence_summary$rate_type <- ifelse(
  as.character(common_convergence_summary$model) ==
  "Chance-corrected binomial logit",
  "Nominal rejection rate",
  "Pseudo-interaction detection rate"
)
common_convergence_summary <- common_convergence_summary[
  order(common_convergence_summary$scenario, common_convergence_summary$model),
]

utils::write.csv(
  common_convergence_summary,
  settings$output_common_convergence_table,
  row.names = FALSE
)

cat("\n", "Simulation summary, common-convergence subset", "\n")
print(common_convergence_summary)
cat(
  "\nEvery model is restricted to the replications in which all four models converged,\n"
)
cat("so the four rates within a scenario are computed on identical datasets.\n")

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
  subtitle = "Horizontal lines mark equal steps on the chance-corrected logit scale",
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
  ggplot2::aes(x = model, y = rejection_rate, shape = model)
) +
  ggplot2::geom_hline(
  yintercept = settings$alpha,
  linetype = "dashed",
  color = "grey35"
) +
  ggplot2::geom_pointrange(
  ggplot2::aes(ymin = ci_low, ymax = ci_high),
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
  subtitle = "Matched generating-scale model: nominal rejection; alternative scales: pseudo-interaction detection. Dashed line: nominal alpha",
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
    simulation_summary = simulation_summary,
    common_convergence_summary = common_convergence_summary
  ),
  file = settings$output_rds
)

cat("\n", "Saved files", "\n")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_summary_table, "\n", sep = "")
cat("- ", settings$output_common_convergence_table, "\n", sep = "")
cat("- ", settings$output_figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_inspection_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
