# scripts/03c-simulation-sum-scores.R
# Simulation 3: sum scores as bounded and discrete outcomes.
#
# Scientific choices for this simulation live in this script.
# Computational settings and scientific calculations are local to this script.

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
rm(list = ls())
library(ggplot2)

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

set.seed(20260526)

cat("\n", "Simulation 3: sum scores", "\n")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
settings <- list(
  N = 600,
  J = 9,
  item_max = 3,
  theta_sd = 1.00,
  beta_x = 0.85,
  beta_group = -0.90,
  beta_x_group = 0.00,
  x_summary_values = c(-1, 0, 1),
  x_plot_range = c(-2.5, 2.5),
  x_bin_breaks = seq(-2.5, 2.5, length.out = 11),
  B = B,
  n_cores = as.integer(Sys.getenv(
      "N_CORES",
      Sys.getenv("SLURM_CPUS_PER_TASK", max(1, parallel::detectCores() - 1))
  )),
  alpha = default_alpha,
  scenario_table_path = "tables/scenario-table-sum-scores.csv",
  simulation_summary_path = "tables/simulation-summary-sum-scores.csv",
  figure_base = "figs/sum-score-simulation",
  inspection_effect_base = "outputs/inspection/sum-score-effect-size-inspection",
  inspection_thresholds_base = "outputs/inspection/sum-score-thresholds-inspection",
  rds_path = "outputs/simulation-sum-scores.rds"
)
settings$max_score <- settings$J * settings$item_max

scenarios <- data.frame(
  scenario = c("Lower range", "Middle range", "Upper range"),
  threshold_shift = c(1.60, 0.00, -1.60),
  interpretation = c(
    "Expected sum scores occupy the lower range of the observed scale.",
    "Expected sum scores occupy the middle range of the observed scale.",
    "Expected sum scores occupy the upper range of the observed scale."
  ),
  stringsAsFactors = FALSE
)

cat("\n", "Scenario parameters", "\n")
print(settings)
cat("\nScenario-specific threshold shifts:\n")
print(scenarios)

x_low <- min(settings$x_summary_values)
x_high <- max(settings$x_summary_values)

# Deterministic item thresholds: no hidden random jitter.
make_thresholds <- function(threshold_shift) {
  item_offsets <- seq(-0.45, 0.45, length.out = settings$J)
  base <- matrix(rep(c(-1, 0, 1), each = settings$J), nrow = settings$J)
  base + item_offsets + threshold_shift
}

latent_mean <- function(x, group_num) {
  settings$beta_x * x +
    settings$beta_group * group_num +
    settings$beta_x_group * x * group_num
}

# Expected sum score after integrating over individual latent residuals.
expected_sum_given_mu <- function(mu, thresholds) {
  q <- stats::qnorm((seq_len(201) - 0.5) / 201) * settings$theta_sd
  mean(vapply(mu + q, function(th) sum(stats::plogis(th - as.vector(thresholds))), numeric(1)))
}

# ---------------------------------------------------------------------
# 2. Compact scenario table
# ---------------------------------------------------------------------
scenario_values <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      th <- make_thresholds(s$threshold_shift)

      g <- expand.grid(
        x = settings$x_summary_values,
        group_num = c(0, 1)
      )

      g$scenario <- s$scenario
      g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
      g$latent_mean <- latent_mean(g$x, g$group_num)
      g$expected_sum_score <- vapply(g$latent_mean, expected_sum_given_mu, numeric(1), thresholds = th)
      g$expected_percent_of_max <- g$expected_sum_score / settings$max_score

      g[, c("scenario", "x", "group", "latent_mean", "expected_sum_score", "expected_percent_of_max")]
}))

scenario_contrasts <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      th <- make_thresholds(s$threshold_shift)

      e00 <- expected_sum_given_mu(latent_mean(x_low, 0), th)
      e01 <- expected_sum_given_mu(latent_mean(x_low, 1), th)
      e10 <- expected_sum_given_mu(latent_mean(x_high, 0), th)
      e11 <- expected_sum_given_mu(latent_mean(x_high, 1), th)

      data.frame(
        scenario = s$scenario,
        contrast = c(
          "Group difference at low x: Group 1 minus Group 0",
          "Group difference at high x: Group 1 minus Group 0",
          "x-related change in Group 0: high x minus low x",
          "x-related change in Group 1: high x minus low x",
          "Change in group difference from low x to high x",
          "Generating latent-scale x-by-group product term"
        ),
        value_sum_score_units = c(
          group_difference(e00, e01),
          group_difference(e10, e11),
          e10 - e00,
          e11 - e01,
          change_in_group_difference(e00, e01, e10, e11),
          NA_real_
        ),
        value_percent_of_max = c(
          group_difference(e00, e01),
          group_difference(e10, e11),
          e10 - e00,
          e11 - e01,
          change_in_group_difference(e00, e01, e10, e11),
          NA_real_
        ) / settings$max_score,
        latent_scale_value = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, settings$beta_x_group),
        stringsAsFactors = FALSE
      )
}))

scenario_table <- rbind(
  data.frame(
    table_part = "implied_values",
    scenario_values,
    contrast = NA_character_,
    value_sum_score_units = NA_real_,
    value_percent_of_max = NA_real_,
    latent_scale_value = NA_real_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    table_part = "derived_contrasts",
    scenario = scenario_contrasts$scenario,
    x = NA_real_,
    group = NA_character_,
    latent_mean = NA_real_,
    expected_sum_score = NA_real_,
    expected_percent_of_max = NA_real_,
    contrast = scenario_contrasts$contrast,
    value_sum_score_units = scenario_contrasts$value_sum_score_units,
    value_percent_of_max = scenario_contrasts$value_percent_of_max,
    latent_scale_value = scenario_contrasts$latent_scale_value,
    stringsAsFactors = FALSE
  )
)

utils::write.csv(scenario_table, settings$scenario_table_path, row.names = FALSE)

cat("\n", "Implied scenario values", "\n")
print(scenario_values)
cat("\n", "Derived contrasts implied by each scenario", "\n")
print(scenario_contrasts)
cat("\nGroup gaps are Group 1 minus Group 0; changes are high minus low.\n")

# ---------------------------------------------------------------------
# 3. Deterministic scenario plotting data
# ---------------------------------------------------------------------
plot_grid <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
      s <- scenarios[i, ]
      th <- make_thresholds(s$threshold_shift)

      g <- expand.grid(
        x = seq(settings$x_plot_range[1], settings$x_plot_range[2], length.out = 200),
        group_num = c(0, 1)
      )

      g$scenario <- s$scenario
      g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
      g$latent_mean <- latent_mean(g$x, g$group_num)
      g$expected_sum_score <- vapply(g$latent_mean, expected_sum_given_mu, numeric(1), thresholds = th)
      g$expected_percent_of_max <- g$expected_sum_score / settings$max_score
      g
}))

gap_data <- do.call(rbind, lapply(split(plot_grid, plot_grid$scenario), function(dat) {
      d0 <- dat[dat$group_num == 0, c("scenario", "x", "expected_sum_score")]
      d1 <- dat[dat$group_num == 1, c("scenario", "x", "expected_sum_score")]
      gap <- group_difference(d0$expected_sum_score, d1$expected_sum_score)

      data.frame(
        scenario = d0$scenario,
        x = d0$x,
        group_difference_sum_score_units = gap,
        group_difference_percent_of_max = gap / settings$max_score,
        stringsAsFactors = FALSE
      )
}))

# Threshold inspection data.
threshold_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
      th <- make_thresholds(scenarios$threshold_shift[i])

      data.frame(
        scenario = scenarios$scenario[i],
        item = rep(seq_len(settings$J), 3),
        threshold_number = rep(1:3, each = settings$J),
        threshold = as.vector(th),
        stringsAsFactors = FALSE
      )
}))

# ---------------------------------------------------------------------
# 4. Simulate one example dataset per scenario
# ---------------------------------------------------------------------

example_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
      threshold_shift <- scenarios$threshold_shift[i]
      x <- stats::rnorm(settings$N, 0, 1)
      group_num <- stats::rbinom(settings$N, 1, 0.5)
      mu <- settings$beta_x * x + settings$beta_group * group_num +
        settings$beta_x_group * x * group_num
      theta <- mu + stats::rnorm(settings$N, 0, settings$theta_sd)
      thresholds <- matrix(rep(c(-1, 0, 1), each = settings$J), nrow = settings$J) +
        seq(-0.45, 0.45, length.out = settings$J) + threshold_shift
      items <- matrix(NA_integer_, nrow = settings$N, ncol = settings$J)
      for (j in seq_len(settings$J)) {
        p_ge_1 <- stats::plogis(theta - thresholds[j, 1])
        p_ge_2 <- stats::plogis(theta - thresholds[j, 2])
        p_ge_3 <- stats::plogis(theta - thresholds[j, 3])
        u <- stats::runif(settings$N)
        items[, j] <- 0L + (u < p_ge_1) + (u < p_ge_2) + (u < p_ge_3)
      }
      sum_score <- rowSums(items)

      # The Gaussian-probit model below requires responses strictly inside (0, 1).
      # We therefore use a continuity-corrected score proportion for that model:
      # (sum_score + 0.5) / (max_score + 1).
      # The raw sum score and raw score proportion are left unchanged for all
      # descriptive summaries, plots, and the identity-link model.
      score_prop <- sum_score / settings$max_score
      score_prop_probit <- (sum_score + 0.5) / (settings$max_score + 1)

      d <- data.frame(
        x = x,
        group_num = group_num,
        group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
        latent_mean = mu,
        theta = theta,
        sum_score = sum_score,
        score_prop = score_prop,
        score_prop_probit = score_prop_probit,
        max_score = settings$max_score,
        stringsAsFactors = FALSE
      )
      d$scenario <- scenarios$scenario[i]
      d
}))

# Binned means for plotting.
example_data$x_bin <- cut(example_data$x, breaks = settings$x_bin_breaks, include.lowest = TRUE)
example_binned <- stats::aggregate(sum_score ~ scenario + x_bin + group, example_data, mean)
example_binned$x_mid <- {
  bin_levels <- levels(example_binned$x_bin)
  bin_labels <- gsub("\\[|\\]|\\(|\\)", "", bin_levels)
  midpoints <- vapply(strsplit(bin_labels, ","), function(z) mean(as.numeric(z)), numeric(1))
  midpoints[as.integer(example_binned$x_bin)]
}

cat("\n", "One example dataset per scenario", "\n")
cat("Observed mean sum score by scenario and group:\n")
print(stats::aggregate(sum_score ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 5. Models fitted explicitly in each Monte Carlo replication
# ---------------------------------------------------------------------
# The intermediate bounded-score model is deliberately not a binomial model.
# It is a Gaussian GLM with a probit link, fitted to the continuity-corrected
# score proportion defined above. Thus it constrains fitted means
# to the 0-1 interval before rescaling predictions to the sum-score metric,
# but it does not assume binomial variance or independent item-level successes.
model_names <- c(
  "Observed sum-score identity",
  "Gaussian-probit bounded score",
  "Latent generating scale"
)

# ---------------------------------------------------------------------
# 6. Repeated simulation
# ---------------------------------------------------------------------
cat("\n", "Monte Carlo simulation", "\n")
cat("Running B = ", settings$B, " replications per scenario.\n", sep = "")

run_replication <- function(replication, scenario_name, threshold_shift) {
  x <- stats::rnorm(settings$N, 0, 1)
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  mu <- settings$beta_x * x + settings$beta_group * group_num +
    settings$beta_x_group * x * group_num
  theta <- mu + stats::rnorm(settings$N, 0, settings$theta_sd)
  thresholds <- matrix(rep(c(-1, 0, 1), each = settings$J), nrow = settings$J) +
    seq(-0.45, 0.45, length.out = settings$J) + threshold_shift
  items <- matrix(NA_integer_, nrow = settings$N, ncol = settings$J)
  for (j in seq_len(settings$J)) {
    p_ge_1 <- stats::plogis(theta - thresholds[j, 1])
    p_ge_2 <- stats::plogis(theta - thresholds[j, 2])
    p_ge_3 <- stats::plogis(theta - thresholds[j, 3])
    u <- stats::runif(settings$N)
    items[, j] <- 0L + (u < p_ge_1) + (u < p_ge_2) + (u < p_ge_3)
  }
  sum_score <- rowSums(items)

  # The Gaussian-probit model below requires responses strictly inside (0, 1).
  # We therefore use a continuity-corrected score proportion for that model:
  # (sum_score + 0.5) / (max_score + 1).
  # The raw sum score and raw score proportion are left unchanged for all
  # descriptive summaries, plots, and the identity-link model.
  score_prop <- sum_score / settings$max_score
  score_prop_probit <- (sum_score + 0.5) / (settings$max_score + 1)

  d <- data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    latent_mean = mu,
    theta = theta,
    sum_score = sum_score,
    score_prop = score_prop,
    score_prop_probit = score_prop_probit,
    max_score = settings$max_score,
    stringsAsFactors = FALSE
  )

  fit_identity <- try(stats::lm(sum_score ~ x * group, data = d), silent = TRUE)
  fit_probit <- try(stats::glm(score_prop_probit ~ x * group,
      family = stats::gaussian(link = stats::make.link("probit")),
      data = d), silent = TRUE)
  fit_latent <- try(stats::lm(theta ~ x * group, data = d), silent = TRUE)
  nd <- expand.grid(x = c(x_low, x_high),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")))
  nd$max_score <- settings$max_score
  p_values <- coefficients <- did <- rep(NA_real_, 3)
  if (!inherits(fit_identity, "try-error")) {
    sm <- summary(fit_identity)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[1] <- sm["x:groupGroup 1", 4]
      coefficients[1] <- sm["x:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_identity, newdata = nd, type = "response")
    did[1] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (!inherits(fit_probit, "try-error")) {
    sm <- summary(fit_probit)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[2] <- sm["x:groupGroup 1", 4]
      coefficients[2] <- sm["x:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_probit, newdata = nd, type = "response") * settings$max_score
    did[2] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (!inherits(fit_latent, "try-error")) {
    sm <- summary(fit_latent)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[3] <- sm["x:groupGroup 1", 4]
      coefficients[3] <- sm["x:groupGroup 1", 1]
    }
    pred <- stats::predict(fit_latent, newdata = nd, type = "response")
    did[3] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  data.frame(scenario = scenario_name, model = model_names, replication = replication,
    p_value = p_values, interaction_coef = coefficients,
    change_in_group_difference_response_scale = did / settings$max_score,
    change_in_group_difference_outcome_units = did,
    stringsAsFactors = FALSE)
}

# N_CORES overrides the SLURM allocation; otherwise use SLURM_CPUS_PER_TASK.
# Parallelize replications only. Each worker uses one BLAS/OpenMP thread.
cluster <- NULL
if (settings$n_cores > 1 && .Platform$OS.type != "unix") {
  cluster <- parallel::makeCluster(settings$n_cores)
  parallel::clusterSetRNGStream(cluster, iseed = 20260526)
  parallel::clusterExport(cluster, c("settings", "model_names", "x_low", "x_high"))
}
scenario_results <- list()
for (i in seq_len(nrow(scenarios))) {
  s <- scenarios[i, ]
  if (settings$n_cores > 1 && .Platform$OS.type == "unix") {
    replications <- parallel::mclapply(seq_len(settings$B), run_replication,
      scenario_name = s$scenario, threshold_shift = s$threshold_shift, mc.cores = settings$n_cores, mc.set.seed = TRUE)
  } else if (!is.null(cluster)) {
    replications <- parallel::parLapply(cluster, seq_len(settings$B), run_replication,
      scenario_name = s$scenario, threshold_shift = s$threshold_shift)
  } else {
    replications <- lapply(seq_len(settings$B), run_replication, scenario_name = s$scenario, threshold_shift = s$threshold_shift)
  }
  scenario_results[[length(scenario_results) + 1L]] <- do.call(rbind, replications)
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
simulation_results <- do.call(rbind, scenario_results)

simulation_summary <- do.call(rbind, lapply(
    split(simulation_results, list(simulation_results$scenario, simulation_results$model), drop = TRUE),
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
      data.frame(scenario = dat$scenario[1], model = dat$model[1], sm, stringsAsFactors = FALSE)
    }
))

simulation_summary <- simulation_summary[order(simulation_summary$scenario, simulation_summary$model), ]
simulation_summary$rate_type <- ifelse(
  simulation_summary$model == "Latent generating scale",
  "Nominal rejection rate",
  "Pseudo-interaction detection rate"
)
simulation_summary$change_in_group_difference_units <- ifelse(
  simulation_summary$model == "Latent generating scale",
  "latent theta units",
  paste0("sum-score units on the 0-", settings$max_score, " scale")
)

utils::write.csv(simulation_summary, settings$simulation_summary_path, row.names = FALSE)

cat("\n", "Simulation summary", "\n")
print(simulation_summary)
cat("\nThe generating x-by-group product term is zero on the latent scale.\n")
cat("The latent generating-scale benchmark supplies nominal rejection rates.\n")
cat("Observed and bounded-score analyses supply pseudo-interaction detection rates.\n")
cat("\nInterpretation aid: median_change_in_group_difference_outcome_units is a contrast, not a possible observed score.\n")
cat("For observed-score models, it is the median model-implied change in the predicted group difference\n")
cat(
  "from x = ", x_low, " to x = ", x_high,
  ", expressed in sum-score units on the 0-", settings$max_score, " scale.\n",
  sep = ""
)
cat("For the latent-generating-scale comparison, the same quantity is expressed in latent theta units.\n")

# ---------------------------------------------------------------------
# 7. Figure panels
# ---------------------------------------------------------------------
base_latent_steps <- seq(-6, 6, by = 1)

latent_scale_grid <- do.call(
  rbind,
  lapply(seq_len(nrow(scenarios)), function(i) {
      th <- make_thresholds(scenarios$threshold_shift[i])

      latent_steps <- base_latent_steps + scenarios$threshold_shift[i]

      data.frame(
        scenario = scenarios$scenario[i],
        yintercept = vapply(
          latent_steps,
          expected_sum_given_mu,
          numeric(1),
          thresholds = th
        )
      )
  })
)

latent_scale_grid$scenario <- factor(
  latent_scale_grid$scenario,
  levels = scenarios$scenario
)

pA <- ggplot2::ggplot(
  plot_grid,
  ggplot2::aes(x, expected_sum_score, color = group, linetype = group)
) +
  ggplot2::geom_hline(
  data = latent_scale_grid,
  ggplot2::aes(yintercept = yintercept),
  inherit.aes = FALSE,
  color = "grey82",
  linewidth = 0.35
) +
  ggplot2::geom_hline(
  yintercept = c(0, settings$max_score),
  linetype = "dashed"
) +
  ggplot2::geom_line(linewidth = .95) +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::scale_y_continuous(limits = c(-.5, settings$max_score + .5)) +
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
  title = "A. Implied sum-score curves",
  subtitle = "Horizontal lines mark equal steps on the latent scale",
  x = "Continuous predictor x",
  y = "Expected sum score"
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
))+
  ggplot2::theme(
  panel.grid.major.y = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank()
)

pB <- ggplot2::ggplot(simulation_summary, ggplot2::aes(x = model, y = rejection_rate)) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::labs(
  title = "B. Product-term rejection rate",
  subtitle = "Latent generating-scale model: nominal rejection; observed-score models: pseudo-interaction detection. Dashed line: nominal alpha",
  x = NULL,
  y = "Replications rejecting the x-by-group product term"
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
))

# Write the same panel layout to PDF and PNG.
plots <- list(pA, pB)
plot_columns <- 1
plot_rows <- ceiling(length(plots) / plot_columns)
dir.create(dirname(settings$figure_base), recursive = TRUE, showWarnings = FALSE)
for (plot_format in c("pdf", "png")) {
  if (plot_format == "pdf") {
    grDevices::pdf(paste0(settings$figure_base, ".pdf"), width = figure_width, height = 7.2)
  } else {
    grDevices::png(paste0(settings$figure_base, ".png"), width = figure_width, height = 7.2, units = "in", res = default_dpi)
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

# Extra inspection plot: model-implied change in group difference.
p_effect <- ggplot2::ggplot(
  simulation_summary,
  ggplot2::aes(x = model, y = median_change_in_group_difference_outcome_units)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::labs(
  title = "Inspection: median model-implied change in the group difference",
  subtitle = "Observed-score models are in sum-score units; the latent-generating-scale comparison is in latent theta units",
  x = NULL,
  y = paste0("Change in group gap from ", (x_low), " to ", (x_high), ", sum-score units on the 0-", (settings$max_score), " scale")
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
))

dir.create(dirname(settings$inspection_effect_base), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(paste0(settings$inspection_effect_base, ".pdf"), plot = p_effect, width = 8, height = 4.5)
ggplot2::ggsave(paste0(settings$inspection_effect_base, ".png"), plot = p_effect, width = 8, height = 4.5, dpi = default_dpi)

p_thresholds <- ggplot2::ggplot(
  threshold_data,
  ggplot2::aes(item, threshold, shape = factor(threshold_number))
) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::labs(
  title = "Inspection: item thresholds by scenario",
  x = "Item",
  y = "Threshold",
  shape = "Threshold"
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
))

dir.create(dirname(settings$inspection_thresholds_base), recursive = TRUE, showWarnings = FALSE)
ggplot2::ggsave(paste0(settings$inspection_thresholds_base, ".pdf"), plot = p_thresholds, width = 8, height = 4.5)
ggplot2::ggsave(paste0(settings$inspection_thresholds_base, ".png"), plot = p_thresholds, width = 8, height = 4.5, dpi = default_dpi)

saveRDS(
  list(
    settings = settings,
    scenarios = scenarios,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    threshold_data = threshold_data,
    example_dataset = example_data,
    example_binned = example_binned,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = settings$rds_path
)

cat("\n", "Saved files", "\n")
cat("- ", settings$scenario_table_path, "\n", sep = "")
cat("- ", settings$simulation_summary_path, "\n", sep = "")
cat("- ", settings$figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_effect_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_thresholds_base, ".pdf/png\n", sep = "")
cat("- ", settings$rds_path, "\n", sep = "")
cat("\nDone.\n")
