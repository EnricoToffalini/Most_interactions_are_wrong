# scripts/03b-simulation-sum-scores.R
# Simulation 2: sum scores as bounded and discrete outcomes.
#
# Scientific choices for this simulation live in this script.
# Project-level defaults and reusable functions live in R/.

rm(list = ls())

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260526)

report_header("Simulation 2: sum scores")

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
  B = default_B,
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
  scenario = c("Middle of scale", "Near floor", "Near ceiling"),
  threshold_shift = c(0.00, 1.60, -1.60),
  interpretation = c(
    "Item thresholds are centered; expected sum scores occupy the middle of the observed scale.",
    "Item thresholds are high; expected sum scores are compressed near the lower bound.",
    "Item thresholds are low; expected sum scores are compressed near the upper bound."
  ),
  stringsAsFactors = FALSE
)

report_section("Scenario parameters")
print_compact(list_to_table(settings))
cat("\nScenario-specific threshold shifts:\n")
print_compact(scenarios)

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

# Expected item score for a given theta and thresholds.
expected_item_score_at_theta <- function(theta, thresholds) {
  # Category is 0, 1, 2, 3.
  # Expected score is P(Y >= 1) + P(Y >= 2) + P(Y >= 3).
  rowSums(stats::plogis(outer(theta, thresholds, "-")))
}

# Expected sum score after integrating over individual latent residuals.
expected_sum_given_mu <- function(mu, thresholds) {
  q <- stats::qnorm((seq_len(201) - 0.5) / 201) * settings$theta_sd
  mean(vapply(mu + q, function(th) sum(expected_item_score_at_theta(th, thresholds)), numeric(1)))
}

simulate_items <- function(theta, thresholds) {
  N <- length(theta)
  J <- nrow(thresholds)
  out <- matrix(NA_integer_, nrow = N, ncol = J)
  
  for (j in seq_len(J)) {
    p_ge_1 <- stats::plogis(theta - thresholds[j, 1])
    p_ge_2 <- stats::plogis(theta - thresholds[j, 2])
    p_ge_3 <- stats::plogis(theta - thresholds[j, 3])
    u <- stats::runif(N)
    out[, j] <- 0L + (u < p_ge_1) + (u < p_ge_2) + (u < p_ge_3)
  }
  
  out
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

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by each scenario")
print_compact(scenario_contrasts)
report_sign_convention(paste0("x = ", x_low), paste0("x = ", x_high))

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
simulate_one <- function(threshold_shift) {
  x <- stats::rnorm(settings$N, 0, 1)
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  mu <- latent_mean(x, group_num)
  theta <- mu + stats::rnorm(settings$N, 0, settings$theta_sd)
  items <- simulate_items(theta, make_thresholds(threshold_shift))
  sum_score <- rowSums(items)
  
  # The Gaussian-probit model below requires responses strictly inside (0, 1).
  # We therefore use a continuity-corrected score proportion for that model:
  # (sum_score + 0.5) / (max_score + 1).
  # The raw sum score and raw score proportion are left unchanged for all
  # descriptive summaries, plots, and the identity-link model.
  score_prop <- sum_score / settings$max_score
  score_prop_probit <- (sum_score + 0.5) / (settings$max_score + 1)
  
  data.frame(
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
}

example_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  d <- simulate_one(scenarios$threshold_shift[i])
  d$scenario <- scenarios$scenario[i]
  d
}))

# Binned means for plotting.
example_data$x_bin <- cut(example_data$x, breaks = settings$x_bin_breaks, include.lowest = TRUE)
example_binned <- stats::aggregate(sum_score ~ scenario + x_bin + group, example_data, mean)
example_binned$x_mid <- bin_midpoints(example_binned$x_bin)

report_section("One example dataset per scenario")
cat("Observed mean sum score by scenario and group:\n")
print_compact(stats::aggregate(sum_score ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 5. Model fitting helpers
# ---------------------------------------------------------------------
# The intermediate bounded-score model is deliberately not a binomial model.
# It is a Gaussian GLM with a probit link, fitted to the continuity-corrected
# score proportion defined in simulate_one(). Thus it constrains fitted means
# to the 0-1 interval before rescaling predictions to the sum-score metric,
# but it does not assume binomial variance or independent item-level successes.
model_names <- c(
  "Observed sum-score identity",
  "Gaussian-probit bounded score",
  "Latent generating scale"
)

fit_models <- function(d) {
  list(
    "Observed sum-score identity" = try(
      stats::lm(sum_score ~ x * group, data = d),
      silent = TRUE
    ),
    "Gaussian-probit bounded score" = try(
      stats::glm(
        score_prop_probit ~ x * group,
        family = stats::gaussian(link = stats::make.link("probit")),
        data = d
      ),
      silent = TRUE
    ),
    "Latent generating scale" = try(
      stats::lm(theta ~ x * group, data = d),
      silent = TRUE
    )
  )
}

model_p <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name %in% c("Observed sum-score identity", "Latent generating scale")) {
    return(interaction_p_from_lm(fit))
  }
  interaction_p_from_glm(fit)
}

model_coef <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name %in% c("Observed sum-score identity", "Latent generating scale")) {
    return(interaction_coef_from_lm(fit))
  }
  interaction_coef_from_glm(fit)
}

model_predict_response <- function(fit, model_name, newdata) {
  if (inherits(fit, "try-error")) return(rep(NA_real_, nrow(newdata)))
  if (model_name == "Observed sum-score identity") return(stats::predict(fit, newdata = newdata))
  if (model_name == "Latent generating scale") return(stats::predict(fit, newdata = newdata))
  stats::predict(fit, newdata = newdata, type = "response") * settings$max_score
}

model_did <- function(fit, model_name) {
  nd <- expand.grid(
    x = c(x_low, x_high),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  nd$max_score <- settings$max_score
  
  pred <- model_predict_response(fit, model_name, nd)
  
  p_low_g0 <- pred[nd$x == x_low & nd$group == "Group 0"]
  p_low_g1 <- pred[nd$x == x_low & nd$group == "Group 1"]
  p_high_g0 <- pred[nd$x == x_high & nd$group == "Group 0"]
  p_high_g1 <- pred[nd$x == x_high & nd$group == "Group 1"]
  
  change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1)
}

# ---------------------------------------------------------------------
# 6. Repeated simulation
# ---------------------------------------------------------------------
report_section("Monte Carlo simulation")
cat("Running B = ", settings$B, " replications per scenario.\n", sep = "")

simulation_results <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  cat("Scenario: ", s$scenario, " ", sep = "")
  
  do.call(rbind, lapply(seq_len(settings$B), function(b) {
    progress_tick(b, settings$B)
    d <- simulate_one(s$threshold_shift)
    fits <- fit_models(d)
    
    do.call(rbind, lapply(model_names, function(m) {
      fit <- fits[[m]]
      did <- model_did(fit, m)
      
      data.frame(
        scenario = s$scenario,
        model = m,
        p_value = model_p(fit, m),
        interaction_coef = model_coef(fit, m),
        change_in_group_difference_response_scale = did / settings$max_score,
        change_in_group_difference_outcome_units = did,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

simulation_summary <- do.call(rbind, lapply(
  split(simulation_results, list(simulation_results$scenario, simulation_results$model), drop = TRUE),
  function(dat) {
    sm <- summarise_model_simulation(dat, alpha = settings$alpha)
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

report_section("Simulation summary")
print_compact(simulation_summary)
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
pA <- ggplot2::ggplot(plot_grid, ggplot2::aes(x, expected_sum_score, color = group, linetype = group)) +
  ggplot2::geom_hline(yintercept = c(0, settings$max_score), linetype = "dashed") +
  ggplot2::geom_line(linewidth = .95) +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::scale_y_continuous(limits = c(-.5, settings$max_score + .5)) +
  link_scale_color_discrete(name = NULL) +
  link_scale_linetype_discrete(name = NULL) +
  ggplot2::labs(
    title = "A. Implied sum-score curves",
    subtitle = "No x-by-group term on the latent scale",
    x = "Continuous predictor x",
    y = "Expected sum score"
  ) +
  link_theme()

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
  link_theme(base_size = 9)

save_plot_grid(
  list(pA, pB),
  filename_base = settings$figure_base,
  width = figure_width,
  height = 7.2,
  ncol = 1,
  dpi = default_dpi
)

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
    y = axis_title_change_group_gap_sum_score(x_low, x_high, settings$max_score)
  ) +
  link_theme()

save_single_plot(
  p_effect,
  settings$inspection_effect_base,
  width = 8,
  height = 4.5,
  dpi = default_dpi
)

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
  link_theme()

save_single_plot(
  p_thresholds,
  settings$inspection_thresholds_base,
  width = 8,
  height = 4.5,
  dpi = default_dpi
)

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

report_section("Saved files")
cat("- ", settings$scenario_table_path, "\n", sep = "")
cat("- ", settings$simulation_summary_path, "\n", sep = "")
cat("- ", settings$figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_effect_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_thresholds_base, ".pdf/png\n", sep = "")
cat("- ", settings$rds_path, "\n", sep = "")
cat("\nDone.\n")
