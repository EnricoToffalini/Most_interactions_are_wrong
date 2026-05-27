# scripts/04-simulation-sum-scores.R
# Simulation 2: sum scores as bounded and discrete outcomes.
# The script makes each threshold scenario visible before testing models.

rm(list = ls())
if (!file.exists("R/simulation-settings.R") && file.exists("../R/simulation-settings.R")) setwd("..")

source("R/simulation-settings.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260526)

report_header("Simulation 2: bounded discrete sum scores")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
settings <- list(
  N = 600,
  J = 9,
  item_max = 3,
  max_score = 27,
  theta_sd = 1.00,
  beta_x = 0.85,
  beta_group = -0.90,
  beta_x_group = 0.00,
  B = default_B,
  alpha = default_alpha
)
settings$max_score <- settings$J * settings$item_max

scenarios <- data.frame(
  scenario = c("Middle scores", "Low scores", "High scores"),
  threshold_shift = c(0.00, 1.60, -1.60),
  interpretation = c(
    "Item thresholds are centered; expected scores occupy the middle of the scale.",
    "Thresholds are high; expected scores are closer to the lower bound.",
    "Thresholds are low; expected scores are closer to the upper bound."
  ),
  stringsAsFactors = FALSE
)

report_section("Scenario parameters you can tune")
print_compact(list_to_table(settings))
cat("\nScenario-specific threshold shifts:\n")
print_compact(scenarios)
cat("\nTuning guide:\n")
cat("- Increase threshold_shift to make items harder and move scores toward the floor.\n")
cat("- Decrease threshold_shift to make items easier and move scores toward the ceiling.\n")
cat("- Increase beta_x to strengthen the continuous predictor effect on the latent trait.\n")
cat("- Make beta_group more negative to increase the latent group gap.\n")
cat("- beta_x_group = 0, so the true latent-scale interaction is absent.\n")

# Deterministic item thresholds: no hidden random jitter.
make_thresholds <- function(threshold_shift) {
  item_offsets <- seq(-0.45, 0.45, length.out = settings$J)
  base <- matrix(rep(c(-1, 0, 1), each = settings$J), nrow = settings$J)
  base + item_offsets + threshold_shift
}

latent_mean <- function(x, group) {
  settings$beta_x * x + settings$beta_group * group + settings$beta_x_group * x * group
}

# Expected item score for a given theta and thresholds.
expected_item_score_at_theta <- function(theta, thresholds) {
  # Category is 0, 1, 2, 3. Expected score is P(Y >= 1) + P(Y >= 2) + P(Y >= 3).
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
  g <- expand.grid(x = x_values_for_summary, group_num = c(0, 1))
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
  x_low <- min(x_values_for_summary)
  x_high <- max(x_values_for_summary)
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
    latent_scale_value = c(NA, NA, NA, NA, NA, settings$beta_x_group),
    stringsAsFactors = FALSE
  )
}))

scenario_table <- rbind(
  data.frame(table_part = "implied_values", scenario_values, contrast = NA_character_,
             value_sum_score_units = NA_real_, value_percent_of_max = NA_real_,
             latent_scale_value = NA_real_, stringsAsFactors = FALSE),
  data.frame(table_part = "derived_contrasts", scenario = scenario_contrasts$scenario,
             x = NA_real_, group = NA_character_, latent_mean = NA_real_,
             expected_sum_score = NA_real_, expected_percent_of_max = NA_real_,
             contrast = scenario_contrasts$contrast,
             value_sum_score_units = scenario_contrasts$value_sum_score_units,
             value_percent_of_max = scenario_contrasts$value_percent_of_max,
             latent_scale_value = scenario_contrasts$latent_scale_value,
             stringsAsFactors = FALSE)
)

utils::write.csv(scenario_table, "tables/scenario-table-sum-scores.csv", row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by each scenario")
print_compact(scenario_contrasts)
report_sign_convention(paste0("x = ", min(x_values_for_summary)), paste0("x = ", max(x_values_for_summary)))

# ---------------------------------------------------------------------
# 3. Deterministic scenario plotting data
# ---------------------------------------------------------------------
plot_grid <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  th <- make_thresholds(s$threshold_shift)
  g <- expand.grid(x = seq(-2.5, 2.5, length.out = 200), group_num = c(0, 1))
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
  data.frame(
    scenario = d0$scenario,
    x = d0$x,
    group_difference_sum_score_units = group_difference(d0$expected_sum_score, d1$expected_sum_score),
    group_difference_percent_of_max = group_difference(d0$expected_sum_score, d1$expected_sum_score) / settings$max_score
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
  data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    latent_mean = mu,
    theta = theta,
    sum_score = sum_score,
    score_prop = sum_score / settings$max_score,
    max_score = settings$max_score
  )
}

example_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  d <- simulate_one(scenarios$threshold_shift[i])
  d$scenario <- scenarios$scenario[i]
  d
}))

# Binned means for plotting.
breaks <- seq(-2.5, 2.5, length.out = 11)
example_data$x_bin <- cut(example_data$x, breaks = breaks, include.lowest = TRUE)
example_binned <- aggregate(sum_score ~ scenario + x_bin + group, example_data, mean)
example_binned$x_mid <- bin_midpoints(example_binned$x_bin)

report_section("One example dataset per scenario")
cat("Observed mean sum score by scenario and group:\n")
print_compact(aggregate(sum_score ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 5. Model fitting helpers
# ---------------------------------------------------------------------
model_names <- c("Sum-score identity", "Bounded-score logit", "Latent oracle")

fit_models <- function(d) {
  list(
    "Sum-score identity" = try(stats::lm(sum_score ~ x * group, data = d), silent = TRUE),
    "Bounded-score logit" = try(stats::glm(cbind(sum_score, max_score - sum_score) ~ x * group, family = stats::binomial("logit"), data = d), silent = TRUE),
    "Latent oracle" = try(stats::lm(theta ~ x * group, data = d), silent = TRUE)
  )
}

model_p <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name %in% c("Sum-score identity", "Latent oracle")) return(interaction_p_from_lm(fit))
  interaction_p_from_glm(fit)
}

model_coef <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name %in% c("Sum-score identity", "Latent oracle")) return(interaction_coef_from_lm(fit))
  interaction_coef_from_glm(fit)
}

model_predict_response <- function(fit, model_name, newdata) {
  if (inherits(fit, "try-error")) return(rep(NA_real_, nrow(newdata)))
  if (model_name == "Sum-score identity") return(stats::predict(fit, newdata = newdata))
  if (model_name == "Latent oracle") return(stats::predict(fit, newdata = newdata))
  stats::predict(fit, newdata = newdata, type = "response") * settings$max_score
}

model_did <- function(fit, model_name) {
  nd <- expand.grid(
    x = c(-1, 1),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  nd$max_score <- settings$max_score
  pred <- model_predict_response(fit, model_name, nd)
  p_low_g0 <- pred[nd$x == -1 & nd$group == "Group 0"]
  p_low_g1 <- pred[nd$x == -1 & nd$group == "Group 1"]
  p_high_g0 <- pred[nd$x == 1 & nd$group == "Group 0"]
  p_high_g1 <- pred[nd$x == 1 & nd$group == "Group 1"]
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

simulation_summary <- do.call(rbind, lapply(split(simulation_results, list(simulation_results$scenario, simulation_results$model), drop = TRUE), function(dat) {
  sm <- summarise_model_simulation(dat, alpha = settings$alpha)
  data.frame(scenario = dat$scenario[1], model = dat$model[1], sm, stringsAsFactors = FALSE)
}))
simulation_summary <- simulation_summary[order(simulation_summary$scenario, simulation_summary$model), ]
simulation_summary$change_in_group_difference_units <- ifelse(
  simulation_summary$model == "Latent oracle",
  "latent theta units",
  paste0("sum-score units on the 0-", settings$max_score, " scale")
)

utils::write.csv(simulation_summary, "tables/simulation-summary-sum-scores.csv", row.names = FALSE)

report_section("Simulation summary")
print_compact(simulation_summary)
cat("\nInterpretation aid: median_change_in_group_difference_outcome_units is a contrast, not a possible observed score.\n")
cat("For observed-score models, it is the median model-implied change in the predicted group difference\n")
cat("from x = ", min(x_values_for_summary), " to x = ", max(x_values_for_summary), ", expressed in sum-score units on the 0-", settings$max_score, " scale.\n", sep = "")
cat("For the oracle model, the same quantity is expressed in latent theta units.\n")

# ---------------------------------------------------------------------
# 7. Figure panels
# ---------------------------------------------------------------------
pA <- ggplot(plot_grid, aes(x, expected_sum_score, linetype = group)) +
  geom_hline(yintercept = c(0, settings$max_score), linetype = "dashed") +
  geom_line(linewidth = .95) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits = c(-.5, settings$max_score + .5)) +
  labs(title = "A. Implied sum-score curves", subtitle = "No x-by-group term on the latent scale", x = "Continuous predictor x", y = "Expected sum score") +
  link_theme()

pB <- ggplot(gap_data, aes(x, group_difference_sum_score_units)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = .95) +
  facet_wrap(~ scenario) +
  labs(
    title = "B. Implied predicted group difference at each x",
    subtitle = "This is Group 1 minus Group 0 on the observed sum-score scale",
    x = "Continuous predictor x",
    y = axis_title_group_gap_sum_score(settings$max_score)
  ) +
  link_theme()

pC <- ggplot(simulation_summary, aes(x = model, y = false_positive_rate)) +
  geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(title = "C. False-positive interaction rate", subtitle = "Dashed line is nominal alpha", x = NULL, y = "Rate") +
  link_theme(base_size = 9)

save_plot_grid(
  list(pA, pB, pC),
  filename_base = "paper/figs/fig3-sum-score-simulation",
  width = figure_width,
  height = 7.2,
  ncol = 1,
  dpi = default_dpi
)

# Extra inspection plot: model-implied change in group difference.
p_effect <- ggplot(simulation_summary, aes(x = model, y = median_change_in_group_difference_outcome_units)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(
    title = "Inspection: median model-implied change in the group difference",
    subtitle = "Observed-score models are in sum-score units; the oracle is in latent theta units",
    x = NULL,
    y = axis_title_change_group_gap_sum_score(min(x_values_for_summary), max(x_values_for_summary), settings$max_score)
  ) +
  link_theme()
save_single_plot(p_effect, "outputs/inspection/sum-score-effect-size-inspection", width = 8, height = 4.5)

p_thresholds <- ggplot(threshold_data, aes(item, threshold, shape = factor(threshold_number))) +
  geom_point(size = 1.8) +
  facet_wrap(~ scenario) +
  labs(title = "Inspection: item thresholds by scenario", x = "Item", y = "Threshold", shape = "Threshold") +
  link_theme()
save_single_plot(p_thresholds, "outputs/inspection/sum-score-thresholds-inspection", width = 8, height = 4.5)

saveRDS(
  list(
    settings = settings,
    scenarios = scenarios,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    threshold_data = threshold_data,
    example_dataset = example_data,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = "outputs/simulation-sum-scores.rds"
)

report_section("Saved files")
cat("- tables/scenario-table-sum-scores.csv\n")
cat("- tables/simulation-summary-sum-scores.csv\n")
cat("- paper/figs/fig3-sum-score-simulation.pdf/png\n")
cat("- outputs/inspection/sum-score-effect-size-inspection.pdf/png\n")
cat("- outputs/inspection/sum-score-thresholds-inspection.pdf/png\n")
cat("- outputs/simulation-sum-scores.rds\n")
cat("\nDone.\n")
