# scripts/05-simulation-within-family-links.R
# Simulation 3: within-family link choice for binary outcomes.
# Shows when logit/probit/cloglog are practically similar and when tail behavior matters.

rm(list = ls())
if (!file.exists("R/simulation-settings.R") && file.exists("../R/simulation-settings.R")) setwd("..")

source("R/simulation-settings.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260527)

report_header("Simulation 3: within-family link choice")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
settings <- list(
  N = 1200,
  beta_x_group = 0.00,
  x_summary_values = c(-1, 0, 1),
  x_plot_range = c(-2.5, 2.5),
  B = default_B,
  alpha = default_alpha
)

scenarios <- data.frame(
  scenario = c("Middle probabilities", "Near upper bound"),
  beta_intercept = c(0.00, 2.30),
  beta_x = c(0.90, 1.20),
  beta_group = c(-0.80, -1.40),
  generating_link = c("logit", "cloglog"),
  interpretation = c(
    "Predicted probabilities mostly occupy the middle range; logit and probit should be very similar.",
    "Predicted probabilities are close to the upper bound; wrong links can mimic moderation."
  ),
  stringsAsFactors = FALSE
)

candidate_links <- c("logit", "probit", "cloglog")

report_section("Scenario parameters you can tune")
print_compact(list_to_table(settings))
cat("\nScenario-specific settings:\n")
print_compact(scenarios)
cat("\nTuning guide:\n")
cat("- beta_intercept moves probabilities toward the lower or upper bound.\n")
cat("- beta_x and beta_group control the predictor effect and group gap.\n")
cat("- Change generating_link to logit, probit, or cloglog.\n")
cat("- beta_x_group = 0, so the true generating link-scale interaction is absent.\n")

eta_fun <- function(x, group, beta_intercept, beta_x, beta_group) {
  beta_intercept + beta_x * x + beta_group * group + settings$beta_x_group * x * group
}

p_fun <- function(x, group, beta_intercept, beta_x, beta_group, link) {
  inv_link(eta_fun(x, group, beta_intercept, beta_x, beta_group), link = link)
}

# ---------------------------------------------------------------------
# 2. Compact scenario table
# ---------------------------------------------------------------------
scenario_values <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  g <- expand.grid(x = settings$x_summary_values, group_num = c(0, 1))
  g$scenario <- s$scenario
  g$generating_link <- s$generating_link
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$linear_predictor <- eta_fun(g$x, g$group_num, s$beta_intercept, s$beta_x, s$beta_group)
  g$expected_probability <- inv_link(g$linear_predictor, link = s$generating_link)
  g[, c("scenario", "generating_link", "x", "group", "linear_predictor", "expected_probability")]
}))

scenario_contrasts <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  x_low <- min(settings$x_summary_values)
  x_high <- max(settings$x_summary_values)
  p00 <- p_fun(x_low, 0, s$beta_intercept, s$beta_x, s$beta_group, s$generating_link)
  p01 <- p_fun(x_low, 1, s$beta_intercept, s$beta_x, s$beta_group, s$generating_link)
  p10 <- p_fun(x_high, 0, s$beta_intercept, s$beta_x, s$beta_group, s$generating_link)
  p11 <- p_fun(x_high, 1, s$beta_intercept, s$beta_x, s$beta_group, s$generating_link)
  data.frame(
    scenario = s$scenario,
    generating_link = s$generating_link,
    contrast = c(
      "Group difference at low x: Group 1 minus Group 0",
      "Group difference at high x: Group 1 minus Group 0",
      "x-related change in Group 0: high x minus low x",
      "x-related change in Group 1: high x minus low x",
      "Change in group difference from low x to high x",
      "Generating link-scale x-by-group product term"
    ),
    value_probability_points = c(
      group_difference(p00, p01),
      group_difference(p10, p11),
      p10 - p00,
      p11 - p01,
      change_in_group_difference(p00, p01, p10, p11),
      NA_real_
    ),
    link_scale_value = c(NA, NA, NA, NA, NA, settings$beta_x_group),
    stringsAsFactors = FALSE
  )
}))

scenario_table <- rbind(
  data.frame(table_part = "implied_values", scenario_values, contrast = NA_character_,
             value_probability_points = NA_real_, link_scale_value = NA_real_, stringsAsFactors = FALSE),
  data.frame(table_part = "derived_contrasts", scenario = scenario_contrasts$scenario,
             generating_link = scenario_contrasts$generating_link, x = NA_real_, group = NA_character_,
             linear_predictor = NA_real_, expected_probability = NA_real_,
             contrast = scenario_contrasts$contrast,
             value_probability_points = scenario_contrasts$value_probability_points,
             link_scale_value = scenario_contrasts$link_scale_value,
             stringsAsFactors = FALSE)
)

utils::write.csv(scenario_table, "tables/scenario-table-within-family-links.csv", row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by each scenario")
print_compact(scenario_contrasts)
report_sign_convention(paste0("x = ", min(settings$x_summary_values)), paste0("x = ", max(settings$x_summary_values)))

# ---------------------------------------------------------------------
# 3. Deterministic scenario plotting data
# ---------------------------------------------------------------------
x_seq <- seq(settings$x_plot_range[1], settings$x_plot_range[2], length.out = 240)
plot_grid <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  g <- expand.grid(x = x_seq, group_num = c(0, 1))
  g$scenario <- s$scenario
  g$generating_link <- s$generating_link
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$eta <- eta_fun(g$x, g$group_num, s$beta_intercept, s$beta_x, s$beta_group)
  g$expected_probability <- inv_link(g$eta, link = s$generating_link)
  g
}))

gap_data <- do.call(rbind, lapply(split(plot_grid, plot_grid$scenario), function(dat) {
  d0 <- dat[dat$group_num == 0, c("scenario", "x", "expected_probability")]
  d1 <- dat[dat$group_num == 1, c("scenario", "x", "expected_probability")]
  data.frame(
    scenario = d0$scenario,
    x = d0$x,
    group_difference_probability_points = group_difference(d0$expected_probability, d1$expected_probability),
    stringsAsFactors = FALSE
  )
}))

link_shape_data <- do.call(rbind, lapply(candidate_links, function(link) {
  eta <- seq(-5, 5, length.out = 400)
  data.frame(link = link, eta = eta, probability = inv_link(eta, link = link), stringsAsFactors = FALSE)
}))

eta_ranges <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  data.frame(
    scenario = s$scenario,
    eta_min = min(plot_grid$eta[plot_grid$scenario == s$scenario]),
    eta_max = max(plot_grid$eta[plot_grid$scenario == s$scenario]),
    stringsAsFactors = FALSE
  )
}))

# ---------------------------------------------------------------------
# 4. Simulate one example dataset per scenario
# ---------------------------------------------------------------------
simulate_one <- function(beta_intercept, beta_x, beta_group, generating_link) {
  x <- stats::rnorm(settings$N, 0, 1)
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  eta <- eta_fun(x, group_num, beta_intercept, beta_x, beta_group)
  p <- inv_link(eta, link = generating_link)
  y <- stats::rbinom(settings$N, 1, p)
  data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    eta = eta,
    p = p,
    y = y
  )
}

example_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  d <- simulate_one(scenarios$beta_intercept[i], scenarios$beta_x[i], scenarios$beta_group[i], scenarios$generating_link[i])
  d$scenario <- scenarios$scenario[i]
  d
}))

# Binned data.
breaks <- seq(settings$x_plot_range[1], settings$x_plot_range[2], length.out = 11)
example_data$x_bin <- cut(example_data$x, breaks = breaks, include.lowest = TRUE)
example_binned <- aggregate(y ~ scenario + x_bin + group, example_data, mean)
example_binned$x_mid <- bin_midpoints(example_binned$x_bin)

report_section("One example dataset per scenario")
cat("Observed mean outcome by scenario and group:\n")
print_compact(aggregate(y ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 5. Model fitting helpers
# ---------------------------------------------------------------------
fit_models <- function(d) {
  out <- lapply(candidate_links, function(link) {
    try(stats::glm(y ~ x * group, family = stats::binomial(link), data = d), silent = TRUE)
  })
  names(out) <- candidate_links
  out
}

model_did <- function(fit) {
  if (inherits(fit, "try-error")) return(NA_real_)
  nd <- expand.grid(
    x = c(-1, 1),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  pred <- stats::predict(fit, newdata = nd, type = "response")
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
    d <- simulate_one(s$beta_intercept, s$beta_x, s$beta_group, s$generating_link)
    fits <- fit_models(d)
    do.call(rbind, lapply(candidate_links, function(link) {
      fit <- fits[[link]]
      did <- model_did(fit)
      data.frame(
        scenario = s$scenario,
        generating_link = s$generating_link,
        fitted_link = link,
        p_value = if (inherits(fit, "try-error")) NA_real_ else interaction_p_from_glm(fit),
        interaction_coef = if (inherits(fit, "try-error")) NA_real_ else interaction_coef_from_glm(fit),
        change_in_group_difference_response_scale = did,
        change_in_group_difference_outcome_units = did,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

simulation_summary <- do.call(rbind, lapply(split(simulation_results, list(simulation_results$scenario, simulation_results$fitted_link), drop = TRUE), function(dat) {
  sm <- summarise_model_simulation(dat, alpha = settings$alpha)
  data.frame(scenario = dat$scenario[1], generating_link = dat$generating_link[1], fitted_link = dat$fitted_link[1], sm, stringsAsFactors = FALSE)
}))
simulation_summary <- simulation_summary[order(simulation_summary$scenario, simulation_summary$fitted_link), ]

utils::write.csv(simulation_summary, "tables/simulation-summary-within-family-links.csv", row.names = FALSE)

report_section("Simulation summary")
print_compact(simulation_summary)
cat("\nInterpretation aid: median_change_in_group_difference_response_scale is a contrast, not a probability.\n")
cat("It is the fitted model's implied change in the predicted group difference from x = ",
    min(settings$x_summary_values), " to x = ", max(settings$x_summary_values), ",\n", sep = "")
cat("expressed in probability-point units.\n")

# ---------------------------------------------------------------------
# 7. Figure panels
# ---------------------------------------------------------------------
pA <- ggplot(link_shape_data, aes(eta, probability, linetype = link)) +
  geom_rect(data = eta_ranges, aes(xmin = eta_min, xmax = eta_max, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, alpha = .10, fill = "grey70") +
  geom_line(linewidth = .95) +
  facet_wrap(~ scenario) +
  labs(title = "A. Candidate inverse links", subtitle = "Shaded regions show the eta ranges occupied by each scenario", x = "Linear predictor eta", y = "Probability") +
  link_theme()

pB <- ggplot(plot_grid, aes(x, expected_probability, linetype = group)) +
  geom_line(linewidth = .95) +
  facet_wrap(~ scenario) +
  labs(title = "B. True scenario curves", subtitle = "The true x-by-group term on the generating link scale is zero", x = "Continuous predictor x", y = "Expected probability") +
  link_theme()

pC <- ggplot(simulation_summary, aes(x = fitted_link, y = false_positive_rate)) +
  geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(title = "C. False-positive interaction rate", subtitle = "Dashed line is nominal alpha", x = "Fitted link", y = "Rate") +
  link_theme(base_size = 9)

save_plot_grid(
  list(pA, pB, pC),
  filename_base = "paper/figs/fig4-within-family-links",
  width = figure_width,
  height = 7.2,
  ncol = 1,
  dpi = default_dpi
)

# Extra inspection plot: model-implied change in group difference.
p_effect <- ggplot(simulation_summary, aes(x = fitted_link, y = median_change_in_group_difference_response_scale)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(
    title = "Inspection: median model-implied change in the group difference",
    subtitle = "Values are contrasts in probability-point units",
    x = "Fitted link",
    y = axis_title_change_group_gap_probability(min(settings$x_summary_values), max(settings$x_summary_values))
  ) +
  link_theme()
save_single_plot(p_effect, "outputs/inspection/within-family-effect-size-inspection", width = 8, height = 4.5)

p_gap <- ggplot(gap_data, aes(x, group_difference_probability_points)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = .95) +
  facet_wrap(~ scenario) +
  labs(
    title = "Inspection: true predicted group difference at each x",
    subtitle = "This is Group 1 minus Group 0 on the observed probability scale",
    x = "Continuous predictor x",
    y = axis_title_group_gap_probability()
  ) +
  link_theme()
save_single_plot(p_gap, "outputs/inspection/within-family-group-gap-inspection", width = 8, height = 4.5)

saveRDS(
  list(
    settings = settings,
    scenarios = scenarios,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    link_shape_data = link_shape_data,
    eta_ranges = eta_ranges,
    example_dataset = example_data,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = "outputs/simulation-within-family-links.rds"
)

report_section("Saved files")
cat("- tables/scenario-table-within-family-links.csv\n")
cat("- tables/simulation-summary-within-family-links.csv\n")
cat("- paper/figs/fig4-within-family-links.pdf/png\n")
cat("- outputs/inspection/within-family-effect-size-inspection.pdf/png\n")
cat("- outputs/inspection/within-family-group-gap-inspection.pdf/png\n")
cat("- outputs/simulation-within-family-links.rds\n")
cat("\nDone.\n")
