# scripts/03a-simulation-forced-choice.R
# Simulation 1: forced-choice accuracy with a non-zero chance floor.
#
# This script first shows the deterministic scenario, then simulates data,
# then quantifies nominal rejection on the known generating scale and
# pseudo-interaction detection on alternative fitted scales. Scenario-specific
# parameters stay here, not in R/.

rm(list = ls())

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------
source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260525)

report_header("Simulation 1: forced-choice accuracy with chance floor")

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
  generating_link = "logit",
  B = default_B,
  alpha = default_alpha,
  output_scenario_table = "tables/scenario-table-forced-choice.csv",
  output_summary_table = "tables/simulation-summary-forced-choice.csv",
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
settings$age_plot_values <- seq(settings$age_range[1], settings$age_range[2], length.out = 200)
settings$age_bin_breaks <- seq(settings$age_range[1], settings$age_range[2], length.out = 9)

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

report_section("Scenario parameters")
print_compact(list_to_table(settings))
cat("\nScenario-specific intercepts:\n")
print_compact(scenarios)

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
  chance_linkinv(
    eta_fun(age, group_num, beta_intercept),
    chance = settings$chance,
    link = settings$generating_link
  )
}

simulate_one <- function(beta_intercept) {
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  eta <- eta_fun(age, group_num, beta_intercept)
  p <- chance_linkinv(eta, chance = settings$chance, link = settings$generating_link)
  y <- stats::rbinom(settings$N, size = settings$k_trials, prob = p)
  
  data.frame(
    age = age,
    age_c = age_c,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    y = y,
    k = settings$k_trials,
    accuracy = y / settings$k_trials,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 3. Compact scenario table
# ---------------------------------------------------------------------
scenario_values <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  g <- expand.grid(
    age = settings$age_summary_values,
    group_num = c(0, 1)
  )
  g$scenario <- s$scenario
  g$group <- ifelse(g$group_num == 0, "Group 0", "Group 1")
  g$linear_predictor <- eta_fun(g$age, g$group_num, s$beta_intercept)
  g$expected_accuracy <- p_fun(g$age, g$group_num, s$beta_intercept)
  g$expected_correct_out_of_k_trials <- g$expected_accuracy * settings$k_trials
  
  g[, c(
    "scenario", "age", "group", "linear_predictor",
    "expected_accuracy", "expected_correct_out_of_k_trials"
  )]
}))

scenario_contrasts <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
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
    link_scale_value = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, settings$beta_age_group),
    stringsAsFactors = FALSE
  )
}))

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

utils::write.csv(scenario_table, settings$output_scenario_table, row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by each scenario")
print_compact(scenario_contrasts)
report_sign_convention(
  paste0("age ", settings$age_range[1]),
  paste0("age ", settings$age_range[2])
)

# ---------------------------------------------------------------------
# 4. Deterministic plotting data
# ---------------------------------------------------------------------
plot_grid <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  g <- expand.grid(
    age = settings$age_plot_values,
    group_num = c(0, 1)
  )
  g$scenario <- s$scenario
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$age_c <- g$age - settings$age_center
  g$eta <- eta_fun(g$age, g$group_num, s$beta_intercept)
  g$expected_accuracy <- chance_linkinv(g$eta, chance = settings$chance, link = settings$generating_link)
  g
}))
plot_grid$scenario <- factor(plot_grid$scenario, levels = scenarios$scenario)

# Predicted group difference over age, using Group 1 minus Group 0.
# This is retained for tables/RDS and for the inspection plot, but no longer
# shown as a main manuscript panel because it largely repeats Panel A.
gap_data <- do.call(rbind, lapply(split(plot_grid, plot_grid$scenario), function(dat) {
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
}))
gap_data$scenario <- factor(gap_data$scenario, levels = scenarios$scenario)

# ---------------------------------------------------------------------
# 5. One illustrative dataset per scenario
# ---------------------------------------------------------------------
example_data <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  d <- simulate_one(scenarios$beta_intercept[i])
  d$scenario <- scenarios$scenario[i]
  d
}))
example_data$scenario <- factor(example_data$scenario, levels = scenarios$scenario)

example_data$age_bin <- cut(
  example_data$age,
  breaks = settings$age_bin_breaks,
  include.lowest = TRUE
)
example_binned <- stats::aggregate(accuracy ~ scenario + age_bin + group, example_data, mean)
example_binned$age_mid <- bin_midpoints(example_binned$age_bin)

report_section("One example dataset per scenario")
cat("Observed mean accuracy by scenario and group:\n")
print_compact(stats::aggregate(accuracy ~ scenario + group, example_data, mean))

# ---------------------------------------------------------------------
# 6. Model-fitting helpers for one dataset
# ---------------------------------------------------------------------
fit_models <- function(d) {
  list(
    "Gaussian identity" = try(
      stats::lm(accuracy ~ age_c * group, data = d),
      silent = TRUE
    ),
    "Standard binomial logit" = try(
      stats::glm(
        cbind(y, k - y) ~ age_c * group,
        family = stats::binomial("logit"),
        data = d
      ),
      silent = TRUE
    ),
    "Standard binomial probit" = try(
      stats::glm(
        cbind(y, k - y) ~ age_c * group,
        family = stats::binomial("probit"),
        data = d
      ),
      silent = TRUE
    ),
    "Chance-corrected binomial logit" = try(
      fit_chance_binom(
        ~ age_c * group,
        data = d,
        y_col = "y",
        k_col = "k",
        chance = settings$chance,
        link = "logit"
      ),
      silent = TRUE
    )
  )
}

model_p <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name == "Gaussian identity") return(interaction_p_from_lm(fit))
  if (model_name == "Chance-corrected binomial logit") return(interaction_p_from_chance(fit))
  interaction_p_from_glm(fit)
}

model_coef <- function(fit, model_name) {
  if (inherits(fit, "try-error")) return(NA_real_)
  if (model_name == "Gaussian identity") return(interaction_coef_from_lm(fit))
  if (model_name == "Chance-corrected binomial logit") return(interaction_coef_from_chance(fit))
  interaction_coef_from_glm(fit)
}

model_predict <- function(fit, model_name, newdata) {
  if (inherits(fit, "try-error")) return(rep(NA_real_, nrow(newdata)))
  if (model_name == "Gaussian identity") return(stats::predict(fit, newdata = newdata))
  if (model_name == "Chance-corrected binomial logit") {
    return(predict_chance_binom(fit, newdata = newdata, type = "response"))
  }
  stats::predict(fit, newdata = newdata, type = "response")
}

model_did <- function(fit, model_name) {
  nd <- expand.grid(
    age = settings$age_range,
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  nd$age_c <- nd$age - settings$age_center
  
  pred <- model_predict(fit, model_name, nd)
  low <- settings$age_range[1]
  high <- settings$age_range[2]
  
  p_low_g0 <- pred[nd$age == low & nd$group == "Group 0"]
  p_low_g1 <- pred[nd$age == low & nd$group == "Group 1"]
  p_high_g0 <- pred[nd$age == high & nd$group == "Group 0"]
  p_high_g1 <- pred[nd$age == high & nd$group == "Group 1"]
  
  change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1)
}

run_replication <- function(scenario_label, beta_intercept) {
  d <- simulate_one(beta_intercept)
  fits <- fit_models(d)
  
  do.call(rbind, lapply(model_names, function(m) {
    fit <- fits[[m]]
    did <- model_did(fit, m)
    
    data.frame(
      scenario = scenario_label,
      model = m,
      p_value = model_p(fit, m),
      interaction_coef = model_coef(fit, m),
      change_in_group_difference_response_scale = did,
      change_in_group_difference_outcome_units = did * settings$k_trials,
      stringsAsFactors = FALSE
    )
  }))
}

# ---------------------------------------------------------------------
# 7. Repeated simulation
# ---------------------------------------------------------------------
report_section("Monte Carlo simulation")
cat("Running B = ", settings$B, " replications per scenario.\n", sep = "")
cat("This can be changed with Sys.setenv(N_SIM = '...') or by editing settings$B.\n")

simulation_results <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  cat("Scenario: ", s$scenario, "\n", sep = "")
  
  do.call(rbind, lapply(seq_len(settings$B), function(b) {
    progress_tick(b, settings$B, label = "  replication ")
    run_replication(s$scenario, s$beta_intercept)
  }))
}))

simulation_summary <- do.call(rbind, lapply(
  split(simulation_results, list(simulation_results$scenario, simulation_results$model), drop = TRUE),
  function(dat) {
    sm <- summarise_model_simulation(dat, alpha = settings$alpha)
    data.frame(
      scenario = dat$scenario[1],
      model = dat$model[1],
      sm,
      stringsAsFactors = FALSE
    )
  }
))
simulation_summary$scenario <- factor(simulation_summary$scenario, levels = scenarios$scenario)
simulation_summary$model <- factor(simulation_summary$model, levels = model_names)
simulation_summary$rate_type <- ifelse(
  as.character(simulation_summary$model) == "Chance-corrected binomial logit",
  "Nominal rejection rate",
  "Pseudo-interaction detection rate"
)
simulation_summary <- simulation_summary[order(simulation_summary$scenario, simulation_summary$model), ]

utils::write.csv(simulation_summary, settings$output_summary_table, row.names = FALSE)

report_section("Simulation summary")
print_compact(simulation_summary)
cat("\nThe generating age-by-group product term is zero on the chance-corrected logit scale.\n")
cat("The matching chance-corrected model supplies nominal rejection rates.\n")
cat("Models fitted on alternative scales supply pseudo-interaction detection rates.\n")
cat("\nInterpretation aid: median_change_in_group_difference_outcome_units is NOT an observed count.\n")
cat(
  "It is the median model-implied change in the predicted group difference from age ",
  settings$age_range[1], " to age ", settings$age_range[2], ",\n",
  sep = ""
)
cat("expressed as correct-response units out of ", settings$k_trials, " trials.\n", sep = "")

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
  yintercept = chance_linkinv(
    eta_grid,
    chance = settings$chance,
    link = settings$generating_link
  )
)

pA <- ggplot2::ggplot(plot_grid, ggplot2::aes(age, expected_accuracy, linetype = group, color = group)) +
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
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::coord_cartesian(ylim = c(settings$chance - 0.05, 1.00)) +
  ggplot2::scale_y_continuous(labels = percent_labels(1), breaks = seq(settings$chance, 1, by = 0.10)) +
  link_scale_color_discrete(name = NULL) +
  link_scale_linetype_discrete(name = NULL) +
  ggplot2::labs(
    title = "A. Scenario curves generated above a chance floor",
    subtitle = "Horizontal lines mark equal steps on the chance-corrected logit scale",
    x = "Age",
    y = "Expected accuracy",
    color = NULL,
    linetype = NULL
  ) +
  link_theme()+
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )

pB <- ggplot2::ggplot(simulation_summary, ggplot2::aes(x = model, y = rejection_rate, shape = model)) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed", color = "grey35") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high), linewidth = 0.45) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::scale_y_continuous(labels = percent_labels(1), breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  ggplot2::scale_shape_manual(values = c(
    "Gaussian identity" = 16,
    "Standard binomial logit" = 15,
    "Standard binomial probit" = 18,
    "Chance-corrected binomial logit" = 17
  )) +
  ggplot2::labs(
    title = "B. Product-term rejection rate",
    subtitle = "Matched generating-scale model: nominal rejection; alternative scales: pseudo-interaction detection. Dashed line: nominal alpha",
    x = NULL,
    y = "Replications rejecting the age-by-group product term",
    shape = NULL
  ) +
  link_theme(base_size = 9) +
  ggplot2::theme(legend.position = "none")

save_plot_grid(
  list(pA, pB),
  filename_base = settings$output_figure_base,
  width = figure_width,
  height = 5.9,
  ncol = 1,
  dpi = default_dpi
)

# Extra inspection plot: model-implied effect sizes.
p_effect <- ggplot2::ggplot(
  simulation_summary,
  ggplot2::aes(x = model, y = median_change_in_group_difference_outcome_units, shape = model)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ scenario) +
  ggplot2::scale_shape_manual(values = c(
    "Gaussian identity" = 16,
    "Standard binomial logit" = 15,
    "Standard binomial probit" = 18,
    "Chance-corrected binomial logit" = 17
  )) +
  ggplot2::labs(
    title = "Inspection: median model-implied change in the group difference",
    subtitle = "Values are contrasts, not possible observed counts",
    x = NULL,
    y = axis_title_change_group_gap_correct(
      settings$age_range[1],
      settings$age_range[2],
      settings$k_trials
    ),
    shape = NULL
  ) +
  link_theme() +
  ggplot2::theme(legend.position = "none")

save_single_plot(
  p_effect,
  settings$output_inspection_base,
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
    example_dataset = example_data,
    example_binned = example_binned,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = settings$output_rds
)

report_section("Saved files")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_summary_table, "\n", sep = "")
cat("- ", settings$output_figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_inspection_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
