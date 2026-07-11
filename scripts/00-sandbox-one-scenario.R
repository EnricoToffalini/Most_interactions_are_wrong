# scripts/00-sandbox-one-scenario.R
# Single-scenario sandbox for rapid tuning.
#
# Purpose:
#   Edit one scenario, inspect the implied response-scale pattern,
#   simulate one dataset, and compare quick interaction estimates under
#   identity, standard binomial links, and a chance-corrected binomial link.
#
# Design principle:
#   Scenario-specific values stay in this script. The R/ folder only provides
#   project defaults and reusable helper functions.

rm(list = ls())

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-link-functions.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260529)

# ---------------------------------------------------------------------
# User-tunable scenario block
# ---------------------------------------------------------------------
scenario <- list(
  name = "sandbox_chance_floor",
  N = 600,
  k_trials = 50,
  chance = 0.50,
  age_range = c(6, 10),
  age_center = 8,
  beta_intercept = 0.00,
  beta_age = 1.00,
  beta_group = -1.40,
  beta_age_group = 0.00,
  generating_link = "logit",
  chance_fit_link = "logit"
)

# Local values used only to summarize and plot this scenario.
# They are deliberately derived from the scenario instead of living in R/.
scenario$age_summary_values <- c(
  scenario$age_range[1],
  scenario$age_center,
  scenario$age_range[2]
)

scenario$age_plot_values <- seq(
  scenario$age_range[1],
  scenario$age_range[2],
  length.out = 200
)

scenario$output_base <- file.path("outputs", "inspection", "sandbox-one-scenario")

# ---------------------------------------------------------------------
# Local helper functions
# ---------------------------------------------------------------------
eta_fun <- function(age, group_num) {
  age_c <- age - scenario$age_center
  scenario$beta_intercept +
    scenario$beta_age * age_c +
    scenario$beta_group * group_num +
    scenario$beta_age_group * age_c * group_num
}

p_fun <- function(age, group_num) {
  chance_linkinv(
    eta_fun(age, group_num),
    chance = scenario$chance,
    link = scenario$generating_link
  )
}

make_group_factor <- function(group_num) {
  factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
}

extract_change_in_group_difference <- function(pred, grid, age_low, age_high) {
  p_low_g0 <- pred[grid$age == age_low & grid$group == "Group 0"]
  p_low_g1 <- pred[grid$age == age_low & grid$group == "Group 1"]
  p_high_g0 <- pred[grid$age == age_high & grid$group == "Group 0"]
  p_high_g1 <- pred[grid$age == age_high & grid$group == "Group 1"]
  
  change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1)
}

# ---------------------------------------------------------------------
# Report current scenario
# ---------------------------------------------------------------------
report_header("Sandbox scenario")

report_section("Current sandbox parameters")
print_compact(list_to_table(scenario))

cat("- beta_intercept: lower = closer to the chance floor, higher = closer to the ceiling.\n")
cat("- beta_age: larger = steeper age trend.\n")
cat("- beta_group: more negative = larger disadvantage for Group 1.\n")
cat("- chance: .50 for 2-AFC, .25 for 4-AFC.\n")
cat("- beta_age_group: keep 0 to study pseudo-interactions from link curvature.\n")
cat("- age_summary_values and age_plot_values are local to this script.\n")

# ---------------------------------------------------------------------
# Scenario-implied values
# ---------------------------------------------------------------------
scenario_grid <- expand.grid(
  age = scenario$age_summary_values,
  group_num = c(0, 1)
)
scenario_grid$group <- make_group_factor(scenario_grid$group_num)
scenario_grid$linear_predictor <- eta_fun(scenario_grid$age, scenario_grid$group_num)
scenario_grid$expected_accuracy <- p_fun(scenario_grid$age, scenario_grid$group_num)
scenario_grid$expected_correct_out_of_k_trials <-
  scenario_grid$expected_accuracy * scenario$k_trials

report_section("Implied values")
print_compact(
  scenario_grid[, c(
    "age", "group", "linear_predictor", "expected_accuracy",
    "expected_correct_out_of_k_trials"
  )]
)

age_low <- scenario$age_range[1]
age_high <- scenario$age_range[2]

p_low_g0 <- p_fun(age_low, 0)
p_low_g1 <- p_fun(age_low, 1)
p_high_g0 <- p_fun(age_high, 0)
p_high_g1 <- p_fun(age_high, 1)

contrasts <- data.frame(
  contrast = c(
    "Group difference at youngest age: Group 1 minus Group 0",
    "Group difference at oldest age: Group 1 minus Group 0",
    "Age-related change in Group 0: oldest minus youngest",
    "Age-related change in Group 1: oldest minus youngest",
    "Change in group difference from youngest to oldest age",
    "Generating link-scale age-by-group product term"
  ),
  value_probability_points = c(
    group_difference(p_low_g0, p_low_g1),
    group_difference(p_high_g0, p_high_g1),
    p_high_g0 - p_low_g0,
    p_high_g1 - p_low_g1,
    change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1),
    NA_real_
  ),
  link_scale_value = c(NA, NA, NA, NA, NA, scenario$beta_age_group),
  stringsAsFactors = FALSE
)
contrasts$value_correct_out_of_k_trials <-
  contrasts$value_probability_points * scenario$k_trials

report_section("Derived contrasts")
print_compact(contrasts)
report_sign_convention(paste0("age ", age_low), paste0("age ", age_high))

# ---------------------------------------------------------------------
# Simulate one dataset
# ---------------------------------------------------------------------
group_num <- stats::rbinom(scenario$N, 1, 0.5)
age <- stats::runif(scenario$N, scenario$age_range[1], scenario$age_range[2])
age_c <- age - scenario$age_center
p <- p_fun(age, group_num)
y <- stats::rbinom(scenario$N, size = scenario$k_trials, prob = p)

sim_data <- data.frame(
  age = age,
  age_c = age_c,
  group = make_group_factor(group_num),
  group_num = group_num,
  y = y,
  k = scenario$k_trials,
  accuracy = y / scenario$k_trials
)

report_section("Observed data summary")
print_compact(stats::aggregate(accuracy ~ group, data = sim_data, FUN = mean))

# ---------------------------------------------------------------------
# Fit quick comparison models
# ---------------------------------------------------------------------
fit_identity <- stats::lm(accuracy ~ age_c * group, data = sim_data)
fit_logit <- stats::glm(
  cbind(y, k - y) ~ age_c * group,
  family = stats::binomial("logit"),
  data = sim_data
)

fit_probit <- stats::glm(
  cbind(y, k - y) ~ age_c * group,
  family = stats::binomial("probit"),
  data = sim_data
)
fit_chance <- fit_chance_binom(
  ~ age_c * group,
  data = sim_data,
  y_col = "y",
  k_col = "k",
  chance = scenario$chance,
  link = scenario$chance_fit_link
)

quick_results <- data.frame(
  model = c(
    "Identity",
    "Standard logit",
    "Standard probit",
    paste0("Chance-corrected ", scenario$chance_fit_link)
  ),
  interaction_coef = c(
    interaction_coef_from_lm(fit_identity),
    interaction_coef_from_glm(fit_logit),
    interaction_coef_from_glm(fit_probit),
    interaction_coef_from_chance(fit_chance)
  ),
  p_value = c(
    interaction_p_from_lm(fit_identity),
    interaction_p_from_glm(fit_logit),
    interaction_p_from_glm(fit_probit),
    interaction_p_from_chance(fit_chance)
  ),
  stringsAsFactors = FALSE
)

quick_results$model <- factor(quick_results$model, levels = quick_results$model)

# Model-implied change in group difference for this one dataset.
contrast_grid <- expand.grid(
  age = c(age_low, age_high),
  group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
)
contrast_grid$age_c <- contrast_grid$age - scenario$age_center

predict_model <- function(model_name, newdata) {
  model_name <- as.character(model_name)
  
  if (model_name == "Identity") {
    return(stats::predict(fit_identity, newdata = newdata))
  }
  
  if (model_name == "Standard logit") {
    return(stats::predict(fit_logit, newdata = newdata, type = "response"))
  }
  
  if (model_name == "Standard probit") {
    return(stats::predict(fit_probit, newdata = newdata, type = "response"))
  }
  
  if (model_name == paste0("Chance-corrected ", scenario$chance_fit_link)) {
    return(predict_chance_binom(fit_chance, newdata = newdata, type = "response"))
  }
  
  stop("Unknown model: ", model_name, call. = FALSE)
}

quick_results$change_in_group_difference_probability_points <- NA_real_
quick_results$change_in_group_difference_correct_out_of_k_trials <- NA_real_

for (m in quick_results$model) {
  pred <- predict_model(m, contrast_grid)
  this_change <- extract_change_in_group_difference(pred, contrast_grid, age_low, age_high)
  
  quick_results$change_in_group_difference_probability_points[quick_results$model == m] <-
    this_change
  quick_results$change_in_group_difference_correct_out_of_k_trials[quick_results$model == m] <-
    this_change * scenario$k_trials
}

report_section("Quick model results for this one dataset")
print_compact(quick_results)
cat("\nInterpretation aid: change_in_group_difference_correct_out_of_k_trials is a contrast, not an observed count.\n")
cat(
  "It is the model-implied change in the predicted group difference from age ",
  age_low, " to age ", age_high, ",\n",
  sep = ""
)
cat("expressed as correct-response units out of ", scenario$k_trials, " trials.\n", sep = "")

# ---------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------
plot_grid <- expand.grid(
  age = scenario$age_plot_values,
  group_num = c(0, 1)
)
plot_grid$group <- make_group_factor(plot_grid$group_num)
plot_grid$age_c <- plot_grid$age - scenario$age_center
plot_grid$expected_accuracy <- p_fun(plot_grid$age, plot_grid$group_num)

pred_long <- rbind(
  data.frame(
    model = "Identity",
    plot_grid,
    predicted = stats::predict(fit_identity, newdata = plot_grid)
  ),
  data.frame(
    model = "Standard logit",
    plot_grid,
    predicted = stats::predict(fit_logit, newdata = plot_grid, type = "response")
  ),
  data.frame(
    model = "Standard probit",
    plot_grid,
    predicted = stats::predict(fit_probit, newdata = plot_grid, type = "response")
  ),
  data.frame(
    model = paste0("Chance-corrected ", scenario$chance_fit_link),
    plot_grid,
    predicted = predict_chance_binom(fit_chance, newdata = plot_grid, type = "response")
  )
)

pred_long$model <- factor(pred_long$model, levels = levels(quick_results$model))

y_low <- max(0, scenario$chance - 0.08)
y_high <- 1.02

p1 <- ggplot2::ggplot(
  plot_grid,
  ggplot2::aes(x = age, y = expected_accuracy, linetype = group)
) +
  ggplot2::geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::coord_cartesian(ylim = c(y_low, y_high)) +
  ggplot2::labs(
    title = "A. True scenario",
    x = "Age",
    y = "Expected accuracy"
  ) +
  link_theme()

p2 <- ggplot2::ggplot() +
  ggplot2::geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  ggplot2::geom_point(
    data = sim_data,
    ggplot2::aes(x = age, y = accuracy, shape = group),
    alpha = 0.25,
    size = 0.8
  ) +
  ggplot2::coord_cartesian(ylim = c(y_low, y_high)) +
  ggplot2::labs(
    title = "B. One simulated dataset",
    x = "Age",
    y = "Observed accuracy"
  ) +
  link_theme()

p3 <- ggplot2::ggplot(
  pred_long,
  ggplot2::aes(x = age, y = predicted, linetype = group)
) +
  ggplot2::geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::facet_wrap(~ model, ncol = 2) +
  ggplot2::coord_cartesian(ylim = c(y_low, y_high)) +
  ggplot2::labs(
    title = "C. Fitted model curves",
    x = "Age",
    y = "Predicted accuracy"
  ) +
  link_theme(base_size = 9)

p4 <- ggplot2::ggplot(
  quick_results,
  ggplot2::aes(
    x = model,
    y = change_in_group_difference_correct_out_of_k_trials
  )
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "D. Model-implied change in the group difference",
    subtitle = "Values are contrasts, not possible observed counts",
    x = NULL,
    y = axis_title_change_group_gap_correct(age_low, age_high, scenario$k_trials)
  ) +
  link_theme()

save_plot_grid(
  list(p1, p2, p3, p4),
  filename_base = scenario$output_base,
  width = 10,
  height = 5.8,
  ncol = 2,
  dpi = default_dpi
)

saveRDS(
  list(
    scenario = scenario,
    implied_values = scenario_grid,
    contrasts = contrasts,
    data = sim_data,
    quick_results = quick_results,
    predictions = pred_long
  ),
  file = paste0(scenario$output_base, ".rds")
)

report_section("Saved files")
cat("- ", scenario$output_base, ".pdf/png\n", sep = "")
cat("- ", scenario$output_base, ".rds\n", sep = "")
cat("\nDone.\n")
