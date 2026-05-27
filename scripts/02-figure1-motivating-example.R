# scripts/02-figure1-motivating-example.R
# Motivating example: one 2-AFC dataset, explicit scenario visualization,
# and model-implied changes in the predicted group difference.

rm(list = ls())
if (!file.exists("R/simulation-settings.R") && file.exists("../R/simulation-settings.R")) setwd("..")

source("R/simulation-settings.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260524)

report_header("Figure 1 motivating example")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
scenario <- list(
  name = "2-AFC chance-floor motivating example",
  N = 1000,
  k_trials = 50,
  chance = 0.50,
  age_range = c(6, 10),
  age_center = 8,
  beta_intercept = 0.00,
  beta_age = 1.00,
  beta_group = -1.40,
  beta_age_group = 0.00,
  generating_link = "logit"
)

report_section("Scenario parameters you can tune")
print_compact(list_to_table(scenario))
cat("\nTuning guide:\n")
cat("- beta_intercept moves the whole scenario toward the floor or ceiling.\n")
cat("- beta_age controls the age trend on the generating link scale.\n")
cat("- beta_group controls the group gap on the generating link scale.\n")
cat("- chance = .50 gives a 2-AFC lower asymptote. Use .25 for 4-AFC.\n")
cat("- beta_age_group is fixed at 0 here, so the true link-scale interaction is absent.\n")

eta_fun <- function(age, group) {
  age_c <- age - scenario$age_center
  scenario$beta_intercept +
    scenario$beta_age * age_c +
    scenario$beta_group * group +
    scenario$beta_age_group * age_c * group
}

p_fun <- function(age, group) {
  chance_linkinv(eta_fun(age, group), chance = scenario$chance, link = scenario$generating_link)
}

# ---------------------------------------------------------------------
# 2. Scenario table: implied values and contrasts
# ---------------------------------------------------------------------
summary_grid <- expand.grid(
  age = age_values_for_summary,
  group_num = c(0, 1)
)
summary_grid$group <- factor(summary_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
summary_grid$eta <- eta_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_accuracy <- p_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_correct_out_of_k_trials <- summary_grid$expected_accuracy * scenario$k_trials

scenario_values <- data.frame(
  scenario = scenario$name,
  age = summary_grid$age,
  group = summary_grid$group,
  linear_predictor = summary_grid$eta,
  expected_accuracy = summary_grid$expected_accuracy,
  expected_correct_out_of_k_trials = summary_grid$expected_correct_out_of_k_trials,
  stringsAsFactors = FALSE
)

age_low <- scenario$age_range[1]
age_high <- scenario$age_range[2]
p00 <- p_fun(age_low, 0)
p01 <- p_fun(age_low, 1)
p10 <- p_fun(age_high, 0)
p11 <- p_fun(age_high, 1)
scenario_contrasts <- data.frame(
  scenario = scenario$name,
  contrast = c(
    "Group difference at youngest age: Group 1 minus Group 0",
    "Group difference at oldest age: Group 1 minus Group 0",
    "Age-related change in Group 0: oldest minus youngest",
    "Age-related change in Group 1: oldest minus youngest",
    "Change in group difference from youngest to oldest age",
    "Generating link-scale age-by-group product term"
  ),
  value_probability_points = c(
    group_difference(p00, p01),
    group_difference(p10, p11),
    p10 - p00,
    p11 - p01,
    change_in_group_difference(p00, p01, p10, p11),
    NA_real_
  ),
  value_correct_out_of_k_trials = c(
    group_difference(p00, p01),
    group_difference(p10, p11),
    p10 - p00,
    p11 - p01,
    change_in_group_difference(p00, p01, p10, p11),
    NA_real_
  ) * scenario$k_trials,
  link_scale_value = c(NA, NA, NA, NA, NA, scenario$beta_age_group),
  stringsAsFactors = FALSE
)

scenario_table <- rbind(
  data.frame(table_part = "implied_values", scenario_values, contrast = NA_character_,
             value_probability_points = NA_real_, value_correct_out_of_k_trials = NA_real_,
             link_scale_value = NA_real_, stringsAsFactors = FALSE),
  data.frame(table_part = "derived_contrasts", scenario = scenario_contrasts$scenario,
             age = NA_real_, group = NA_character_, linear_predictor = NA_real_,
             expected_accuracy = NA_real_, expected_correct_out_of_k_trials = NA_real_,
             contrast = scenario_contrasts$contrast,
             value_probability_points = scenario_contrasts$value_probability_points,
             value_correct_out_of_k_trials = scenario_contrasts$value_correct_out_of_k_trials,
             link_scale_value = scenario_contrasts$link_scale_value,
             stringsAsFactors = FALSE)
)

utils::write.csv(scenario_table, "tables/scenario-table-fig1-motivating-example.csv", row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by the scenario")
print_compact(scenario_contrasts)
report_sign_convention(paste0("age ", age_low), paste0("age ", age_high))

# ---------------------------------------------------------------------
# 3. Simulate one illustrative dataset
# ---------------------------------------------------------------------
group_num <- stats::rbinom(scenario$N, 1, 0.5)
age <- stats::runif(scenario$N, scenario$age_range[1], scenario$age_range[2])
age_c <- age - scenario$age_center
eta <- eta_fun(age, group_num)
prob <- chance_linkinv(eta, chance = scenario$chance, link = scenario$generating_link)
y <- stats::rbinom(scenario$N, size = scenario$k_trials, prob = prob)

d <- data.frame(
  age = age,
  age_c = age_c,
  group_num = group_num,
  group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
  y = y,
  k = scenario$k_trials,
  accuracy = y / scenario$k_trials
)

report_section("One simulated dataset")
cat("Observed mean accuracy by group:\n")
print_compact(aggregate(accuracy ~ group, d, mean))

# ---------------------------------------------------------------------
# 4. Fit candidate models
# ---------------------------------------------------------------------
fit_identity <- stats::lm(accuracy ~ age_c * group, data = d)
fit_logit <- stats::glm(cbind(y, k - y) ~ age_c * group, family = stats::binomial("logit"), data = d)
fit_probit <- stats::glm(cbind(y, k - y) ~ age_c * group, family = stats::binomial("probit"), data = d)
fit_chance <- fit_chance_binom(~ age_c * group, data = d, y_col = "y", k_col = "k",
                               chance = scenario$chance, link = "logit")

predict_model <- function(model_name, newdata) {
  if (model_name == "Identity") return(stats::predict(fit_identity, newdata = newdata))
  if (model_name == "Standard logit") return(stats::predict(fit_logit, newdata = newdata, type = "response"))
  if (model_name == "Standard probit") return(stats::predict(fit_probit, newdata = newdata, type = "response"))
  if (model_name == "Chance-corrected logit") return(predict_chance_binom(fit_chance, newdata = newdata, type = "response"))
  stop("Unknown model")
}

model_names <- c("Identity", "Standard logit", "Standard probit", "Chance-corrected logit")
plot_model_names <- c("Identity", "Standard logit", "Chance-corrected logit")

model_results <- data.frame(
  model = model_names,
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

# Model-implied change in the predicted group difference on the response scale.
contrast_grid <- expand.grid(
  age = c(age_low, age_high),
  group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
)
contrast_grid$age_c <- contrast_grid$age - scenario$age_center
for (m in model_names) {
  pred <- predict_model(m, contrast_grid)
  p_low_g0 <- pred[contrast_grid$age == age_low & contrast_grid$group == "Group 0"]
  p_low_g1 <- pred[contrast_grid$age == age_low & contrast_grid$group == "Group 1"]
  p_high_g0 <- pred[contrast_grid$age == age_high & contrast_grid$group == "Group 0"]
  p_high_g1 <- pred[contrast_grid$age == age_high & contrast_grid$group == "Group 1"]
  model_results$change_in_group_difference_probability_points[model_results$model == m] <-
    change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1)
  model_results$change_in_group_difference_correct_out_of_k_trials[model_results$model == m] <-
    model_results$change_in_group_difference_probability_points[model_results$model == m] * scenario$k_trials
}

utils::write.csv(model_results, "tables/model-results-fig1-motivating-example.csv", row.names = FALSE)

report_section("Model conclusions for the same dataset")
print_compact(model_results)
cat("\nInterpretation aid: change_in_group_difference_correct_out_of_k_trials is NOT an observed count.\n")
cat("It is the model-implied change in the predicted group difference from age ", age_low, " to age ", age_high, ",\n", sep = "")
cat("expressed as correct-response units out of ", scenario$k_trials, " trials.\n", sep = "")

# ---------------------------------------------------------------------
# 5. Figure panels
# ---------------------------------------------------------------------
plot_grid <- expand.grid(
  age = seq(scenario$age_range[1], scenario$age_range[2], length.out = 200),
  group_num = c(0, 1)
)
plot_grid$group <- factor(plot_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
plot_grid$age_c <- plot_grid$age - scenario$age_center
plot_grid$expected_accuracy <- p_fun(plot_grid$age, plot_grid$group_num)

pred_long <- do.call(rbind, lapply(plot_model_names, function(m) {
  data.frame(
    age = plot_grid$age,
    group = plot_grid$group,
    model = m,
    predicted = predict_model(m, plot_grid),
    stringsAsFactors = FALSE
  )
}))
pred_long$model <- factor(pred_long$model, levels = plot_model_names)

# Binned data for readability.
breaks <- seq(scenario$age_range[1], scenario$age_range[2], length.out = 9)
d$age_bin <- cut(d$age, breaks = breaks, include.lowest = TRUE)
binned <- aggregate(accuracy ~ age_bin + group, d, mean)
binned$age_mid <- bin_midpoints(binned$age_bin)

p1 <- ggplot(plot_grid, aes(age, expected_accuracy, linetype = group)) +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(scenario$chance - .02, 1.02)) +
  labs(title = "A. True scenario", subtitle = "No age-by-group term on the 2-AFC link scale", x = "Age", y = "Expected accuracy") +
  link_theme()

p2 <- ggplot() +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_point(data = d, aes(age, accuracy, shape = group), alpha = .20, size = .8) +
  geom_point(data = binned, aes(age_mid, accuracy, shape = group), size = 2.1) +
  scale_y_continuous(limits = c(scenario$chance - .02, 1.02)) +
  labs(title = "B. One simulated dataset", subtitle = "Small points are participants; larger points are age-bin means", x = "Age", y = "Observed accuracy") +
  link_theme()

p3 <- ggplot(pred_long, aes(age, predicted, linetype = group)) +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_line(linewidth = .9) +
  facet_wrap(~ model, ncol = 3) +
  scale_y_continuous(limits = c(scenario$chance - .04, 1.04)) +
  labs(title = "C. Fitted curves from three plausible models", subtitle = "Same data, different link assumptions", x = "Age", y = "Predicted accuracy") +
  link_theme(base_size = 9)

save_plot_grid(
  list(p1, p2, p3),
  filename_base = "paper/figs/fig1-motivating-example",
  width = figure_width,
  height = 7.2,
  ncol = 1,
  dpi = default_dpi
)

saveRDS(
  list(
    scenario_parameters = scenario,
    scenario_table = scenario_table,
    example_dataset = d,
    model_results_example = model_results,
    predictions = pred_long,
    fits = list(identity = fit_identity, logit = fit_logit, probit = fit_probit, chance_logit = fit_chance)
  ),
  file = "outputs/fig1-motivating-example.rds"
)

report_section("Saved files")
cat("- tables/scenario-table-fig1-motivating-example.csv\n")
cat("- tables/model-results-fig1-motivating-example.csv\n")
cat("- paper/figs/fig1-motivating-example.pdf/png\n")
cat("- outputs/fig1-motivating-example.rds\n")
cat("\nDone.\n")
