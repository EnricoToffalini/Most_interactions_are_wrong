# scripts/00-sandbox-one-scenario.R
# Single-scenario sandbox for rapid tuning.
# Edit the scenario block, run the script, inspect console output and the sandbox figure.

rm(list = ls())
if (!file.exists("R/simulation-settings.R") && file.exists("../R/simulation-settings.R")) setwd("..")

source("R/simulation-settings.R")
source("R/utils-link-functions.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260529)

report_header("Sandbox: one tunable forced-choice scenario")

# ---------------------------------------------------------------------
# Edit this block freely
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
  generating_link = "logit"
)

report_section("Current sandbox parameters")
print_compact(list_to_table(scenario))
cat("\nTuning guide:\n")
cat("- beta_intercept: lower = closer to the chance floor, higher = closer to the ceiling.\n")
cat("- beta_age: larger = steeper age trend.\n")
cat("- beta_group: more negative = larger disadvantage for Group 1.\n")
cat("- chance: .50 for 2-AFC, .25 for 4-AFC.\n")
cat("- beta_age_group: keep 0 to study pseudo-interactions from link curvature.\n")

eta_fun <- function(age, group) {
  age_c <- age - scenario$age_center
  scenario$beta_intercept + scenario$beta_age * age_c + scenario$beta_group * group + scenario$beta_age_group * age_c * group
}

p_fun <- function(age, group) {
  chance_linkinv(eta_fun(age, group), chance = scenario$chance, link = scenario$generating_link)
}

# Scenario table -------------------------------------------------------
sg <- expand.grid(age = age_values_for_summary, group_num = c(0, 1))
sg$group <- factor(sg$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
sg$linear_predictor <- eta_fun(sg$age, sg$group_num)
sg$expected_accuracy <- p_fun(sg$age, sg$group_num)
sg$expected_correct_out_of_k_trials <- sg$expected_accuracy * scenario$k_trials

report_section("Implied values")
print_compact(sg[, c("age", "group", "linear_predictor", "expected_accuracy", "expected_correct_out_of_k_trials")])

age_low <- scenario$age_range[1]
age_high <- scenario$age_range[2]
p00 <- p_fun(age_low, 0)
p01 <- p_fun(age_low, 1)
p10 <- p_fun(age_high, 0)
p11 <- p_fun(age_high, 1)
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
report_section("Derived contrasts")
print_compact(contrasts)
report_sign_convention(paste0("age ", age_low), paste0("age ", age_high))

# Simulate one dataset -------------------------------------------------
group_num <- stats::rbinom(scenario$N, 1, 0.5)
age <- stats::runif(scenario$N, scenario$age_range[1], scenario$age_range[2])
age_c <- age - scenario$age_center
p <- p_fun(age, group_num)
y <- stats::rbinom(scenario$N, size = scenario$k_trials, prob = p)
d <- data.frame(
  age = age,
  age_c = age_c,
  group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
  y = y,
  k = scenario$k_trials,
  accuracy = y / scenario$k_trials
)

report_section("Observed data summary")
print_compact(aggregate(accuracy ~ group, d, mean))

# Fit quick models -----------------------------------------------------
fit_identity <- stats::lm(accuracy ~ age_c * group, data = d)
fit_logit <- stats::glm(cbind(y, k - y) ~ age_c * group, family = stats::binomial("logit"), data = d)
fit_probit <- stats::glm(cbind(y, k - y) ~ age_c * group, family = stats::binomial("probit"), data = d)
fit_chance <- fit_chance_binom(~ age_c * group, data = d, y_col = "y", k_col = "k", chance = scenario$chance, link = "logit")

quick_results <- data.frame(
  model = c("Identity", "Standard logit", "Standard probit", "Chance-corrected logit"),
  interaction_coef = c(
    interaction_coef_from_lm(fit_identity), interaction_coef_from_glm(fit_logit),
    interaction_coef_from_glm(fit_probit), interaction_coef_from_chance(fit_chance)
  ),
  p_value = c(
    interaction_p_from_lm(fit_identity), interaction_p_from_glm(fit_logit),
    interaction_p_from_glm(fit_probit), interaction_p_from_chance(fit_chance)
  ),
  stringsAsFactors = FALSE
)

# Model-implied change in group difference for the same one dataset.
contrast_grid <- expand.grid(
  age = c(age_low, age_high),
  group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
)
contrast_grid$age_c <- contrast_grid$age - scenario$age_center
model_names <- quick_results$model
predict_model <- function(model_name, newdata) {
  if (model_name == "Identity") return(stats::predict(fit_identity, newdata = newdata))
  if (model_name == "Standard logit") return(stats::predict(fit_logit, newdata = newdata, type = "response"))
  if (model_name == "Standard probit") return(stats::predict(fit_probit, newdata = newdata, type = "response"))
  if (model_name == "Chance-corrected logit") return(predict_chance_binom(fit_chance, newdata = newdata, type = "response"))
  stop("Unknown model")
}
for (m in model_names) {
  pred <- predict_model(m, contrast_grid)
  p_low_g0 <- pred[contrast_grid$age == age_low & contrast_grid$group == "Group 0"]
  p_low_g1 <- pred[contrast_grid$age == age_low & contrast_grid$group == "Group 1"]
  p_high_g0 <- pred[contrast_grid$age == age_high & contrast_grid$group == "Group 0"]
  p_high_g1 <- pred[contrast_grid$age == age_high & contrast_grid$group == "Group 1"]
  quick_results$change_in_group_difference_probability_points[quick_results$model == m] <-
    change_in_group_difference(p_low_g0, p_low_g1, p_high_g0, p_high_g1)
  quick_results$change_in_group_difference_correct_out_of_k_trials[quick_results$model == m] <-
    quick_results$change_in_group_difference_probability_points[quick_results$model == m] * scenario$k_trials
}

report_section("Quick model results for this one dataset")
print_compact(quick_results)
cat("\nInterpretation aid: change_in_group_difference_correct_out_of_k_trials is a contrast, not an observed count.\n")
cat("It is the model-implied change in the predicted group difference from age ", age_low, " to age ", age_high, ",\n", sep = "")
cat("expressed as correct-response units out of ", scenario$k_trials, " trials.\n", sep = "")

# Plots ----------------------------------------------------------------
plot_grid <- expand.grid(age = seq(scenario$age_range[1], scenario$age_range[2], length.out = 200), group_num = c(0, 1))
plot_grid$group <- factor(plot_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
plot_grid$age_c <- plot_grid$age - scenario$age_center
plot_grid$expected_accuracy <- p_fun(plot_grid$age, plot_grid$group_num)

pred_long <- rbind(
  data.frame(model = "Identity", plot_grid, predicted = stats::predict(fit_identity, newdata = plot_grid)),
  data.frame(model = "Standard logit", plot_grid, predicted = stats::predict(fit_logit, newdata = plot_grid, type = "response")),
  data.frame(model = "Standard probit", plot_grid, predicted = stats::predict(fit_probit, newdata = plot_grid, type = "response")),
  data.frame(model = "Chance-corrected logit", plot_grid, predicted = predict_chance_binom(fit_chance, newdata = plot_grid, type = "response"))
)

p1 <- ggplot(plot_grid, aes(age, expected_accuracy, linetype = group)) +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(scenario$chance - .02, 1.02)) +
  labs(title = "A. True scenario", x = "Age", y = "Expected accuracy") +
  link_theme()

p2 <- ggplot() +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_point(data = d, aes(age, accuracy, shape = group), alpha = .25, size = .8) +
  scale_y_continuous(limits = c(scenario$chance - .02, 1.02)) +
  labs(title = "B. One simulated dataset", x = "Age", y = "Observed accuracy") +
  link_theme()

p3 <- ggplot(pred_long, aes(age, predicted, linetype = group)) +
  geom_hline(yintercept = scenario$chance, linetype = "dashed") +
  geom_line(linewidth = .8) +
  facet_wrap(~ model, ncol = 2) +
  scale_y_continuous(limits = c(scenario$chance - .08, 1.08)) +
  labs(title = "C. Fitted model curves", x = "Age", y = "Predicted accuracy") +
  link_theme(base_size = 9)


p4 <- ggplot(quick_results, aes(x = model, y = change_in_group_difference_correct_out_of_k_trials)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  coord_flip() +
  labs(
    title = "D. Model-implied change in the group difference",
    subtitle = "Values are contrasts, not possible observed counts",
    x = NULL,
    y = axis_title_change_group_gap_correct(age_low, age_high, scenario$k_trials)
  ) +
  link_theme()

save_plot_grid(
  list(p1, p2, p3, p4),
  filename_base = "outputs/inspection/sandbox-one-scenario",
  width = 10,
  height = 5.8,
  ncol = 2,
  dpi = default_dpi
)

saveRDS(
  list(
    scenario = scenario,
    implied_values = sg,
    contrasts = contrasts,
    data = d,
    quick_results = quick_results,
    predictions = pred_long
  ),
  file = "outputs/inspection/sandbox-one-scenario.rds"
)

report_section("Saved files")
cat("- outputs/inspection/sandbox-one-scenario.pdf/png\n")
cat("- outputs/inspection/sandbox-one-scenario.rds\n")
cat("\nDone.\n")
