# scripts/06-diagnostic-worked-example.R
# Diagnostic worked example: wrong-link false-positive interaction vs DHARMa detection.
#
# Main diagnostic strategy:
#   1. Fit a deliberately wrong standard binomial-logit model to data generated with
#      a known .50 chance floor.
#   2. Estimate the false-positive rate for the age-by-group product term.
#   3. Estimate how often prespecified DHARMa simulation-based residual diagnostics
#      flag the misspecification.
#   4. Include a Pregibon-style added-term link check only as a secondary,
#      link-specific comparator.

rm(list = ls())
if (!file.exists("R/simulation-settings.R") && file.exists("../R/simulation-settings.R")) setwd("..")

if (!requireNamespace("DHARMa", quietly = TRUE)) {
  stop("Please install DHARMa before running script 06. It is the primary diagnostic tool in this example.", call. = FALSE)
}

source("R/simulation-settings.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()
set.seed(20260528)

report_header("Diagnostic worked example")

# ---------------------------------------------------------------------
# 1. User-tunable scenario block
# ---------------------------------------------------------------------
settings <- list(
  N = 120,
  k_trials = 20,
  chance = 0.50,
  age_range = c(6, 10),
  age_center = 8,
  beta_intercept = 0.00,
  beta_age = 1.00,
  beta_group = -1.40,
  beta_age_group = 0.00,
  generating_link = "logit",
  fitted_link = "logit",
  B = default_B,
  alpha = default_alpha,
  dharma_n_sim = as.integer(Sys.getenv("DHARMA_N_SIM", "250"))
)

report_section("Scenario parameters you can tune")
print_compact(list_to_table(settings))
cat("\nThis diagnostic example deliberately generates data with a .50 lower asymptote,\n")
cat("then fits a standard binomial-logit model whose lower asymptote is 0.\n")
cat("The main diagnostic question is whether DHARMa simulation-based residual checks\n")
cat("flag the wrong link as often as the wrong link produces a significant\n")
cat("age-by-group interaction. A Pregibon-style added-term link check is reported\n")
cat("only as a secondary, link-specific comparator.\n")

eta_fun <- function(age, group) {
  age_c <- age - settings$age_center
  settings$beta_intercept +
    settings$beta_age * age_c +
    settings$beta_group * group +
    settings$beta_age_group * age_c * group
}

p_fun <- function(age, group) {
  chance_linkinv(eta_fun(age, group), chance = settings$chance, link = settings$generating_link)
}

# ---------------------------------------------------------------------
# 2. Compact scenario table
# ---------------------------------------------------------------------
summary_grid <- expand.grid(age = age_values_for_summary, group_num = c(0, 1))
summary_grid$group <- factor(summary_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
summary_grid$linear_predictor <- eta_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_accuracy <- p_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_correct_out_of_k_trials <- summary_grid$expected_accuracy * settings$k_trials

scenario_values <- data.frame(
  scenario = "Diagnostic chance-floor scenario",
  age = summary_grid$age,
  group = summary_grid$group,
  linear_predictor = summary_grid$linear_predictor,
  expected_accuracy = summary_grid$expected_accuracy,
  expected_correct_out_of_k_trials = summary_grid$expected_correct_out_of_k_trials,
  stringsAsFactors = FALSE
)

age_low <- settings$age_range[1]
age_high <- settings$age_range[2]
p00 <- p_fun(age_low, 0)
p01 <- p_fun(age_low, 1)
p10 <- p_fun(age_high, 0)
p11 <- p_fun(age_high, 1)
scenario_contrasts <- data.frame(
  scenario = "Diagnostic chance-floor scenario",
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
  ) * settings$k_trials,
  link_scale_value = c(NA, NA, NA, NA, NA, settings$beta_age_group),
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
utils::write.csv(scenario_table, "tables/scenario-table-diagnostic-worked-example.csv", row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_values)
report_section("Derived contrasts implied by the scenario")
print_compact(scenario_contrasts)
report_sign_convention(paste0("age ", settings$age_range[1]), paste0("age ", settings$age_range[2]))

# ---------------------------------------------------------------------
# 3. Simulation and diagnostic functions
# ---------------------------------------------------------------------
simulate_one <- function() {
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  eta <- eta_fun(age, group_num)
  p <- chance_linkinv(eta, chance = settings$chance, link = settings$generating_link)
  y <- stats::rbinom(settings$N, size = settings$k_trials, prob = p)
  data.frame(
    age = age,
    age_c = age_c,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    y = y,
    k = settings$k_trials,
    accuracy = y / settings$k_trials
  )
}

extract_p_value <- function(x) {
  if (inherits(x, "try-error")) return(NA_real_)
  if (!is.null(x$p.value) && length(x$p.value) == 1) return(unname(x$p.value))
  NA_real_
}

safe_dharma_test <- function(expr) {
  extract_p_value(try(expr, silent = TRUE))
}

# DHARMa is the primary diagnostic family. The omnibus flag is prespecified as
# the Holm-adjusted minimum across four checks: overall uniformity, dispersion,
# residual quantile patterns over fitted values, and residual quantile patterns
# over age. This makes the diagnostic closer to a practical model-checking
# workflow while avoiding an unadjusted "any test significant" rule.
dharma_diagnostics <- function(fit, data) {
  sim <- try(
    DHARMa::simulateResiduals(
      fittedModel = fit,
      n = settings$dharma_n_sim,
      plot = FALSE
    ),
    silent = TRUE
  )
  if (inherits(sim, "try-error")) {
    return(data.frame(
      dharma_uniformity_p = NA_real_,
      dharma_dispersion_p = NA_real_,
      dharma_quantile_fitted_p = NA_real_,
      dharma_quantile_age_p = NA_real_,
      dharma_omnibus_p = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  pvals <- c(
    uniformity = safe_dharma_test(DHARMa::testUniformity(sim, plot = FALSE)),
    dispersion = safe_dharma_test(DHARMa::testDispersion(sim, plot = FALSE)),
    quantile_fitted = safe_dharma_test(DHARMa::testQuantiles(sim, plot = FALSE)),
    quantile_age = safe_dharma_test(DHARMa::testQuantiles(sim, predictor = data$age_c, plot = FALSE))
  )
  pvals_adj <- stats::p.adjust(pvals[is.finite(pvals)], method = "holm")
  omnibus <- if (length(pvals_adj) == 0) NA_real_ else min(pvals_adj)

  data.frame(
    dharma_uniformity_p = unname(pvals["uniformity"]),
    dharma_dispersion_p = unname(pvals["dispersion"]),
    dharma_quantile_fitted_p = unname(pvals["quantile_fitted"]),
    dharma_quantile_age_p = unname(pvals["quantile_age"]),
    dharma_omnibus_p = omnibus,
    stringsAsFactors = FALSE
  )
}

# Secondary diagnostic. This is retained because it targets link adequacy more
# directly than a generic residual check, but it tests a narrow added-term
# alternative and is not used as the primary diagnostic conclusion.
pregibon_added_term_p <- function(fit, data) {
  eta_hat <- stats::predict(fit, type = "link")
  data$.eta_hat_sq <- eta_hat^2
  fit2 <- try(
    stats::glm(cbind(y, k - y) ~ age_c * group + .eta_hat_sq,
               family = stats::binomial(settings$fitted_link), data = data),
    silent = TRUE
  )
  if (inherits(fit2, "try-error")) return(NA_real_)
  sm <- coef(summary(fit2))
  if (!".eta_hat_sq" %in% rownames(sm)) return(NA_real_)
  sm[".eta_hat_sq", grep("Pr\\(", colnames(sm), value = TRUE)[1]]
}

# ---------------------------------------------------------------------
# 4. Repeated simulation
# ---------------------------------------------------------------------
report_section("Monte Carlo simulation")
cat("Running B = ", settings$B, " replications.\n", sep = "")

simulation_results <- do.call(rbind, lapply(seq_len(settings$B), function(b) {
  progress_tick(b, settings$B, label = "Diagnostic replications ")
  d <- simulate_one()
  fit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
                        family = stats::binomial(settings$fitted_link), data = d), silent = TRUE)
  if (inherits(fit, "try-error")) {
    return(data.frame(
      interaction_p = NA_real_,
      interaction_coef = NA_real_,
      dharma_uniformity_p = NA_real_,
      dharma_dispersion_p = NA_real_,
      dharma_quantile_fitted_p = NA_real_,
      dharma_quantile_age_p = NA_real_,
      dharma_omnibus_p = NA_real_,
      pregibon_link_test_p = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  cbind(
    data.frame(
      interaction_p = interaction_p_from_glm(fit),
      interaction_coef = interaction_coef_from_glm(fit),
      stringsAsFactors = FALSE
    ),
    dharma_diagnostics(fit, d),
    data.frame(
      pregibon_link_test_p = pregibon_added_term_p(fit, d),
      stringsAsFactors = FALSE
    )
  )
}))

fp <- summarise_detection(simulation_results$interaction_p, alpha = settings$alpha)
dharma_all <- summarise_detection(simulation_results$dharma_omnibus_p, alpha = settings$alpha)
dharma_uniform <- summarise_detection(simulation_results$dharma_uniformity_p, alpha = settings$alpha)
dharma_disp <- summarise_detection(simulation_results$dharma_dispersion_p, alpha = settings$alpha)
dharma_fit <- summarise_detection(simulation_results$dharma_quantile_fitted_p, alpha = settings$alpha)
dharma_age <- summarise_detection(simulation_results$dharma_quantile_age_p, alpha = settings$alpha)
preg <- summarise_detection(simulation_results$pregibon_link_test_p, alpha = settings$alpha)

simulation_summary <- data.frame(
  quantity = c(
    "Wrong-link interaction",
    "DHARMa residual checks, Holm omnibus",
    "DHARMa uniformity",
    "DHARMa dispersion",
    "DHARMa residual pattern over fitted values",
    "DHARMa residual pattern over age",
    "Pregibon-style added-term link check, secondary"
  ),
  n_successful_fits = c(
    fp$n_successful_fits,
    dharma_all$n_successful_fits,
    dharma_uniform$n_successful_fits,
    dharma_disp$n_successful_fits,
    dharma_fit$n_successful_fits,
    dharma_age$n_successful_fits,
    preg$n_successful_fits
  ),
  n_significant = c(
    fp$n_significant,
    dharma_all$n_significant,
    dharma_uniform$n_significant,
    dharma_disp$n_significant,
    dharma_fit$n_significant,
    dharma_age$n_significant,
    preg$n_significant
  ),
  rate = c(
    fp$rate,
    dharma_all$rate,
    dharma_uniform$rate,
    dharma_disp$rate,
    dharma_fit$rate,
    dharma_age$rate,
    preg$rate
  ),
  ci_low = c(
    fp$ci_low,
    dharma_all$ci_low,
    dharma_uniform$ci_low,
    dharma_disp$ci_low,
    dharma_fit$ci_low,
    dharma_age$ci_low,
    preg$ci_low
  ),
  ci_high = c(
    fp$ci_high,
    dharma_all$ci_high,
    dharma_uniform$ci_high,
    dharma_disp$ci_high,
    dharma_fit$ci_high,
    dharma_age$ci_high,
    preg$ci_high
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(simulation_summary, "tables/simulation-summary-diagnostic-worked-example.csv", row.names = FALSE)

report_section("Diagnostic summary")
print_compact(simulation_summary)
cat("\nInterpretation aid: the first row is the false-positive rate for the interaction under the wrong standard link.\n")
cat("The second row is the prespecified primary DHARMa diagnostic flag, using a Holm-adjusted omnibus p-value across four DHARMa residual checks.\n")
cat("The final row is a secondary, link-specific Pregibon-style added-term check.\n")

# ---------------------------------------------------------------------
# 5. Figure panels
# ---------------------------------------------------------------------
plot_grid <- expand.grid(
  age = seq(settings$age_range[1], settings$age_range[2], length.out = 200),
  group_num = c(0, 1)
)
plot_grid$group <- factor(plot_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
plot_grid$expected_accuracy <- p_fun(plot_grid$age, plot_grid$group_num)

gap_data <- data.frame(
  age = sort(unique(plot_grid$age)),
  group_difference_correct_out_of_k_trials = group_difference(
    plot_grid$expected_accuracy[plot_grid$group_num == 0],
    plot_grid$expected_accuracy[plot_grid$group_num == 1]
  ) * settings$k_trials
)

pA <- ggplot(plot_grid, aes(age, expected_accuracy, linetype = group)) +
  geom_hline(yintercept = settings$chance, linetype = "dashed") +
  geom_line(linewidth = .95) +
  scale_y_continuous(limits = c(settings$chance - .02, 1.02)) +
  labs(title = "A. Scenario", subtitle = "Generated with a .50 floor; fitted with a standard logit", x = "Age", y = "Expected accuracy") +
  link_theme()

pB <- ggplot(gap_data, aes(age, group_difference_correct_out_of_k_trials)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = .95) +
  labs(
    title = "B. True group gap on the observed scale",
    subtitle = "Group 1 minus Group 0",
    x = "Age",
    y = axis_title_group_gap_correct(settings$k_trials)
  ) +
  link_theme()

figure_summary <- simulation_summary[simulation_summary$quantity %in% c(
  "Wrong-link interaction",
  "DHARMa residual checks, Holm omnibus",
  "Pregibon-style added-term link check, secondary"
), ]
figure_summary$quantity <- factor(
  figure_summary$quantity,
  levels = c(
    "Pregibon-style added-term link check, secondary",
    "DHARMa residual checks, Holm omnibus",
    "Wrong-link interaction"
  )
)

pC <- ggplot(figure_summary, aes(x = quantity, y = rate)) +
  geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  coord_flip() +
  labs(title = "C. Interaction false positives and diagnostic detection", subtitle = "Dashed line is nominal alpha", x = NULL, y = "Rate") +
  link_theme()

save_plot_grid(
  list(pA, pB, pC),
  filename_base = "paper/figs/fig5-diagnostic-worked-example",
  width = figure_width,
  height = 7.0,
  ncol = 1,
  dpi = default_dpi
)

# Save one representative DHARMa diagnostic plot for inspection. This is not the
# main inferential figure; it helps authors inspect what DHARMa is seeing in one
# simulated dataset.
example_data <- simulate_one()
example_fit <- stats::glm(cbind(y, k - y) ~ age_c * group,
                          family = stats::binomial(settings$fitted_link), data = example_data)
example_sim <- DHARMa::simulateResiduals(
  fittedModel = example_fit,
  n = settings$dharma_n_sim,
  plot = FALSE
)
grDevices::pdf("outputs/inspection/dharma-diagnostic-example.pdf", width = 7.2, height = 6.5)
old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(example_sim, quantreg = TRUE)
DHARMa::plotResiduals(example_sim, form = example_data$age_c, quantreg = TRUE, main = "Residuals by age")
DHARMa::plotResiduals(example_sim, form = example_data$group, main = "Residuals by group")
graphics::par(old_par)
grDevices::dev.off()

saveRDS(
  list(
    settings = settings,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = "outputs/diagnostic-worked-example.rds"
)

report_section("Saved files")
cat("- tables/scenario-table-diagnostic-worked-example.csv\n")
cat("- tables/simulation-summary-diagnostic-worked-example.csv\n")
cat("- paper/figs/fig5-diagnostic-worked-example.pdf/png\n")
cat("- outputs/inspection/dharma-diagnostic-example.pdf\n")
cat("- outputs/diagnostic-worked-example.rds\n")
cat("\nDone.\n")
