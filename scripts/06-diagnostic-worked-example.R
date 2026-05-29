# scripts/06-diagnostic-worked-example.R
# Diagnostic worked example: wrong-link false-positive interaction vs DHARMa detection.
#
# Purpose:
#   The data are generated with a .50 chance floor, but the fitted model is a
#   standard binomial GLM with a lower asymptote at 0. The script compares:
#     1. the false-positive rate for the age-by-group product term;
#     2. the rate at which four prespecified DHARMa residual diagnostics flag
#        the wrong-link problem;
#     3. a Pregibon-style added-term link check, retained only as a secondary
#        link-specific comparator.
#
# Design rule for this project:
#   Global technical defaults and reusable functions live in R/.
#   Scenario-specific parameters and output choices live in this script.

rm(list = ls())

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------

required_packages <- c("DHARMa", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running script 06: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source("R/project-settings.R")
source("R/utils-reporting.R")
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
  dharma_n_sim = as.integer(Sys.getenv("DHARMA_N_SIM", "250")),
  age_plot_n = 200,
  output_scenario_table = "tables/scenario-table-diagnostic-worked-example.csv",
  output_simulation_summary = "tables/simulation-summary-diagnostic-worked-example.csv",
  output_simulation_summary_paper = "tables/table6-diagnostic-worked-example.csv",
  output_figure_base = "paper/figs/fig5-diagnostic-worked-example",
  output_dharma_example = "outputs/inspection/dharma-diagnostic-example.pdf",
  output_rds = "outputs/diagnostic-worked-example.rds"
)

settings$age_summary_values <- c(
  settings$age_range[1],
  settings$age_center,
  settings$age_range[2]
)

settings$age_plot_values <- seq(
  settings$age_range[1],
  settings$age_range[2],
  length.out = settings$age_plot_n
)

validate_settings <- function(settings) {
  if (!is.list(settings)) stop("settings must be a list.", call. = FALSE)
  if (!is.finite(settings$N) || settings$N <= 0) stop("settings$N must be positive.", call. = FALSE)
  if (!is.finite(settings$k_trials) || settings$k_trials <= 0) stop("settings$k_trials must be positive.", call. = FALSE)
  if (!is.finite(settings$chance) || settings$chance < 0 || settings$chance >= 1) {
    stop("settings$chance must be in [0, 1).", call. = FALSE)
  }
  if (length(settings$age_range) != 2 || any(!is.finite(settings$age_range))) {
    stop("settings$age_range must contain two finite values.", call. = FALSE)
  }
  if (settings$age_range[1] >= settings$age_range[2]) {
    stop("settings$age_range must be increasing.", call. = FALSE)
  }
  if (settings$age_center < settings$age_range[1] || settings$age_center > settings$age_range[2]) {
    stop("settings$age_center should lie inside settings$age_range.", call. = FALSE)
  }
  if (!is.finite(settings$B) || settings$B <= 0) stop("settings$B must be positive.", call. = FALSE)
  if (!is.finite(settings$alpha) || settings$alpha <= 0 || settings$alpha >= 1) {
    stop("settings$alpha must be in (0, 1).", call. = FALSE)
  }
  if (!is.finite(settings$dharma_n_sim) || settings$dharma_n_sim <= 0) {
    stop("settings$dharma_n_sim must be positive.", call. = FALSE)
  }
  check_supported_link(settings$generating_link)
  check_supported_link(settings$fitted_link)
  invisible(settings)
}

validate_settings(settings)

# This folder is used only for a representative DHARMa inspection plot.
dir.create(dirname(settings$output_dharma_example), recursive = TRUE, showWarnings = FALSE)

report_section("Scenario parameters you can tune")
print_compact(list_to_table(settings))
cat("\nThis diagnostic example deliberately generates data with a .50 lower asymptote,\n")
cat("then fits a standard binomial-logit model whose lower asymptote is 0.\n")
cat("The main diagnostic question is whether DHARMa simulation-based residual checks\n")
cat("flag the wrong link as often as the wrong link produces a significant\n")
cat("age-by-group interaction. A Pregibon-style added-term link check is reported\n")
cat("only as a secondary, link-specific comparator.\n")

eta_fun <- function(age, group_num) {
  age_c <- age - settings$age_center
  settings$beta_intercept +
    settings$beta_age * age_c +
    settings$beta_group * group_num +
    settings$beta_age_group * age_c * group_num
}

p_fun <- function(age, group_num) {
  chance_linkinv(
    eta_fun(age, group_num),
    chance = settings$chance,
    link = settings$generating_link
  )
}

# ---------------------------------------------------------------------
# 2. Compact scenario table
# ---------------------------------------------------------------------
summary_grid <- expand.grid(
  age = settings$age_summary_values,
  group_num = c(0, 1)
)
summary_grid$group <- factor(
  summary_grid$group_num,
  levels = c(0, 1),
  labels = c("Group 0", "Group 1")
)
summary_grid$linear_predictor <- eta_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_accuracy <- p_fun(summary_grid$age, summary_grid$group_num)
summary_grid$expected_correct_out_of_k_trials <-
  summary_grid$expected_accuracy * settings$k_trials

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
  link_scale_value = c(NA, NA, NA, NA, NA, settings$beta_age_group),
  stringsAsFactors = FALSE
)

scenario_contrasts$value_correct_out_of_k_trials <-
  scenario_contrasts$value_probability_points * settings$k_trials

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
report_section("Derived contrasts implied by the scenario")
print_compact(scenario_contrasts)
report_sign_convention(
  paste0("age ", settings$age_range[1]),
  paste0("age ", settings$age_range[2])
)

# ---------------------------------------------------------------------
# 3. Simulation and diagnostic functions
# ---------------------------------------------------------------------
simulate_one <- function() {
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  p <- p_fun(age, group_num)
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

fit_wrong_link_model <- function(data) {
  stats::glm(
    cbind(y, k - y) ~ age_c * group,
    family = stats::binomial(settings$fitted_link),
    data = data
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

# DHARMa is the primary diagnostic family. We report four prespecified
# diagnostics separately: overall uniformity, dispersion, residual quantile
# patterns over fitted values, and residual quantile patterns over age.
# We do not combine them into a single omnibus p-value, because these checks
# are partly overlapping summaries of the same simulated residuals. The goal is
# descriptive and diagnostic: to evaluate which checks, if any, detect the
# wrong-link problem in the same simulations where the wrong link can produce
# a false-positive interaction.
dharma_diagnostics <- function(fit, data) {
  sim <- try(
    DHARMa::simulateResiduals(
      fittedModel = fit,
      n = settings$dharma_n_sim,
      plot = FALSE
    ),
    silent = TRUE
  )
  
  empty <- data.frame(
    dharma_uniformity_p = NA_real_,
    dharma_dispersion_p = NA_real_,
    dharma_quantile_fitted_p = NA_real_,
    dharma_quantile_age_p = NA_real_,
    stringsAsFactors = FALSE
  )
  
  if (inherits(sim, "try-error")) return(empty)
  
  pvals <- c(
    uniformity = safe_dharma_test(DHARMa::testUniformity(sim, plot = FALSE)),
    dispersion = safe_dharma_test(DHARMa::testDispersion(sim, plot = FALSE)),
    quantile_fitted = safe_dharma_test(DHARMa::testQuantiles(sim, plot = FALSE)),
    quantile_age = safe_dharma_test(
      DHARMa::testQuantiles(sim, predictor = data$age_c, plot = FALSE)
    )
  )
  
  data.frame(
    dharma_uniformity_p = unname(pvals["uniformity"]),
    dharma_dispersion_p = unname(pvals["dispersion"]),
    dharma_quantile_fitted_p = unname(pvals["quantile_fitted"]),
    dharma_quantile_age_p = unname(pvals["quantile_age"]),
    stringsAsFactors = FALSE
  )
}

# Secondary diagnostic. This is retained because it targets link adequacy more
# directly than generic residual checks, but it tests only a narrow added-term
# alternative and is not used as the primary diagnostic conclusion.
pregibon_added_term_p <- function(fit, data) {
  eta_hat <- stats::predict(fit, type = "link")
  data$.eta_hat_sq <- eta_hat^2
  
  fit2 <- try(
    stats::glm(
      cbind(y, k - y) ~ age_c * group + .eta_hat_sq,
      family = stats::binomial(settings$fitted_link),
      data = data
    ),
    silent = TRUE
  )
  
  if (inherits(fit2, "try-error")) return(NA_real_)
  
  sm <- stats::coef(summary(fit2))
  if (!".eta_hat_sq" %in% rownames(sm)) return(NA_real_)
  
  p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(p_col)) return(NA_real_)
  
  unname(sm[".eta_hat_sq", p_col])
}

run_replication <- function(rep_id) {
  progress_tick(rep_id, settings$B, label = "Diagnostic replications ")
  
  d <- simulate_one()
  fit <- try(fit_wrong_link_model(d), silent = TRUE)
  
  if (inherits(fit, "try-error")) {
    return(data.frame(
      interaction_p = NA_real_,
      interaction_coef = NA_real_,
      dharma_uniformity_p = NA_real_,
      dharma_dispersion_p = NA_real_,
      dharma_quantile_fitted_p = NA_real_,
      dharma_quantile_age_p = NA_real_,
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
}

# ---------------------------------------------------------------------
# 4. Repeated simulation
# ---------------------------------------------------------------------
report_section("Monte Carlo simulation")
cat("Running B = ", settings$B, " replications.\n", sep = "")

simulation_results <- do.call(
  rbind,
  lapply(seq_len(settings$B), run_replication)
)

summaries <- list(
  interaction = summarise_detection(
    simulation_results$interaction_p,
    alpha = settings$alpha
  ),
  dharma_uniformity = summarise_detection(
    simulation_results$dharma_uniformity_p,
    alpha = settings$alpha
  ),
  dharma_dispersion = summarise_detection(
    simulation_results$dharma_dispersion_p,
    alpha = settings$alpha
  ),
  dharma_quantile_fitted = summarise_detection(
    simulation_results$dharma_quantile_fitted_p,
    alpha = settings$alpha
  ),
  dharma_quantile_age = summarise_detection(
    simulation_results$dharma_quantile_age_p,
    alpha = settings$alpha
  ),
  pregibon = summarise_detection(
    simulation_results$pregibon_link_test_p,
    alpha = settings$alpha
  )
)

simulation_summary <- data.frame(
  quantity = c(
    "Wrong-link interaction",
    "DHARMa uniformity",
    "DHARMa dispersion",
    "DHARMa residual quantiles over fitted values",
    "DHARMa residual quantiles over age",
    "Pregibon-style added-term link check, secondary"
  ),
  diagnostic_family = c(
    "Interaction test",
    "DHARMa",
    "DHARMa",
    "DHARMa",
    "DHARMa",
    "Pregibon-style link check"
  ),
  n_successful_fits = c(
    summaries$interaction$n_successful_fits,
    summaries$dharma_uniformity$n_successful_fits,
    summaries$dharma_dispersion$n_successful_fits,
    summaries$dharma_quantile_fitted$n_successful_fits,
    summaries$dharma_quantile_age$n_successful_fits,
    summaries$pregibon$n_successful_fits
  ),
  n_significant = c(
    summaries$interaction$n_significant,
    summaries$dharma_uniformity$n_significant,
    summaries$dharma_dispersion$n_significant,
    summaries$dharma_quantile_fitted$n_significant,
    summaries$dharma_quantile_age$n_significant,
    summaries$pregibon$n_significant
  ),
  rate = c(
    summaries$interaction$rate,
    summaries$dharma_uniformity$rate,
    summaries$dharma_dispersion$rate,
    summaries$dharma_quantile_fitted$rate,
    summaries$dharma_quantile_age$rate,
    summaries$pregibon$rate
  ),
  ci_low = c(
    summaries$interaction$ci_low,
    summaries$dharma_uniformity$ci_low,
    summaries$dharma_dispersion$ci_low,
    summaries$dharma_quantile_fitted$ci_low,
    summaries$dharma_quantile_age$ci_low,
    summaries$pregibon$ci_low
  ),
  ci_high = c(
    summaries$interaction$ci_high,
    summaries$dharma_uniformity$ci_high,
    summaries$dharma_dispersion$ci_high,
    summaries$dharma_quantile_fitted$ci_high,
    summaries$dharma_quantile_age$ci_high,
    summaries$pregibon$ci_high
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(simulation_summary, settings$output_simulation_summary, row.names = FALSE)
utils::write.csv(simulation_summary, settings$output_simulation_summary_paper, row.names = FALSE)

report_section("Diagnostic summary")
print_compact(simulation_summary)
cat("\nInterpretation aid: the first row is the false-positive rate for the interaction under the wrong standard link.\n")
cat("The DHARMa rows report four prespecified residual diagnostics separately, without an omnibus combination.\n")
cat("The final row is a secondary, link-specific Pregibon-style added-term check.\n")

# ---------------------------------------------------------------------
# 5. Figure panels
# ---------------------------------------------------------------------
plot_grid <- expand.grid(
  age = settings$age_plot_values,
  group_num = c(0, 1)
)
plot_grid$group <- factor(
  plot_grid$group_num,
  levels = c(0, 1),
  labels = c("Group 0", "Group 1")
)
plot_grid$expected_accuracy <- p_fun(plot_grid$age, plot_grid$group_num)

expected_group0 <- plot_grid$expected_accuracy[plot_grid$group_num == 0]
expected_group1 <- plot_grid$expected_accuracy[plot_grid$group_num == 1]

gap_data <- data.frame(
  age = settings$age_plot_values,
  group_difference_correct_out_of_k_trials = group_difference(
    expected_group0,
    expected_group1
  ) * settings$k_trials
)

pA <- ggplot2::ggplot(
  plot_grid,
  ggplot2::aes(age, expected_accuracy, linetype = group)
) +
  ggplot2::geom_hline(yintercept = settings$chance, linetype = "dashed") +
  ggplot2::geom_line(linewidth = .95) +
  ggplot2::scale_y_continuous(limits = c(settings$chance - .02, 1.02)) +
  ggplot2::labs(
    title = "A. Scenario",
    subtitle = "Generated with a .50 floor; fitted with a standard logit",
    x = "Age",
    y = "Expected accuracy"
  ) +
  link_theme()

pB <- ggplot2::ggplot(
  gap_data,
  ggplot2::aes(age, group_difference_correct_out_of_k_trials)
) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_line(linewidth = .95) +
  ggplot2::labs(
    title = "B. True group gap on the observed scale",
    subtitle = "Group 1 minus Group 0",
    x = "Age",
    y = axis_title_group_gap_correct(settings$k_trials)
  ) +
  link_theme()

figure_summary <- simulation_summary[
  simulation_summary$quantity %in% c(
    "Wrong-link interaction",
    "DHARMa uniformity",
    "DHARMa dispersion",
    "DHARMa residual quantiles over fitted values",
    "DHARMa residual quantiles over age",
    "Pregibon-style added-term link check, secondary"
  ),
]

figure_summary$quantity <- factor(
  figure_summary$quantity,
  levels = c(
    "Pregibon-style added-term link check, secondary",
    "DHARMa residual quantiles over age",
    "DHARMa residual quantiles over fitted values",
    "DHARMa dispersion",
    "DHARMa uniformity",
    "Wrong-link interaction"
  )
)

pC <- ggplot2::ggplot(
  figure_summary,
  ggplot2::aes(x = quantity, y = rate)
) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high)) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "C. Interaction false positives and diagnostic detection",
    subtitle = "DHARMa checks are reported separately; dashed line is nominal alpha",
    x = NULL,
    y = "Rate"
  ) +
  link_theme()

save_plot_grid(
  list(pA, pB, pC),
  filename_base = settings$output_figure_base,
  width = figure_width,
  height = 7.0,
  ncol = 1,
  dpi = default_dpi
)

# Save one representative DHARMa diagnostic plot for inspection. This helps
# inspect what DHARMa is seeing in one simulated dataset.
example_data <- simulate_one()
example_fit <- fit_wrong_link_model(example_data)
example_sim <- DHARMa::simulateResiduals(
  fittedModel = example_fit,
  n = settings$dharma_n_sim,
  plot = FALSE
)

grDevices::pdf(settings$output_dharma_example, width = 7.2, height = 6.5)
old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
graphics::plot(example_sim, quantreg = TRUE)
DHARMa::plotResiduals(
  example_sim,
  form = example_data$age_c,
  quantreg = TRUE,
  main = "Residuals by age"
)
DHARMa::plotResiduals(
  example_sim,
  form = example_data$group,
  main = "Residuals by group"
)
graphics::par(old_par)
grDevices::dev.off()

saveRDS(
  list(
    settings = settings,
    scenario_table = scenario_table,
    scenario_plot_data = plot_grid,
    gap_data = gap_data,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary,
    example_data = example_data
  ),
  file = settings$output_rds
)

report_section("Saved files")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_simulation_summary, "\n", sep = "")
cat("- ", settings$output_simulation_summary_paper, "\n", sep = "")
cat("- ", settings$output_figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$output_dharma_example, "\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
