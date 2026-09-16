# scripts/00-sandbox-one-scenario.R
# Single-scenario sandbox for rapid tuning.
#
# Purpose:
#   Edit one scenario, inspect the implied response-scale pattern,
#   simulate one dataset, and compare quick interaction estimates under
#   identity, standard binomial links, and a chance-corrected binomial link.
#
# Design principle:
#   Settings, scientific calculations, model fits, and plots are all local.

rm(list = ls())
library(ggplot2)

# Run from the repository root. This sandbox draws one dataset.
default_dpi <- 300

for (path in c("tables", "figs", "outputs", "outputs/inspection")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
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
  generating_link = "logit", # Label for the explicit DGP below.
  chance_fit_link = "logit" # Label for the explicit likelihood below.
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
  age_c <- age - scenario$age_center
  eta <- scenario$beta_intercept + scenario$beta_age * age_c +
    scenario$beta_group * group_num + scenario$beta_age_group * age_c * group_num
  scenario$chance + (1 - scenario$chance) * stats::plogis(eta)
}

# ---------------------------------------------------------------------
# Report current scenario
# ---------------------------------------------------------------------
cat("\n", "Sandbox scenario", "\n")

cat("\n", "Current sandbox parameters", "\n")
print(scenario)

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
scenario_grid$group <- factor(scenario_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
scenario_grid$linear_predictor <- eta_fun(scenario_grid$age, scenario_grid$group_num)
scenario_grid$expected_accuracy <- p_fun(scenario_grid$age, scenario_grid$group_num)
scenario_grid$expected_correct_out_of_k_trials <-
  scenario_grid$expected_accuracy * scenario$k_trials

cat("\n", "Implied values", "\n")
print(
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
    (p_low_g1 - p_low_g0),
    (p_high_g1 - p_high_g0),
    p_high_g0 - p_low_g0,
    p_high_g1 - p_low_g1,
    ((p_high_g1) - (p_high_g0)) - ((p_low_g1) - (p_low_g0)),
    NA_real_
  ),
  link_scale_value = c(NA, NA, NA, NA, NA, scenario$beta_age_group),
  stringsAsFactors = FALSE
)
contrasts$value_correct_out_of_k_trials <-
  contrasts$value_probability_points * scenario$k_trials

cat("\n", "Derived contrasts", "\n")
print(contrasts)
cat("\nGroup gaps are Group 1 minus Group 0; changes are high minus low.\n")

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
  group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
  group_num = group_num,
  y = y,
  k = scenario$k_trials,
  accuracy = y / scenario$k_trials
)

cat("\n", "Observed data summary", "\n")
print(stats::aggregate(accuracy ~ group, data = sim_data, FUN = mean))

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
d <- sim_data
settings <- scenario
# Chance-corrected binomial logit: likelihood, three starts, and Wald test.
X <- stats::model.matrix(~ age_c * group, data = d)
y <- d$y
k <- d$k
chance <- settings$chance
nll <- function(beta) {
  eta <- drop(X %*% beta)
  p <- chance + (1 - chance) * stats::plogis(eta)
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
}
gradient <- function(beta) {
  eta <- drop(X %*% beta)
  q <- stats::plogis(eta)
  p <- chance + (1 - chance) * q
  p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
  weight <- ((y - k * p) / (p * (1 - p))) * (1 - chance) * q * (1 - q)
  -drop(crossprod(X, weight))
}
zero <- stats::setNames(rep(0, ncol(X)), colnames(X))
from_standard <- zero
start_fit <- try(stats::glm.fit(X, cbind(y, k - y),
    family = stats::binomial("logit")), silent = TRUE)
if (!inherits(start_fit, "try-error") && all(is.finite(stats::coef(start_fit)))) {
  from_standard[] <- stats::coef(start_fit)
}
above <- (y / k - chance) / (1 - chance)
above <- pmin(pmax(above, 0.02), 0.98)
from_above <- zero
above_fit <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
if (!inherits(above_fit, "try-error") && all(is.finite(stats::coef(above_fit)))) {
  from_above[] <- stats::coef(above_fit)
}
candidates <- list()
for (start in list(zero, from_standard, from_above)) {
  candidate <- try(stats::optim(start, nll, gr = gradient, method = "BFGS",
      control = list(maxit = 1500, reltol = 1e-10)),
    silent = TRUE)
  if (!inherits(candidate, "try-error") && is.finite(candidate$value) &&
      all(is.finite(candidate$par))) {
    candidates[[length(candidates) + 1L]] <- candidate
  }
}
chance_coef <- chance_se <- chance_p <- rep(NA_real_, ncol(X))
chance_vcov <- matrix(NA_real_, ncol(X), ncol(X))
chance_usable <- FALSE
chance_nll <- NA_real_
if (length(candidates)) {
  best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "value"))]]
  chance_coef <- best$par
  chance_nll <- best$value
  hessian <- try(stats::optimHess(best$par, nll, gr = gradient), silent = TRUE)
  well_conditioned <- FALSE
  if (!inherits(hessian, "try-error") && all(is.finite(hessian))) {
    hess_sym <- (hessian + t(hessian)) / 2
    eig <- try(eigen(hess_sym, symmetric = TRUE, only.values = TRUE)$values,
      silent = TRUE)
    if (!inherits(eig, "try-error")) {
      well_conditioned <- all(is.finite(eig)) && min(eig) > 1e-7 &&
        min(eig) / max(eig) > sqrt(.Machine$double.eps)
    }
    if (well_conditioned) {
      V <- try(solve(hess_sym), silent = TRUE)
      if (!inherits(V, "try-error") && all(is.finite(V))) chance_vcov <- V
    }
  }
  variances <- diag(chance_vcov)
  variances[!is.finite(variances) | variances <= 0] <- NA_real_
  chance_se <- sqrt(variances)
  z <- chance_coef / chance_se
  chance_p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  chance_usable <- isTRUE(best$convergence == 0) && well_conditioned &&
    all(is.finite(chance_vcov)) && all(is.finite(chance_se)) && all(is.finite(chance_p))
  if (!chance_usable) chance_p[] <- NA_real_
}
names(chance_coef) <- names(chance_se) <- names(chance_p) <- colnames(X)

quick_results <- data.frame(
  model = c(
    "Identity",
    "Standard logit",
    "Standard probit",
    paste0("Chance-corrected ", scenario$chance_fit_link)
  ),
  interaction_coef = c(
    unname(stats::coef(fit_identity)["age_c:groupGroup 1"]),
    unname(stats::coef(fit_logit)["age_c:groupGroup 1"]),
    unname(stats::coef(fit_probit)["age_c:groupGroup 1"]),
    unname(chance_coef["age_c:groupGroup 1"])
  ),
  p_value = c(
    unname(summary(fit_identity)$coefficients["age_c:groupGroup 1", 4]),
    unname(summary(fit_logit)$coefficients["age_c:groupGroup 1", 4]),
    unname(summary(fit_probit)$coefficients["age_c:groupGroup 1", 4]),
    unname(chance_p["age_c:groupGroup 1"])
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

predictions <- cbind(
  stats::predict(fit_identity, newdata = contrast_grid),
  stats::predict(fit_logit, newdata = contrast_grid, type = "response"),
  stats::predict(fit_probit, newdata = contrast_grid, type = "response"),
  scenario$chance + (1 - scenario$chance) * stats::plogis(
    drop(stats::model.matrix(~ age_c * group, contrast_grid) %*% chance_coef)))
quick_results$change_in_group_difference_probability_points <-
  predictions[4, ] - predictions[2, ] - predictions[3, ] + predictions[1, ]
quick_results$change_in_group_difference_correct_out_of_k_trials <-
  quick_results$change_in_group_difference_probability_points * scenario$k_trials

cat("\n", "Quick model results for this one dataset", "\n")
print(quick_results)
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
plot_grid$group <- factor(plot_grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
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
    predicted = scenario$chance + (1 - scenario$chance) * stats::plogis(drop(stats::model.matrix(~ age_c * group, plot_grid) %*% chance_coef))
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
  y = paste0("Change in group gap from ", (age_low), " to ", (age_high), ", correct responses out of ", (scenario$k_trials))
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

# Write the same panel layout to PDF and PNG.
plots <- list(p1, p2, p3, p4)
plot_columns <- 2
plot_rows <- ceiling(length(plots) / plot_columns)
dir.create(dirname(scenario$output_base), recursive = TRUE, showWarnings = FALSE)
for (plot_format in c("pdf", "png")) {
  if (plot_format == "pdf") {
    grDevices::pdf(paste0(scenario$output_base, ".pdf"), width = 10, height = 5.8)
  } else {
    grDevices::png(paste0(scenario$output_base, ".png"), width = 10, height = 5.8, units = "in", res = default_dpi)
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

cat("\n", "Saved files", "\n")
cat("- ", scenario$output_base, ".pdf/png\n", sep = "")
cat("- ", scenario$output_base, ".rds\n", sep = "")
cat("\nDone.\n")
