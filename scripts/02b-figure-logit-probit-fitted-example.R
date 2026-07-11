# scripts/02b-figure-logit-probit-fitted-example.R
# Fitted logit vs probit curves on one simulated binomial dataset.
#
# Purpose:
#   Show that logit and probit fits can look almost interchangeable while still
#   implying a structured pointwise discrepancy in fitted probabilities.
#
# Output:
#   figs/logit-probit-fitted-example.pdf
#   figs/logit-probit-fitted-example.png
#   tables/model-results-logit-probit-fitted-example.csv
#   outputs/logit-probit-fitted-example.rds

rm(list = ls())

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-plots.R")

ensure_output_dirs()
report_header("Fitted logit vs probit example")

# ---------------------------------------------------------------------
# 1. User-tunable settings
# ---------------------------------------------------------------------

settings <- list(
  seed = 20260601,
  
  # Data-generating scenario.
  N = 1000,
  k_trials = 60,
  x_range = c(-2.5, 2.5),
  beta_intercept = 0.05,
  beta_x = 1.75,

  # Prediction grid.
  prediction_n = 500,

  # Plot tuning.
  point_alpha = 0.35,
  point_size = 1.20,
  line_width = 1.05,
  discrepancy_width = 0.90,
  diff_min_limit = 0.010,

  # Output.
  figure_width = 7.2,
  figure_height = 5.8,
  figure_base = "figs/logit-probit-fitted-example",
  table_path = "tables/model-results-logit-probit-fitted-example.csv",
  rds_path = "outputs/logit-probit-fitted-example.rds"
)

set.seed(settings$seed)

# ---------------------------------------------------------------------
# 2. Simulate one dataset
# ---------------------------------------------------------------------

x <- stats::runif(settings$N, settings$x_range[1], settings$x_range[2])
eta <- settings$beta_intercept + settings$beta_x * x
p_true <- stats::plogis(eta)
correct <- stats::rbinom(settings$N, size = settings$k_trials, prob = p_true)

d <- data.frame(
  x = x,
  correct = correct,
  incorrect = settings$k_trials - correct,
  proportion = correct / settings$k_trials,
  p_true = p_true
)

# ---------------------------------------------------------------------
# 3. Fit logit and probit GLMs to the same responses
# ---------------------------------------------------------------------

fit_logit <- stats::glm(
  cbind(correct, incorrect) ~ x,
  family = stats::binomial(link = "logit"),
  data = d
)

fit_probit <- stats::glm(
  cbind(correct, incorrect) ~ x,
  family = stats::binomial(link = "probit"),
  data = d
)

model_results <- data.frame(
  model = c("logit", "probit"),
  AIC = c(stats::AIC(fit_logit), stats::AIC(fit_probit)),
  intercept = c(stats::coef(fit_logit)[1], stats::coef(fit_probit)[1]),
  slope = c(stats::coef(fit_logit)[2], stats::coef(fit_probit)[2]),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------
# 4. Predictions and pointwise discrepancy
# ---------------------------------------------------------------------

newd <- data.frame(
  x = seq(settings$x_range[1], settings$x_range[2], length.out = settings$prediction_n)
)

newd$logit <- stats::predict(fit_logit, newdata = newd, type = "response")
newd$probit <- stats::predict(fit_probit, newdata = newd, type = "response")
newd$difference <- newd$logit - newd$probit

pred_long <- rbind(
  data.frame(x = newd$x, probability = newd$logit, model = "Fitted logit"),
  data.frame(x = newd$x, probability = newd$probit, model = "Fitted probit")
)

diff_limit <- max(abs(newd$difference), na.rm = TRUE) * 1.10
diff_limit <- max(diff_limit, settings$diff_min_limit)

# ---------------------------------------------------------------------
# 5. Plot
# ---------------------------------------------------------------------

pA <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = d,
    ggplot2::aes(x = x, y = proportion),
    alpha = settings$point_alpha,
    size = settings$point_size,
    shape = 21,
    stroke = 0.15,
    fill = "grey55",
    colour = "grey15"
  ) +
  ggplot2::geom_line(
    data = pred_long,
    ggplot2::aes(x = x, y = probability, colour = model),
    linewidth = settings$line_width
  ) +
  ggplot2::geom_hline(
    yintercept = 0.5,
    linetype = "dotted",
    linewidth = 0.35,
    colour = "grey55"
  ) +
  ggplot2::scale_colour_manual(
    values = c(
      "Fitted logit" = "#E69F00",
      "Fitted probit" = "#009E73"
    )
  ) +
  ggplot2::coord_cartesian(
    xlim = settings$x_range,
    ylim = c(0, 1),
    clip = "off"
  ) +
  ggplot2::labs(
    title = "A. Fitted probability",
    x = NULL,
    y = "Observed proportion / fitted probability",
    colour = NULL
  ) +
  link_theme(base_size = 10) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", hjust = 0, margin = ggplot2::margin(b = 4)),
    plot.margin = ggplot2::margin(5.5, 5.5, 0, 5.5)
  )

pB <- ggplot2::ggplot(newd, ggplot2::aes(x = x, y = difference)) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dotted",
    linewidth = 0.35,
    colour = "grey55"
  ) +
  ggplot2::geom_line(
    linewidth = settings$discrepancy_width,
    linetype = "longdash",
    colour = "grey25"
  ) +
  ggplot2::coord_cartesian(
    xlim = settings$x_range,
    ylim = c(-diff_limit, diff_limit),
    clip = "off"
  ) +
  ggplot2::labs(
    title = "B. Logit minus probit fitted probability",
    x = "Predictor value",
    y = "Logit - probit fitted probability"
  ) +
  link_theme(base_size = 10) +
  ggplot2::theme(
    legend.position = "none",
    plot.title = ggplot2::element_text(face = "bold", hjust = 0, margin = ggplot2::margin(b = 4)),
    plot.margin = ggplot2::margin(0, 5.5, 5.5, 5.5)
  )

p <- patchwork::wrap_plots(
  pA,
  pB,
  ncol = 1,
  heights = c(2.2, 1.0)
)

# ---------------------------------------------------------------------
# 6. Save and export
# ---------------------------------------------------------------------

dir.create(dirname(settings$figure_base), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(settings$table_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(settings$rds_path), recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  filename = paste0(settings$figure_base, ".pdf"),
  plot = p,
  width = settings$figure_width,
  height = settings$figure_height,
  units = "in"
)

ggplot2::ggsave(
  filename = paste0(settings$figure_base, ".png"),
  plot = p,
  width = settings$figure_width,
  height = settings$figure_height,
  units = "in",
  dpi = 300
)

utils::write.csv(model_results, settings$table_path, row.names = FALSE)

saveRDS(
  list(
    settings = settings,
    data = d,
    predictions = newd,
    model_results = model_results,
    fit_logit = fit_logit,
    fit_probit = fit_probit,
    plot = p
  ),
  settings$rds_path
)

cat("\nSaved figure to:\n")
cat("- ", paste0(settings$figure_base, ".pdf"), "\n", sep = "")
cat("- ", paste0(settings$figure_base, ".png"), "\n", sep = "")

cat("\nModel comparison:\n")
print(model_results)
