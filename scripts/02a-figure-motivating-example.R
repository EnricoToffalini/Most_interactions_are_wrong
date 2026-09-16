# scripts/02a-figure-motivating-example.R
# Motivating example figure.
# A minimal 2 x 2 (condition x group) design with the SAME cell probabilities
# in every panel. Because the panels only differ in the scale on which the
# product term is evaluated, the implied interaction coefficient changes -
# it is positive on the standard logit scale, negative on the probability
# (linear) scale, and exactly zero on the chance-corrected logit scale, the
# scale on which the data were generated. Same data, different no-interaction
# baseline, different moderation conclusion.

library(ggplot2)

# Run from the repository root. This figure uses deterministic cell probabilities.
default_dpi <- 300
figure_width <- 7.2

dir.create("figs", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# 1. Scenario
# ---------------------------------------------------------------------
# Additive data-generating structure on the chance-corrected logit scale:
#   p = chance + (1 - chance) * plogis(eta)
#   eta = beta0 + beta_condition * condition + beta_group * group
# There is NO condition-by-group product term on this scale.

chance <- 0.50

beta0 <- -1.00
beta_condition <- 2.10
beta_group <- 1.70
beta_interaction <- 0.00

cells <- expand.grid(
  condition = c(0, 1),
  group_num = c(0, 1)
)

cells$eta <- with(
  cells,
  beta0 + beta_condition * condition + beta_group * group_num +
    beta_interaction * condition * group_num
)

# Same cell probabilities feed all three panels.
cells$prob <- chance + (1 - chance) * stats::plogis(cells$eta)

cells$group <- factor(
  cells$group_num,
  levels = c(0, 1),
  labels = c("Group 0", "Group 1")
)

# ---------------------------------------------------------------------
# 2. Implied interaction coefficient on each scale
# ---------------------------------------------------------------------
# The 2 x 2 design is saturated, so the product-term coefficient equals the
# difference between the two condition differences on the relevant scale.

b_linear   <- coef(lm((cells$prob) ~ condition * group_num, data = cells))[["condition:group_num"]]
b_logit    <- coef(lm((qlogis(cells$prob)) ~ condition * group_num, data = cells))[["condition:group_num"]]
b_cc_logit <- coef(lm((stats::qlogis(pmin(pmax((cells$prob - chance) / (1 - chance), 1e-8), 1 - 1e-8))) ~ condition * group_num, data = cells))[["condition:group_num"]]

coef_table <- data.frame(
  panel = c("Linear (probability)", "Standard logit", "Chance-corrected logit"),
  b_interaction = c(b_linear, b_logit, b_cc_logit)
)

cat("\nImplied interaction coefficient by scale:\n")
print(coef_table, row.names = FALSE)
cat("\nCell probabilities:\n")
print(cells[order(cells$group_num, cells$condition), c("group", "condition", "eta", "prob")],
  row.names = FALSE)

# The figure prints these quantities inside the panel labels only. Saving them
# lets Supplement A document the generative parameters of the figure without
# duplicating the constants in the supplement source.
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

motivating_cells <- cells[order(cells$group_num, cells$condition), ]
utils::write.csv(
  data.frame(
    chance = chance,
    beta_intercept = beta0,
    beta_condition = beta_condition,
    beta_group = beta_group,
    beta_interaction = beta_interaction,
    group = as.character(motivating_cells$group),
    condition = motivating_cells$condition,
    linear_predictor = motivating_cells$eta,
    expected_probability = motivating_cells$prob,
    stringsAsFactors = FALSE
  ),
  "tables/motivating-example-cells.csv",
  row.names = FALSE
)

utils::write.csv(
  coef_table,
  "tables/motivating-example-interaction-by-scale.csv",
  row.names = FALSE
)

# ---------------------------------------------------------------------
# 3. Plot data
# ---------------------------------------------------------------------

# Clean numerical noise so an exact zero does not print as "-0.000".
zap <- function(x, tol = 1e-8) if (abs(x) < tol) 0 else x

panel_labels <- c(
  sprintf("Linear (probability)\nB = %+0.3f", zap(b_linear)),
  sprintf("Standard logit\nB = %+0.3f", zap(b_logit)),
  sprintf("Chance-corrected logit\nB = %0.3f", zap(b_cc_logit))
)

plot_data <- do.call(
  rbind,
  lapply(panel_labels, function(lab) {
      out <- cells
      out$panel <- lab
      out
  })
)
plot_data$panel <- factor(plot_data$panel, levels = panel_labels)

# ---------------------------------------------------------------------
# 4. Link-scale reference lines
# ---------------------------------------------------------------------
# Horizontal lines are equally spaced on each panel's OWN link scale. Equal
# steps on the link scale map to unequal steps on the probability axis: this
# is the visual signature of the scale problem.

# Each panel gets equally spaced steps on its own link scale.
identity_lines <- seq(0.50, 1.00, length.out = 6)
logit_lines <- plogis(seq(qlogis(0.50), qlogis(0.997), length.out = 11))
chance_lines <- chance + (1 - chance) * plogis(seq(
    qlogis((0.503 - chance) / (1 - chance)),
    qlogis((0.997 - chance) / (1 - chance)), length.out = 11))
link_grid <- rbind(
  data.frame(panel = panel_labels[1], yintercept = identity_lines),
  data.frame(panel = panel_labels[2], yintercept = logit_lines),
  data.frame(panel = panel_labels[3], yintercept = chance_lines))
link_grid$panel <- factor(link_grid$panel, levels = panel_labels)

# ---------------------------------------------------------------------
# 5. Figure
# ---------------------------------------------------------------------

p <- ggplot(
  plot_data,
  aes(x = condition, y = prob, group = group,
    color = group, linetype = group, shape = group)
) +
  geom_hline(
  data = link_grid,
  aes(yintercept = yintercept),
  inherit.aes = FALSE,
  color = "grey66",
  linewidth = 0.4
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.8) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(
  breaks = c(0, 1),
  limits = c(-0.15, 1.15),
  name = "Condition"
) +
  scale_y_continuous(
  breaks = seq(0.5, 1.0, by = 0.1),
  limits = c(0.48, 1.02),
  labels = function(x) paste0(round(100 * x / 1) * 1, "%"),
  name = "Probability"
) +
  ggplot2::scale_color_manual(values = c(
    "Group 0" = "#0072B2",
    "Group 1" = "#D55E00",
    "Identity" = "#009E73",
    "Gaussian identity" = "#009E73",
    "Standard logit" = "#0072B2",
    "Standard probit" = "#CC79A7",
    "Standard binomial logit" = "#0072B2",
    "Chance-corrected logit" = "#D55E00",
    "Chance-corrected binomial" = "#D55E00",
    "Chance-corrected binomial link" = "#D55E00",
    "Observed data" = "grey30",
    "Generating model" = "black"
  ), name = "Group") +
  ggplot2::scale_linetype_manual(values = c(
    "Group 0" = "solid",
    "Group 1" = "longdash",
    "Identity" = "solid",
    "Gaussian identity" = "solid",
    "Standard logit" = "solid",
    "Standard probit" = "dotdash",
    "Standard binomial logit" = "solid",
    "Chance-corrected logit" = "longdash",
    "Chance-corrected binomial" = "longdash",
    "Chance-corrected binomial link" = "longdash",
    "Observed data" = "blank",
    "Generating model" = "solid"
  ), name = "Group") +
  ggplot2::scale_shape_manual(values = c(
    "Group 0" = 16,
    "Group 1" = 17,
    "Identity" = 16,
    "Gaussian identity" = 16,
    "Standard logit" = 15,
    "Standard probit" = 18,
    "Standard binomial logit" = 15,
    "Chance-corrected logit" = 17,
    "Chance-corrected binomial" = 17,
    "Chance-corrected binomial link" = 17,
    "Observed data" = 16,
    "Generating model" = 1
  ), name = "Group") +
  (ggplot2::theme_minimal(base_size = (10.5), base_family = ("")) +
    ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = (10.5) + 1,
      margin = ggplot2::margin(b = 3)
    ),
    plot.subtitle = ggplot2::element_text(
      size = (10.5) - 1,
      color = "grey25",
      margin = ggplot2::margin(b = 6)
    ),
    axis.title = ggplot2::element_text(size = (10.5)),
    axis.text = ggplot2::element_text(size = (10.5) - 1, color = "grey20"),
    strip.text = ggplot2::element_text(face = "bold", size = (10.5) - 1),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(size = (10.5) - 1),
    legend.text = ggplot2::element_text(size = (10.5) - 1),
    legend.key.width = grid::unit(1.25, "lines"),
    panel.grid.major = ggplot2::element_blank(),
    panel.spacing = grid::unit(0.9, "lines"),
    plot.margin = ggplot2::margin(6, 8, 6, 8)
))

# SAVE OUTPUT #

# Write the same panel layout to PDF and PNG.
plots <- list(p)
plot_columns <- 1
plot_rows <- ceiling(length(plots) / plot_columns)
dir.create(dirname("figs/motivating-example"), recursive = TRUE, showWarnings = FALSE)
for (plot_format in c("pdf", "png")) {
  if (plot_format == "pdf") {
    grDevices::pdf(paste0("figs/motivating-example", ".pdf"), width = figure_width, height = 3.9)
  } else {
    grDevices::png(paste0("figs/motivating-example", ".png"), width = figure_width, height = 3.9, units = "in", res = default_dpi)
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

cat("\nSaved figs/motivating-example.pdf/png\n")
