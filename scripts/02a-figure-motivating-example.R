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

source("R/project-settings.R")
source("R/utils-link-functions.R")
source("R/utils-plots.R")


link_theme <- function(base_size = 10, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 1,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size - 1,
        color = "grey25",
        margin = ggplot2::margin(b = 6)
      ),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "grey20"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.width = grid::unit(1.25, "lines"),
      panel.grid.major = ggplot2::element_blank(),
      panel.spacing = grid::unit(0.9, "lines"),
      plot.margin = ggplot2::margin(6, 8, 6, 8)
    )
}

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
cells$prob <- chance_linkinv(cells$eta, chance = chance, link = "logit")

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

interaction_on_scale <- function(y) {
  coef(lm(y ~ condition * group_num, data = cells))[["condition:group_num"]]
}

b_linear   <- interaction_on_scale(cells$prob)
b_logit    <- interaction_on_scale(qlogis(cells$prob))
b_cc_logit <- interaction_on_scale(chance_link(cells$prob, chance = chance, link = "logit"))

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

make_link_grid <- function(panel_label, link, n_lines = 11,
                           prob_limits = c(0.50, 0.995), chance = 0.50) {
  eps <- 1e-4
  
  if (link == "identity") {
    y <- seq(prob_limits[1], prob_limits[2], length.out = n_lines)
  }
  
  if (link == "logit") {
    eta_min <- qlogis(pmin(pmax(prob_limits[1], eps), 1 - eps))
    eta_max <- qlogis(pmin(pmax(prob_limits[2], eps), 1 - eps))
    y <- plogis(seq(eta_min, eta_max, length.out = n_lines))
  }
  
  if (link == "cc_logit") {
    p_min <- pmin(pmax(prob_limits[1], chance + eps), 1 - eps)
    p_max <- pmin(pmax(prob_limits[2], chance + eps), 1 - eps)
    eta_min <- qlogis((p_min - chance) / (1 - chance))
    eta_max <- qlogis((p_max - chance) / (1 - chance))
    y <- chance_linkinv(seq(eta_min, eta_max, length.out = n_lines),
                        chance = chance, link = "logit")
  }
  
  data.frame(panel = panel_label, yintercept = y)
}

link_grid <- rbind(
  make_link_grid(panel_labels[1], "identity", prob_limits = c(0.50, 1.00), n_lines = 6),
  make_link_grid(panel_labels[2], "logit",    prob_limits = c(0.50, 0.997), n_lines = 11),
  make_link_grid(panel_labels[3], "cc_logit", prob_limits = c(0.503, 0.997), n_lines = 11, chance = chance)
)
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
    labels = percent_labels(),
    name = "Probability"
  ) +
  link_scale_color_discrete(name = "Group") +
  link_scale_linetype_discrete(name = "Group") +
  link_scale_shape_discrete(name = "Group") +
  link_theme(base_size = 10.5)

# SAVE OUTPUT #

save_plot_grid(
  list(p),
  filename_base = "figs/motivating-example",
  width = figure_width,
  height = 3.9,
  ncol = 1,
  dpi = default_dpi
)

cat("\nSaved figs/motivating-example.pdf/png\n")
