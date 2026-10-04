# Motivating example figure.
# A minimal 2 x 2 (condition x group) design with the SAME cell probabilities in
# every panel. Only the scale on which the product term is evaluated changes:
# the implied interaction is positive on the standard logit scale, negative on
# the probability scale, and exactly zero on the chance-corrected logit scale,
# where the data were generated. Same data, different no-interaction baseline.
# Run from the repository root.

rm(list = ls())
library(ggplot2)
dir.create("figs", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# additive structure on the chance-corrected logit scale: p = chance + (1 - chance) * plogis(eta)
chance <- 0.50
beta0 <- -1.00
beta_condition <- 2.10
beta_group <- 1.70
beta_interaction <- 0

cells <- expand.grid(condition = c(0, 1), group_num = c(0, 1))
cells$eta <- beta0 + beta_condition * cells$condition + beta_group * cells$group_num + beta_interaction * cells$condition * cells$group_num
cells$prob <- chance + (1 - chance) * plogis(cells$eta)
cells$group <- factor(cells$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))

# The 2 x 2 design is saturated, so the product-term coefficient is the
# difference between the two condition differences on each scale
b_linear <- coef(lm(prob ~ condition * group_num, data = cells))[["condition:group_num"]]
b_logit <- coef(lm(qlogis(prob) ~ condition * group_num, data = cells))[["condition:group_num"]]
b_cc_logit <- coef(lm(qlogis(pmin(pmax((prob - chance) / (1 - chance), 1e-8), 1 - 1e-8)) ~ condition * group_num, data = cells))[["condition:group_num"]]

coef_table <- data.frame(panel = c("Linear (probability)", "Standard logit", "Chance-corrected logit"),
                         b_interaction = c(b_linear, b_logit, b_cc_logit))
print(coef_table, row.names = FALSE)

# saved so that Supplement A can report the generating parameters of the figure
cells <- cells[order(cells$group_num, cells$condition), ]
print(cells[, c("group", "condition", "eta", "prob")], row.names = FALSE)
write.csv(data.frame(chance = chance, beta_intercept = beta0, beta_condition = beta_condition, beta_group = beta_group,
                     beta_interaction = beta_interaction, group = as.character(cells$group), condition = cells$condition,
                     linear_predictor = cells$eta, expected_probability = cells$prob),
          "tables/motivating-example-cells.csv", row.names = FALSE)
write.csv(coef_table, "tables/motivating-example-interaction-by-scale.csv", row.names = FALSE)

####################################################
# Figure
####################################################

# round numerical noise so that an exact zero does not print as "-0.000"
zap <- function(x) if (abs(x) < 1e-8) 0 else x
panel_labels <- c(sprintf("Linear (probability)\nB = %+0.3f", zap(b_linear)),
                  sprintf("Standard logit\nB = %+0.3f", zap(b_logit)),
                  sprintf("Chance-corrected logit\nB = %0.3f", zap(b_cc_logit)))
plot_data <- rbind(data.frame(cells, panel = panel_labels[1]),
                   data.frame(cells, panel = panel_labels[2]),
                   data.frame(cells, panel = panel_labels[3]))
plot_data$panel <- factor(plot_data$panel, levels = panel_labels)

# Horizontal lines are equally spaced on each panel's OWN link scale: equal link-scale
# steps map to unequal probability steps, which is the visual signature of the scale problem
link_grid <- rbind(
  data.frame(panel = panel_labels[1], yintercept = seq(0.50, 1.00, length.out = 6)),
  data.frame(panel = panel_labels[2], yintercept = plogis(seq(qlogis(0.50), qlogis(0.997), length.out = 11))),
  data.frame(panel = panel_labels[3], yintercept = chance + (1 - chance) * plogis(seq(qlogis((0.503 - chance) / (1 - chance)),
                                                                                      qlogis((0.997 - chance) / (1 - chance)), length.out = 11))))
link_grid$panel <- factor(link_grid$panel, levels = panel_labels)

size <- 10.5
p <- ggplot(plot_data, aes(x = condition, y = prob, group = group, color = group, linetype = group, shape = group)) +
  geom_hline(data = link_grid, aes(yintercept = yintercept), color = "grey66", linewidth = 0.4) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.8) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(breaks = c(0, 1), limits = c(-0.15, 1.15), name = "Condition") +
  scale_y_continuous(breaks = seq(0.5, 1.0, by = 0.1), limits = c(0.48, 1.02), labels = function(x) paste0(round(100 * x), "%"), name = "Probability") +
  scale_color_manual(values = c("Group 0" = "#0072B2", "Group 1" = "#D55E00"), name = "Group") +
  scale_linetype_manual(values = c("Group 0" = "solid", "Group 1" = "longdash"), name = "Group") +
  scale_shape_manual(values = c("Group 0" = 16, "Group 1" = 17), name = "Group") +
  theme_minimal(base_size = size) +
  theme(axis.title = element_text(size = size),
        axis.text = element_text(size = size - 1, color = "grey20"),
        strip.text = element_text(face = "bold", size = size - 1),
        legend.position = "bottom",
        legend.title = element_text(size = size - 1),
        legend.text = element_text(size = size - 1),
        legend.key.width = unit(1.25, "lines"),
        panel.grid.major = element_blank(),
        panel.spacing = unit(0.9, "lines"),
        plot.margin = margin(6, 8, 6, 8))

ggsave("figs/motivating-example.pdf", p, width = 7.2, height = 3.9)
ggsave("figs/motivating-example.png", p, width = 7.2, height = 3.9, dpi = 300, bg = "white")
