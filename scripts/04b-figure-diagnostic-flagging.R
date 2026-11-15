# Flagging rates of the DHARMa residual checks at alpha = .05, correct versus
# wrong link, per scenario and check. Reads tables/diagnostic-calibration.csv,
# written by scripts/04-diagnostic-worked-example.R (no new simulation); these are
# the rates that the diagnostic table summarizes as ranges.
# Also draws ROC curves of the same checks for inspection (outputs/inspection/),
# from outputs/diagnostic-simulation-replications.csv.
# Run from the repository root.

rm(list = ls())
library(ggplot2)
source("scripts/common-helpers.R")
dir.create("figs", showWarnings = FALSE)

cal <- read.csv("tables/diagnostic-calibration.csv", stringsAsFactors = FALSE)
cal <- cal[grepl("^DHARMa", cal$diagnostic), ]
scenarios <- c("Count errors", "Chance-floor accuracy", "Binary repeated trials",
               "Binary continuous predictor", "Gamma mean response time")
# the focal-predictor check is the quantile check for a continuous predictor
# and the comparison across design cells in the 2 x 2 case
checks <- c("DHARMa uniformity" = "Uniformity", "DHARMa dispersion" = "Dispersion",
            "DHARMa residual quantiles over fitted values" = "Quantiles over fitted values",
            "DHARMa residual quantiles over focal predictor" = "Focal predictor or design cells",
            "DHARMa residual distribution across design cells" = "Focal predictor or design cells")
cal$check <- factor(checks[cal$diagnostic], levels = unique(checks))
cal$scenario <- factor(cal$scenario, levels = scenarios)
cal$row <- factor(cal$scenario, levels = rev(scenarios))
wide <- merge(cal[cal$model_specification == "Correct link", c("scenario", "check", "row", "flagging_rate")],
              cal[cal$model_specification == "Wrong link", c("scenario", "check", "flagging_rate")],
              by = c("scenario", "check"), suffixes = c("_correct", "_wrong"))
print(wide[order(wide$scenario, wide$check), ])

scenario_colors <- c("Count errors" = "#D55E00", "Chance-floor accuracy" = "#0072B2", "Binary repeated trials" = "#009E73",
                     "Binary continuous predictor" = "#CC79A7", "Gamma mean response time" = "#E69F00")
# shape repeats the scenario, so the figure also reads in greyscale
scenario_shapes <- c("Count errors" = 21, "Chance-floor accuracy" = 22, "Binary repeated trials" = 24,
                     "Binary continuous predictor" = 25, "Gamma mean response time" = 23)
# square-root axis spreads the low rates; 0 stays at the left edge
rate_breaks <- c(0, .01, .05, .10, .25, .50, .75, 1)
rate_labels <- c("0", ".01", ".05", ".10", ".25", ".50", ".75", "1")

size <- 10.5
p <- ggplot(wide, aes(y = row)) +
  geom_vline(xintercept = .05, linetype = "dashed", color = "grey55", linewidth = 0.4) +
  geom_segment(aes(x = flagging_rate_correct, xend = flagging_rate_wrong, yend = row, color = scenario), linewidth = 0.8) +
  geom_point(aes(x = flagging_rate_correct, shape = scenario, color = scenario), fill = "white", size = 2.4, stroke = 0.8) +
  geom_point(aes(x = flagging_rate_wrong, shape = scenario, fill = scenario), color = "grey15", size = 2.4, stroke = 0.4) +
  facet_grid(check ~ ., switch = "y", labeller = labeller(check = label_wrap_gen(16))) +
  scale_color_manual(values = scenario_colors, guide = "none") +
  scale_fill_manual(values = scenario_colors, name = NULL) +
  scale_shape_manual(values = scenario_shapes, name = NULL) +
  scale_x_sqrt(limits = c(0, 1), breaks = rate_breaks, labels = rate_labels, expand = expansion(mult = 0.01),
               name = "Flagging rate (open: correct link; filled: wrong link)") +
  scale_y_discrete(labels = NULL, name = NULL) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(color = "grey15", stroke = 0.4)),
         shape = guide_legend(nrow = 2)) +
  theme_paper(size) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = size - 1),
        panel.grid.major.y = element_blank(), panel.spacing.y = unit(0.35, "lines"),
        panel.background = element_rect(fill = "grey97", color = NA),
        legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(0.3, "lines"))

ggsave("figs/diagnostic-flagging.pdf", p, width = 7.2, height = 4.2)
ggsave("figs/diagnostic-flagging.png", p, width = 7.2, height = 4.2, dpi = 300, bg = "white")

####################################################
# Inspection only (not a paper figure): ROC curves of the same checks.
# The p-value threshold t runs from 0 to 1: x = share of correct-link fits
# with p < t, y = share of wrong-link fits with p < t. Points mark t = .05,
# the rates of the figure above. Each curve uses all replications in which
# that check returned a p-value, as in the diagnostic table.
####################################################

d <- read.csv("outputs/diagnostic-simulation-replications.csv", stringsAsFactors = FALSE)
share_below <- function(p, t) findInterval(t, sort(p), left.open = TRUE) / length(p) # share of p < t
roc_checks <- c("Uniformity" = "uniformity_p", "Dispersion" = "dispersion_p",
                "Quantiles over fitted values" = "quantile_fitted_p", "Focal predictor or design cells" = "focal")
roc <- roc_points <- NULL
for (s in scenarios) {
  dat <- d[d$scenario == s, ]
  for (j in 1:length(roc_checks)) {
    suffix <- roc_checks[j]
    if (suffix == "focal") suffix <- if (s == "Binary repeated trials") "categorical_design_p" else "quantile_predictor_p"
    p_correct <- dat[[paste0("dharma_correct_", suffix)]]
    p_wrong <- dat[[paste0("dharma_wrong_", suffix)]]
    p_correct <- p_correct[is.finite(p_correct)]
    p_wrong <- p_wrong[is.finite(p_wrong)]
    t <- c(0, sort(unique(c(p_correct, p_wrong))), 1.001)
    roc <- rbind(roc, data.frame(scenario = s, check = names(roc_checks)[j],
      fpr = share_below(p_correct, t), tpr = share_below(p_wrong, t)))
    roc_points <- rbind(roc_points, data.frame(scenario = s, check = names(roc_checks)[j],
      fpr = mean(p_correct < .05), tpr = mean(p_wrong < .05),
      auc = mean(outer(p_wrong, p_correct, "<")) + 0.5 * mean(outer(p_wrong, p_correct, "=="))))
  }
}
print(roc_points)
roc$scenario <- factor(roc$scenario, levels = scenarios)
roc_points$scenario <- factor(roc_points$scenario, levels = scenarios)
roc$check <- factor(roc$check, levels = names(roc_checks))
roc_points$check <- factor(roc_points$check, levels = names(roc_checks))

check_lines <- c("Uniformity" = "dotted", "Dispersion" = "dotdash", "Quantiles over fitted values" = "longdash",
                 "Focal predictor or design cells" = "solid")
check_shapes <- c("Uniformity" = 21, "Dispersion" = 24, "Quantiles over fitted values" = 22, "Focal predictor or design cells" = 23)
p_roc <- ggplot(roc, aes(fpr, tpr, color = scenario, linetype = check)) +
  geom_abline(intercept = 0, slope = 1, color = "grey60", linewidth = 0.4) +
  geom_path(linewidth = 0.65) +
  geom_point(data = roc_points, aes(fill = scenario, shape = check), color = "grey15", size = 2.4, stroke = 0.4) +
  scale_color_manual(values = scenario_colors, name = "Scenario") +
  scale_fill_manual(values = scenario_colors, guide = "none") +
  scale_shape_manual(values = check_shapes, name = "DHARMa check") +
  scale_linetype_manual(values = check_lines, name = "DHARMa check") +
  scale_x_sqrt(limits = c(0, 1), breaks = rate_breaks, labels = rate_labels, expand = expansion(mult = 0.01),
               name = "Flagging rate, correct link") +
  scale_y_sqrt(limits = c(0, 1), breaks = rate_breaks, labels = rate_labels, expand = expansion(mult = 0.01),
               name = "Flagging rate, wrong link") +
  coord_equal() +
  guides(color = guide_legend(ncol = 1, order = 1, override.aes = list(shape = NA, linewidth = 1.1)),
         shape = guide_legend(ncol = 1, order = 2, override.aes = list(fill = "grey70")),
         linetype = guide_legend(ncol = 1, order = 2, override.aes = list(linewidth = 0.6))) +
  labs(title = "ROC curves of the DHARMa residual checks",
       subtitle = "Threshold on the p-value from 0 to 1; points at .05; square-root axes") +
  theme_paper(size) +
  theme(legend.position = "right", legend.key.width = unit(2.6, "lines"))

ggsave("outputs/inspection/diagnostic-roc.pdf", p_roc, width = 7.2, height = 4.8)
ggsave("outputs/inspection/diagnostic-roc.png", p_roc, width = 7.2, height = 4.8, dpi = 300, bg = "white")
