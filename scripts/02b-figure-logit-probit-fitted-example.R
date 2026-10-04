# Fitted logit vs probit curves on one simulated binomial dataset: the two fits
# look almost interchangeable but imply a structured pointwise discrepancy in
# fitted probabilities. Run from the repository root.

rm(list = ls())
library(ggplot2)
library(patchwork)
for (path in c("tables", "figs", "outputs")) dir.create(path, showWarnings = FALSE)
set.seed(20260601)

N <- 1000
k_trials <- 60
x_range <- c(-2.5, 2.5)
beta_intercept <- 0.05
beta_x <- 1.75

# one dataset from a logit DGP
x <- runif(N, x_range[1], x_range[2])
p_true <- plogis(beta_intercept + beta_x * x)
correct <- rbinom(N, size = k_trials, prob = p_true)
d <- data.frame(x = x, correct = correct, incorrect = k_trials - correct, proportion = correct / k_trials, p_true = p_true)

fit_logit <- glm(cbind(correct, incorrect) ~ x, family = binomial(link = "logit"), data = d)
fit_probit <- glm(cbind(correct, incorrect) ~ x, family = binomial(link = "probit"), data = d)
model_results <- data.frame(model = c("logit", "probit"), AIC = c(AIC(fit_logit), AIC(fit_probit)),
                            intercept = c(coef(fit_logit)[1], coef(fit_probit)[1]), slope = c(coef(fit_logit)[2], coef(fit_probit)[2]))
print(model_results)
write.csv(model_results, "tables/model-results-logit-probit-fitted-example.csv", row.names = FALSE)

newd <- data.frame(x = seq(x_range[1], x_range[2], length.out = 500))
newd$logit <- predict(fit_logit, newdata = newd, type = "response")
newd$probit <- predict(fit_probit, newdata = newd, type = "response")
newd$difference <- newd$logit - newd$probit
pred_long <- rbind(data.frame(x = newd$x, probability = newd$logit, model = "Fitted logit"),
                   data.frame(x = newd$x, probability = newd$probit, model = "Fitted probit"))
diff_limit <- max(max(abs(newd$difference)) * 1.10, 0.010)

####################################################
# Figure
####################################################

theme_paper <- theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0, margin = margin(b = 4)),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9, color = "grey20"),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.width = unit(1.25, "lines"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, color = "grey88"))

pA <- ggplot() +
  geom_point(data = d, aes(x = x, y = proportion), alpha = 0.35, size = 1.2, shape = 21, stroke = 0.15, fill = "grey55", colour = "grey15") +
  geom_line(data = pred_long, aes(x = x, y = probability, colour = model), linewidth = 1.05) +
  geom_hline(yintercept = 0.5, linetype = "dotted", linewidth = 0.35, colour = "grey55") +
  scale_colour_manual(values = c("Fitted logit" = "#E69F00", "Fitted probit" = "#009E73")) +
  coord_cartesian(xlim = x_range, ylim = c(0, 1), clip = "off") +
  labs(title = "A. Fitted probability", x = NULL, y = "Observed proportion / fitted probability", colour = NULL) +
  theme_paper +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(5.5, 5.5, 0, 5.5))

pB <- ggplot(newd, aes(x = x, y = difference)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.35, colour = "grey55") +
  geom_line(linewidth = 0.90, linetype = "longdash", colour = "grey25") +
  coord_cartesian(xlim = x_range, ylim = c(-diff_limit, diff_limit), clip = "off") +
  labs(title = "B. Logit minus probit fitted probability", x = "Predictor value", y = "Logit - probit fitted probability") +
  theme_paper +
  theme(plot.margin = margin(0, 5.5, 5.5, 5.5))

p <- pA / pB + plot_layout(heights = c(2.2, 1.0))
ggsave("figs/logit-probit-fitted-example.pdf", p, width = 7.2, height = 5.8)
ggsave("figs/logit-probit-fitted-example.png", p, width = 7.2, height = 5.8, dpi = 300)

saveRDS(list(data = d, predictions = newd, model_results = model_results, fit_logit = fit_logit, fit_probit = fit_probit),
        "outputs/logit-probit-fitted-example.rds")
