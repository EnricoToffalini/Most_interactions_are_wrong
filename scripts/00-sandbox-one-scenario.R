# Single-scenario sandbox for quick tuning (optional, not part of run.R).
# Edit the scenario, inspect the implied accuracies, simulate one dataset, and
# compare interaction estimates under identity, standard logit/probit, and a
# chance-corrected binomial logit link. Run from the repository root.

rm(list = ls())
library(ggplot2)
dir.create("outputs/inspection", recursive = TRUE, showWarnings = FALSE)
set.seed(20260529)

# scenario (keep beta_age_group = 0 to study pseudo-interactions from link curvature)
N <- 600
k_trials <- 50
chance <- 0.50 # .50 for 2-AFC, .25 for 4-AFC
age_range <- c(6, 10)
age_center <- 8
beta_intercept <- 0 # lower = closer to the chance floor
beta_age <- 1.00
beta_group <- -1.40
beta_age_group <- 0

expected_accuracy <- function(age, group_num) {
  age_c <- age - age_center
  chance + (1 - chance) * plogis(beta_intercept + beta_age * age_c + beta_group * group_num + beta_age_group * age_c * group_num)
}

# implied values; group gaps are Group 1 minus Group 0, changes are oldest minus youngest
implied <- expand.grid(age = c(age_range[1], age_center, age_range[2]), group_num = c(0, 1))
implied$expected_accuracy <- expected_accuracy(implied$age, implied$group_num)
implied$expected_correct_out_of_k_trials <- implied$expected_accuracy * k_trials
print(implied)
p00 <- expected_accuracy(age_range[1], 0)
p01 <- expected_accuracy(age_range[1], 1)
p10 <- expected_accuracy(age_range[2], 0)
p11 <- expected_accuracy(age_range[2], 1)
cat("Change in group difference from youngest to oldest age:", round((p11 - p10) - (p01 - p00), 4),
    "probability points (", round(((p11 - p10) - (p01 - p00)) * k_trials, 2), "correct out of", k_trials, ")\n")

# one dataset
group_num <- rbinom(N, 1, 0.5)
age <- runif(N, age_range[1], age_range[2])
d <- data.frame(age = age, age_c = age - age_center, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")), k = k_trials)
d$y <- rbinom(N, size = k_trials, prob = expected_accuracy(age, group_num))
d$accuracy <- d$y / k_trials
print(aggregate(accuracy ~ group, data = d, mean))

####################################################
# Quick comparison models
####################################################

fit_identity <- lm(accuracy ~ age_c * group, data = d)
fit_logit <- glm(cbind(y, k - y) ~ age_c * group, family = binomial("logit"), data = d)
fit_probit <- glm(cbind(y, k - y) ~ age_c * group, family = binomial("probit"), data = d)

# chance-corrected binomial logit, with the likelihood written out explicitly
X <- model.matrix(~ age_c * group, data = d)
nll <- function(beta) {
  p <- chance + (1 - chance) * plogis(drop(X %*% beta))
  -sum(dbinom(d$y, size = d$k, prob = pmin(pmax(p, 1e-10), 1 - 1e-10), log = TRUE))
}
above <- pmin(pmax((d$accuracy - chance) / (1 - chance), 0.02), 0.98) # starting values from above-chance proportions
start <- coef(lm(qlogis(above) ~ age_c * group, data = d))
opt <- optim(start, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 1500, reltol = 1e-10))
chance_coef <- opt$par
chance_se <- sqrt(diag(solve(opt$hessian)))
chance_p <- 2 * pnorm(abs(chance_coef / chance_se), lower.tail = FALSE)

term <- "age_c:groupGroup 1"
models <- c("Identity", "Standard logit", "Standard probit", "Chance-corrected logit")
quick_results <- data.frame(model = factor(models, levels = models),
  interaction_coef = c(coef(fit_identity)[term], coef(fit_logit)[term], coef(fit_probit)[term], chance_coef[term]),
  p_value = c(summary(fit_identity)$coefficients[term, 4], summary(fit_logit)$coefficients[term, 4],
              summary(fit_probit)$coefficients[term, 4], chance_p[term]))

# model-implied change in the group difference from youngest to oldest age
nd <- expand.grid(age = age_range, group = factor(c("Group 0", "Group 1")))
nd$age_c <- nd$age - age_center
pred <- cbind(predict(fit_identity, newdata = nd), predict(fit_logit, newdata = nd, type = "response"),
              predict(fit_probit, newdata = nd, type = "response"),
              chance + (1 - chance) * plogis(drop(model.matrix(~ age_c * group, nd) %*% chance_coef)))
quick_results$change_in_group_difference_correct_out_of_k_trials <- (pred[4, ] - pred[2, ] - pred[3, ] + pred[1, ]) * k_trials
rownames(quick_results) <- NULL
print(quick_results)

####################################################
# Plots
####################################################

plot_grid <- expand.grid(age = seq(age_range[1], age_range[2], length.out = 200), group = factor(c("Group 0", "Group 1")))
plot_grid$age_c <- plot_grid$age - age_center
plot_grid$expected_accuracy <- expected_accuracy(plot_grid$age, as.numeric(plot_grid$group == "Group 1"))
pred_long <- rbind(
  data.frame(model = models[1], plot_grid, predicted = predict(fit_identity, newdata = plot_grid)),
  data.frame(model = models[2], plot_grid, predicted = predict(fit_logit, newdata = plot_grid, type = "response")),
  data.frame(model = models[3], plot_grid, predicted = predict(fit_probit, newdata = plot_grid, type = "response")),
  data.frame(model = models[4], plot_grid, predicted = chance + (1 - chance) * plogis(drop(model.matrix(~ age_c * group, plot_grid) %*% chance_coef))))
pred_long$model <- factor(pred_long$model, levels = models)
ylim <- c(max(0, chance - 0.08), 1.02)

p1 <- ggplot(plot_grid, aes(age, expected_accuracy, linetype = group)) +
  geom_hline(yintercept = chance, linetype = "dashed") + geom_line(linewidth = 1) +
  coord_cartesian(ylim = ylim) + labs(title = "A. True scenario", x = "Age", y = "Expected accuracy") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
p2 <- ggplot(d, aes(age, accuracy, shape = group)) +
  geom_hline(yintercept = chance, linetype = "dashed") + geom_point(alpha = 0.25, size = 0.8) +
  coord_cartesian(ylim = ylim) + labs(title = "B. One simulated dataset", x = "Age", y = "Observed accuracy") +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
p3 <- ggplot(pred_long, aes(age, predicted, linetype = group)) +
  geom_hline(yintercept = chance, linetype = "dashed") + geom_line(linewidth = 0.8) + facet_wrap(~ model, ncol = 2) +
  coord_cartesian(ylim = ylim) + labs(title = "C. Fitted model curves", x = "Age", y = "Predicted accuracy") +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
p4 <- ggplot(quick_results, aes(model, change_in_group_difference_correct_out_of_k_trials)) +
  geom_hline(yintercept = 0, linetype = "dashed") + geom_point(size = 2.4) + coord_flip() +
  labs(title = "D. Model-implied change in the group difference", subtitle = "Values are contrasts, not possible observed counts",
       x = NULL, y = paste0("Change in group gap from ", age_range[1], " to ", age_range[2], ", correct out of ", k_trials)) +
  theme_minimal(base_size = 10)

png("outputs/inspection/sandbox-one-scenario.png", width = 10, height = 5.8, units = "in", res = 300)
grid::grid.newpage()
print(p1, vp = grid::viewport(x = 0.25, y = 0.75, width = 0.5, height = 0.5))
print(p2, vp = grid::viewport(x = 0.75, y = 0.75, width = 0.5, height = 0.5))
print(p3, vp = grid::viewport(x = 0.25, y = 0.25, width = 0.5, height = 0.5))
print(p4, vp = grid::viewport(x = 0.75, y = 0.25, width = 0.5, height = 0.5))
dev.off()
