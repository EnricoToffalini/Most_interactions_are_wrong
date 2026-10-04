# Simulation 3: sum scores as bounded and discrete outcomes.
# A latent score theta is generated with NO x-by-group product term; J ordinal
# items (0-3) are drawn from a graded logistic model and summed.
# The latent model gives nominal rejection rates; the two observed-score models
# give pseudo-interaction detection rates.
# Run from the repository root. N_SIM, N_CORES, ALPHA can be set as environment variables.

rm(list = ls())
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(ggplot2)
library(parallel)

B <- as.integer(Sys.getenv("N_SIM", "3000")) # replications per scenario (3000 for the paper)
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
seed <- 20260526
set.seed(seed)
for (path in c("tables", "figs", "outputs/inspection")) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# scenario parameters
N <- 600
J <- 9 # items
item_max <- 3
max_score <- J * item_max
theta_sd <- 1
beta_x <- 0.85
beta_group <- -0.90
beta_x_group <- 0
x_low <- -1
x_high <- 1

scenarios <- data.frame(
  scenario = c("Lower range", "Middle range", "Upper range"),
  threshold_shift = c(1.60, 0.00, -1.60),
  interpretation = c("Expected sum scores occupy the lower range of the observed scale.",
                     "Expected sum scores occupy the middle range of the observed scale.",
                     "Expected sum scores occupy the upper range of the observed scale.")
)
model_names <- c("Observed sum-score identity", "Gaussian-probit bounded score", "Latent generating scale")

# deterministic item thresholds (J x 3), no random jitter
make_thresholds <- function(threshold_shift) {
  matrix(rep(c(-1, 0, 1), each = J), nrow = J) + seq(-0.45, 0.45, length.out = J) + threshold_shift
}

# expected sum score at latent mean mu, integrating over the latent residual (201-point grid)
expected_sum <- function(mu, thresholds) {
  q <- qnorm((1:201 - 0.5) / 201) * theta_sd
  mean(sapply(mu + q, function(th) sum(plogis(th - as.vector(thresholds)))))
}

# one simulated dataset
sim_data <- function(threshold_shift) {
  x <- rnorm(N, 0, 1)
  group_num <- rbinom(N, 1, 0.5)
  theta <- beta_x * x + beta_group * group_num + beta_x_group * x * group_num + rnorm(N, 0, theta_sd)
  thresholds <- make_thresholds(threshold_shift)
  items <- matrix(NA, N, J)
  for (j in 1:J) {
    u <- runif(N)
    items[, j] <- (u < plogis(theta - thresholds[j, 1])) + (u < plogis(theta - thresholds[j, 2])) + (u < plogis(theta - thresholds[j, 3]))
  }
  d <- data.frame(x = x, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
                  theta = theta, sum_score = rowSums(items))
  # the Gaussian-probit model needs responses strictly inside (0, 1): continuity-corrected proportion
  d$score_prop_probit <- (d$sum_score + 0.5) / (max_score + 1)
  d
}

cat("\nSimulation 3: sum scores\n")
print(scenarios[, 1:2])

####################################################
# Deterministic scenario table and plotting grids
####################################################

implied_values <- derived_contrasts <- plot_grid <- threshold_data <- NULL
for (i in 1:nrow(scenarios)) {
  th <- make_thresholds(scenarios$threshold_shift[i])
  g <- expand.grid(x = c(x_low, 0, x_high), group_num = c(0, 1))
  g$latent_mean <- beta_x * g$x + beta_group * g$group_num + beta_x_group * g$x * g$group_num
  g$expected_sum_score <- sapply(g$latent_mean, expected_sum, thresholds = th)
  implied_values <- rbind(implied_values, data.frame(table_part = "implied_values", scenario = scenarios$scenario[i],
    x = g$x, group = paste("Group", g$group_num), latent_mean = g$latent_mean, expected_sum_score = g$expected_sum_score,
    expected_percent_of_max = g$expected_sum_score / max_score,
    contrast = NA, value_sum_score_units = NA, value_percent_of_max = NA, latent_scale_value = NA))

  # group gaps are Group 1 minus Group 0; changes are high x minus low x
  e <- g$expected_sum_score
  e00 <- e[g$x == x_low & g$group_num == 0]
  e01 <- e[g$x == x_low & g$group_num == 1]
  e10 <- e[g$x == x_high & g$group_num == 0]
  e11 <- e[g$x == x_high & g$group_num == 1]
  values <- c(e01 - e00, e11 - e10, e10 - e00, e11 - e01, (e11 - e10) - (e01 - e00), NA)
  derived_contrasts <- rbind(derived_contrasts, data.frame(table_part = "derived_contrasts", scenario = scenarios$scenario[i],
    x = NA, group = NA, latent_mean = NA, expected_sum_score = NA, expected_percent_of_max = NA,
    contrast = c("Group difference at low x: Group 1 minus Group 0",
                 "Group difference at high x: Group 1 minus Group 0",
                 "x-related change in Group 0: high x minus low x",
                 "x-related change in Group 1: high x minus low x",
                 "Change in group difference from low x to high x",
                 "Generating latent-scale x-by-group product term"),
    value_sum_score_units = values, value_percent_of_max = values / max_score,
    latent_scale_value = c(NA, NA, NA, NA, NA, beta_x_group)))

  g <- expand.grid(x = seq(-2.5, 2.5, length.out = 200), group_num = c(0, 1))
  g$scenario <- scenarios$scenario[i]
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$expected_sum_score <- sapply(beta_x * g$x + beta_group * g$group_num + beta_x_group * g$x * g$group_num, expected_sum, thresholds = th)
  plot_grid <- rbind(plot_grid, g)

  threshold_data <- rbind(threshold_data, data.frame(scenario = scenarios$scenario[i], item = rep(1:J, 3),
    threshold_number = rep(1:3, each = J), threshold = as.vector(th)))
}
scenario_table <- rbind(implied_values, derived_contrasts)
write.csv(scenario_table, "tables/scenario-table-sum-scores.csv", row.names = FALSE)
print(scenario_table[, c("scenario", "x", "group", "expected_sum_score", "contrast", "value_sum_score_units")])

####################################################
# One example dataset per scenario
####################################################

example_data <- NULL
for (i in 1:nrow(scenarios)) example_data <- rbind(example_data, data.frame(scenario = scenarios$scenario[i], sim_data(scenarios$threshold_shift[i])))
cat("\nObserved mean sum score in one example dataset per scenario:\n")
print(aggregate(sum_score ~ scenario + group, example_data, mean))

####################################################
# Monte Carlo simulation
####################################################

# One replication of scenario i. The bounded-score model is a Gaussian GLM with
# probit link on the continuity-corrected proportion (not a binomial model).
sim_one <- function(b, i) {
  set.seed(seed + 100000 * i + b)
  d <- sim_data(scenarios$threshold_shift[i])
  fits <- list(try(lm(sum_score ~ x * group, data = d), silent = TRUE),
               try(glm(score_prop_probit ~ x * group, family = gaussian(link = make.link("probit")), data = d), silent = TRUE),
               try(lm(theta ~ x * group, data = d), silent = TRUE))
  to_sum_score <- c(1, max_score, 1) # rescale bounded-score predictions to the sum-score metric
  nd <- expand.grid(x = c(x_low, x_high), group = factor(c("Group 0", "Group 1")))
  p <- coefs <- did <- rep(NA, 3)
  for (j in 1:3) {
    if (inherits(fits[[j]], "try-error")) next
    tab <- summary(fits[[j]])$coefficients
    if ("x:groupGroup 1" %in% rownames(tab)) {
      p[j] <- tab["x:groupGroup 1", 4]
      coefs[j] <- tab["x:groupGroup 1", 1]
    }
    pred <- predict(fits[[j]], newdata = nd, type = "response") * to_sum_score[j]
    did[j] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  data.frame(scenario = scenarios$scenario[i], model = model_names, replication = b, p_value = p, interaction_coef = coefs,
             change_in_group_difference_response_scale = did / max_score, change_in_group_difference_outcome_units = did)
}

cat("\nRunning B =", B, "replications per scenario on", n_cores, "cores\n")
cl <- makeCluster(n_cores)
clusterExport(cl, c("sim_data", "make_thresholds", "seed", "scenarios", "model_names", "N", "J", "max_score",
                    "theta_sd", "beta_x", "beta_group", "beta_x_group", "x_low", "x_high"))
results <- list()
for (i in 1:nrow(scenarios)) {
  cat("Scenario:", scenarios$scenario[i], "\n")
  results[[i]] <- do.call(rbind, parLapply(cl, 1:B, sim_one, i = i))
}
stopCluster(cl)
simulation_results <- do.call(rbind, results)

####################################################
# Summary
####################################################

simulation_summary <- NULL
for (s in scenarios$scenario) {
  for (m in model_names) {
    dat <- simulation_results[simulation_results$scenario == s & simulation_results$model == m, ]
    ok <- is.finite(dat$p_value)
    n <- sum(ok)
    x <- sum(dat$p_value[ok] < alpha)
    # Wilson 95% interval
    z <- qnorm(0.975)
    rate <- if (n > 0) x / n else NA
    center <- (rate + z^2 / (2 * n)) / (1 + z^2 / n)
    half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
    cf <- dat$interaction_coef[is.finite(dat$interaction_coef)]
    did <- dat$change_in_group_difference_outcome_units[is.finite(dat$change_in_group_difference_outcome_units)]
    simulation_summary <- rbind(simulation_summary, data.frame(scenario = s, model = m,
      n_successful_fits = n, n_rejections = x, rejection_rate = rate,
      ci_low = if (n > 0) center - half else NA, ci_high = if (n > 0) center + half else NA,
      median_interaction_coef = median(cf), mean_interaction_coef = if (length(cf)) mean(cf) else NA, sd_interaction_coef = sd(cf),
      q25_interaction_coef = unname(quantile(cf, 0.25)), q75_interaction_coef = unname(quantile(cf, 0.75)),
      q025_interaction_coef = unname(quantile(cf, 0.025)), q975_interaction_coef = unname(quantile(cf, 0.975)),
      median_abs_coef_significant = median(abs(dat$interaction_coef[ok & dat$p_value < alpha]), na.rm = TRUE),
      median_abs_coef_nonsignificant = median(abs(dat$interaction_coef[ok & dat$p_value >= alpha]), na.rm = TRUE),
      median_change_in_group_difference_response_scale = median(dat$change_in_group_difference_response_scale, na.rm = TRUE),
      median_change_in_group_difference_outcome_units = median(did),
      mean_change_in_group_difference_outcome_units = if (length(did)) mean(did) else NA, sd_change_in_group_difference_outcome_units = sd(did),
      rate_type = if (m == "Latent generating scale") "Nominal rejection rate" else "Pseudo-interaction detection rate",
      change_in_group_difference_units = if (m == "Latent generating scale") "latent theta units" else paste0("sum-score units on the 0-", max_score, " scale")))
  }
}
simulation_summary <- simulation_summary[order(simulation_summary$scenario, simulation_summary$model), ]
write.csv(simulation_summary, "tables/simulation-summary-sum-scores.csv", row.names = FALSE)
print(simulation_summary[, c("scenario", "model", "n_successful_fits", "rejection_rate", "median_change_in_group_difference_outcome_units")])
cat("median_change_in_group_difference_outcome_units: model-implied change in the group gap from x =", x_low, "to x =", x_high,
    "in sum-score units (latent theta units for the latent model); a contrast, not an observable score\n")

####################################################
# Figures
####################################################

theme_paper <- function(size) {
  theme_minimal(base_size = size) +
    theme(plot.title = element_text(face = "bold", size = size + 1, margin = margin(b = 3)),
          plot.subtitle = element_text(size = size - 1, color = "grey25", margin = margin(b = 6)),
          axis.title = element_text(size = size),
          axis.text = element_text(size = size - 1, color = "grey20"),
          strip.text = element_text(face = "bold", size = size - 1),
          legend.position = "bottom",
          legend.title = element_text(size = size - 1),
          legend.text = element_text(size = size - 1),
          legend.key.width = unit(1.25, "lines"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
          panel.spacing = unit(0.9, "lines"),
          plot.margin = margin(6, 8, 6, 8))
}

# light lines at equal steps on the latent scale, mapped to expected sum scores
latent_grid <- NULL
for (i in 1:nrow(scenarios)) {
  latent_steps <- -6:6 + scenarios$threshold_shift[i]
  latent_grid <- rbind(latent_grid, data.frame(scenario = scenarios$scenario[i],
    yintercept = sapply(latent_steps, expected_sum, thresholds = make_thresholds(scenarios$threshold_shift[i]))))
}

pA <- ggplot(plot_grid, aes(x, expected_sum_score, color = group, linetype = group)) +
  geom_hline(data = latent_grid, aes(yintercept = yintercept), color = "grey82", linewidth = 0.35) +
  geom_hline(yintercept = c(0, max_score), linetype = "dashed") +
  geom_line(linewidth = 0.95) +
  facet_wrap(~ scenario) +
  scale_y_continuous(limits = c(-0.5, max_score + 0.5)) +
  scale_color_manual(values = c("Group 0" = "#0072B2", "Group 1" = "#D55E00"), name = NULL) +
  scale_linetype_manual(values = c("Group 0" = "solid", "Group 1" = "longdash"), name = NULL) +
  labs(title = "A. Implied sum-score curves", subtitle = "Horizontal lines mark equal steps on the latent scale",
       x = "Continuous predictor x", y = "Expected sum score") +
  theme_paper(10) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

pB <- ggplot(simulation_summary, aes(x = model, y = rejection_rate)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high)) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(title = "B. Product-term rejection rate",
       subtitle = "Latent generating-scale model: nominal rejection; observed-score models: pseudo-interaction detection. Dashed line: nominal alpha",
       x = NULL, y = "Replications rejecting the x-by-group product term") +
  theme_paper(9)

for (ext in c("pdf", "png")) {
  if (ext == "pdf") pdf("figs/sum-score-simulation.pdf", width = 7.2, height = 7.2)
  if (ext == "png") png("figs/sum-score-simulation.png", width = 7.2, height = 7.2, units = "in", res = 300)
  grid::grid.newpage()
  print(pA, vp = grid::viewport(y = 0.75, height = 0.5))
  print(pB, vp = grid::viewport(y = 0.25, height = 0.5))
  dev.off()
}

# inspection plots
p_effect <- ggplot(simulation_summary, aes(x = model, y = median_change_in_group_difference_outcome_units)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  facet_wrap(~ scenario) +
  labs(title = "Inspection: median model-implied change in the group difference",
       subtitle = "Observed-score models are in sum-score units; the latent-generating-scale comparison is in latent theta units",
       x = NULL, y = paste0("Change in group gap from ", x_low, " to ", x_high, ", sum-score units on the 0-", max_score, " scale")) +
  theme_paper(10)
ggsave("outputs/inspection/sum-score-effect-size-inspection.pdf", p_effect, width = 8, height = 4.5)
ggsave("outputs/inspection/sum-score-effect-size-inspection.png", p_effect, width = 8, height = 4.5, dpi = 300)

p_thresholds <- ggplot(threshold_data, aes(item, threshold, shape = factor(threshold_number))) +
  geom_point(size = 1.8) +
  facet_wrap(~ scenario) +
  labs(title = "Inspection: item thresholds by scenario", x = "Item", y = "Threshold", shape = "Threshold") +
  theme_paper(10)
ggsave("outputs/inspection/sum-score-thresholds-inspection.pdf", p_thresholds, width = 8, height = 4.5)
ggsave("outputs/inspection/sum-score-thresholds-inspection.png", p_thresholds, width = 8, height = 4.5, dpi = 300)

settings <- list(N = N, J = J, item_max = item_max, max_score = max_score, theta_sd = theta_sd, beta_x = beta_x,
  beta_group = beta_group, beta_x_group = beta_x_group, x_low = x_low, x_high = x_high,
  B = B, n_cores = n_cores, alpha = alpha, seed = seed)
saveRDS(list(settings = settings, scenarios = scenarios, scenario_table = scenario_table, scenario_plot_data = plot_grid,
             threshold_data = threshold_data, example_dataset = example_data,
             simulation_results = simulation_results, simulation_summary = simulation_summary),
        "outputs/simulation-sum-scores.rds")
cat("\nDone.\n")
