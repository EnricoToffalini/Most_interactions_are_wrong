# Simulation 1: forced-choice accuracy with a non-zero chance floor.
# Data are generated with NO age-by-group product term on the conditional
# chance-corrected logit scale (subject random intercept held fixed).
# The chance-corrected model gives nominal rejection rates; the other models
# give pseudo-interaction detection rates.
# Run from the repository root. N_SIM, N_CORES, ALPHA can be set as environment variables.

rm(list = ls())
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(ggplot2)
library(lme4)
library(psyphy)
library(parallel)

B <- as.integer(Sys.getenv("N_SIM", "3000")) # replications per scenario (3000 for the paper)
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
seed <- 20260525
set.seed(seed)
for (path in c("tables", "figs", "outputs/inspection")) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# scenario parameters
N <- 250
k_trials <- 20
chance <- 0.50
age_range <- c(6, 10)
age_center <- 8
beta_age <- 0.60
beta_group <- -0.90
beta_age_group <- 0
target_icc <- 0.30
sigma_u <- sqrt(target_icc * (pi^2 / 3) / (1 - target_icc))

scenarios <- data.frame(
  scenario = c("Lower performance", "Middle performance", "Higher performance"),
  beta_intercept = c(-0.80, 0.00, 0.80),
  interpretation = c(
    "Predicted accuracies are closer to the .50 chance floor, so observed-scale compression is more visible.",
    "Predicted accuracies mostly remain in the middle of the admissible above-chance range.",
    "Predicted accuracies are higher, so the chance-floor problem is less dominant but the link still matters."
  )
)
model_names <- c("Gaussian identity", "Standard binomial logit", "Standard binomial probit", "Chance-corrected binomial logit")

# expected accuracy at random intercept = 0
expected_accuracy <- function(age, group_num, beta_intercept) {
  age_c <- age - age_center
  eta <- beta_intercept + beta_age * age_c + beta_group * group_num + beta_age_group * age_c * group_num
  chance + (1 - chance) * plogis(eta)
}

cat("\nSimulation 1: forced-choice accuracy with chance floor\n")
print(scenarios[, 1:2])

####################################################
# Deterministic scenario table and plotting grid
####################################################

implied_values <- derived_contrasts <- plot_grid <- NULL
for (i in 1:nrow(scenarios)) {
  b0 <- scenarios$beta_intercept[i]
  g <- expand.grid(age = c(age_range[1], age_center, age_range[2]), group_num = c(0, 1))
  g$eta <- b0 + beta_age * (g$age - age_center) + beta_group * g$group_num + beta_age_group * (g$age - age_center) * g$group_num
  g$p <- chance + (1 - chance) * plogis(g$eta)
  implied_values <- rbind(implied_values, data.frame(table_part = "implied_values", scenario = scenarios$scenario[i], age = g$age,
    group = paste("Group", g$group_num), linear_predictor = g$eta, expected_accuracy = g$p,
    expected_correct_out_of_k_trials = g$p * k_trials, curve_condition = "random intercept = 0",
    contrast = NA, value_probability_points = NA, value_correct_out_of_k_trials = NA, link_scale_value = NA))

  # group gaps are Group 1 minus Group 0; changes are oldest minus youngest
  p00 <- expected_accuracy(age_range[1], 0, b0)
  p01 <- expected_accuracy(age_range[1], 1, b0)
  p10 <- expected_accuracy(age_range[2], 0, b0)
  p11 <- expected_accuracy(age_range[2], 1, b0)
  values <- c(p01 - p00, p11 - p10, p10 - p00, p11 - p01, (p11 - p10) - (p01 - p00), NA)
  derived_contrasts <- rbind(derived_contrasts, data.frame(table_part = "derived_contrasts", scenario = scenarios$scenario[i], age = NA,
    group = NA, linear_predictor = NA, expected_accuracy = NA, expected_correct_out_of_k_trials = NA, curve_condition = NA,
    contrast = c("Group difference at youngest age: Group 1 minus Group 0",
                 "Group difference at oldest age: Group 1 minus Group 0",
                 "Age-related change in Group 0: oldest minus youngest",
                 "Age-related change in Group 1: oldest minus youngest",
                 "Change in group difference from youngest to oldest age",
                 "Generating link-scale age-by-group product term"),
    value_probability_points = values, value_correct_out_of_k_trials = values * k_trials,
    link_scale_value = c(NA, NA, NA, NA, NA, beta_age_group)))

  g <- expand.grid(age = seq(age_range[1], age_range[2], length.out = 200), group_num = c(0, 1))
  g$scenario <- scenarios$scenario[i]
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$expected_accuracy <- expected_accuracy(g$age, g$group_num, b0)
  plot_grid <- rbind(plot_grid, g)
}
plot_grid$scenario <- factor(plot_grid$scenario, levels = scenarios$scenario)
scenario_table <- rbind(implied_values, derived_contrasts)
write.csv(scenario_table, "tables/scenario-table-forced-choice.csv", row.names = FALSE)
print(scenario_table[, c("scenario", "age", "group", "expected_accuracy", "contrast", "value_probability_points")])

####################################################
# One example dataset per scenario
####################################################

example_data <- NULL
for (i in 1:nrow(scenarios)) {
  group_num <- rbinom(N, 1, 0.5)
  age <- runif(N, age_range[1], age_range[2])
  u <- rnorm(N, 0, sigma_u)
  eta <- scenarios$beta_intercept[i] + beta_age * (age - age_center) + beta_group * group_num +
    beta_age_group * (age - age_center) * group_num + u
  d <- data.frame(scenario = scenarios$scenario[i], id = factor(rep(1:N, each = k_trials)),
    age = rep(age, each = k_trials), group = factor(rep(group_num, each = k_trials), levels = c(0, 1), labels = c("Group 0", "Group 1")))
  d$correct <- rbinom(nrow(d), 1, chance + (1 - chance) * plogis(rep(eta, each = k_trials)))
  example_data <- rbind(example_data, d)
}
example_data$scenario <- factor(example_data$scenario, levels = scenarios$scenario)
cat("\nObserved mean accuracy in one example dataset per scenario:\n")
print(aggregate(correct ~ scenario + group, example_data, mean))

####################################################
# Monte Carlo simulation
####################################################

# Fits one model and returns the interaction test plus minimal fit-quality flags.
# Warnings are collected (not printed) and errors return NA, so that one bad
# replication does not stop a long parallel run.
fit_check <- function(fit_expr, nd) {
  warn <- character()
  out <- tryCatch(withCallingHandlers({
    fit <- fit_expr # the model is actually fitted here (lazy evaluation)
    list(fit = fit, tab = coef(summary(fit)))
  }, warning = function(w) {
    warn <<- c(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) conditionMessage(e))
  warn <- unique(warn)

  if (is.character(out)) {
    return(data.frame(interaction_coef = NA, interaction_se = NA, p_value = NA, fit_problem = TRUE,
      problem_message = out, singular = NA, warning_message = paste(warn, collapse = " | "),
      change_in_group_difference_response_scale = NA))
  }

  fit <- out$fit
  term <- "age_c:groupGroup 1"
  est <- if (term %in% rownames(out$tab)) out$tab[term, 1] else NA
  se <- if (term %in% rownames(out$tab)) out$tab[term, 2] else NA
  p <- if (is.finite(est) && is.finite(se) && se > 0) 2 * pnorm(abs(est / se), lower.tail = FALSE) else NA
  pred <- try(predict(fit, newdata = nd, type = "response", re.form = NA), silent = TRUE)
  did <- if (inherits(pred, "try-error") || any(!is.finite(pred))) NA else (pred[4] - pred[2]) - (pred[3] - pred[1])
  singular <- isSingular(fit, tol = 1e-4)
  problems <- c(fit@optinfo$conv$lme4$messages,
                if (singular) "singular fit",
                grep("failed to converge|identif|Hessian|positive definite|degenerate", warn, ignore.case = TRUE, value = TRUE),
                if (!is.finite(p)) "non-finite or non-positive interaction inference")
  data.frame(interaction_coef = unname(est), interaction_se = unname(se), p_value = unname(p),
    fit_problem = length(problems) > 0, problem_message = paste(unique(problems), collapse = " | "),
    singular = singular, warning_message = paste(warn, collapse = " | "),
    change_in_group_difference_response_scale = unname(did))
}

# One replication of scenario i: trial-level DGP and four random-intercept models
sim_one <- function(b, i) {
  set.seed(seed + 100000 * i + b)
  group_num <- rbinom(N, 1, 0.5)
  age <- runif(N, age_range[1], age_range[2])
  age_c <- age - age_center
  u <- rnorm(N, 0, sigma_u)
  eta <- scenarios$beta_intercept[i] + beta_age * age_c + beta_group * group_num + beta_age_group * age_c * group_num + u
  d <- data.frame(id = factor(rep(1:N, each = k_trials)), age_c = rep(age_c, each = k_trials),
    group = factor(rep(group_num, each = k_trials), levels = c(0, 1), labels = c("Group 0", "Group 1")))
  d$correct <- rbinom(nrow(d), 1, chance + (1 - chance) * plogis(rep(eta, each = k_trials)))

  # youngest and oldest age in each group, for the response-scale difference in differences
  nd <- expand.grid(age = age_range, group = factor(c("Group 0", "Group 1")))
  nd$age_c <- nd$age - age_center
  res <- rbind(
    fit_check(lmer(correct ~ age_c * group + (1 | id), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial("logit"), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial("probit"), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial(mafc.logit(round(1 / chance))), data = d), nd)
  )
  res <- cbind(scenario = scenarios$scenario[i], replication = b, model = model_names, res)
  res$change_in_group_difference_outcome_units <- res$change_in_group_difference_response_scale * k_trials
  res
}

cat("\nRunning B =", B, "replications per scenario on", n_cores, "cores\n")
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, {library(lme4); library(psyphy)}))
clusterExport(cl, c("fit_check", "seed", "scenarios", "model_names", "N", "k_trials", "chance", "age_range",
                    "age_center", "beta_age", "beta_group", "beta_age_group", "sigma_u"))
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

wilson <- function(x, n) {
  if (n == 0) return(c(NA, NA))
  z <- qnorm(0.975)
  p <- x / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  c(center - half, center + half)
}

# "ok" = finite p-value from a fit passing the minimal lme4 checks (primary);
# "problem" = finite p-value from a flagged fit
simulation_summary <- NULL
for (s in scenarios$scenario) {
  for (m in model_names) {
    dat <- simulation_results[simulation_results$scenario == s & simulation_results$model == m, ]
    ok <- is.finite(dat$p_value) & !dat$fit_problem
    flagged <- is.finite(dat$p_value) & dat$fit_problem
    n_ok <- sum(ok)
    n_problem <- sum(flagged)
    reject_ok <- sum(dat$p_value[ok] < alpha)
    reject_problem <- sum(dat$p_value[flagged] < alpha)
    ci_ok <- wilson(reject_ok, n_ok)
    ci_problem <- wilson(reject_problem, n_problem)
    coefs <- dat$interaction_coef[is.finite(dat$interaction_coef)]
    did <- dat$change_in_group_difference_outcome_units[is.finite(dat$change_in_group_difference_outcome_units)]
    simulation_summary <- rbind(simulation_summary, data.frame(
      scenario = s, model = m, n_total = nrow(dat),
      n_fit_ok = sum(!dat$fit_problem), n_fit_problem = sum(dat$fit_problem), fit_problem_rate = mean(dat$fit_problem),
      n_p_finite_ok = n_ok, n_rejections_ok = reject_ok, rejection_rate_ok = if (n_ok > 0) reject_ok / n_ok else NA,
      ci_low_ok = ci_ok[1], ci_high_ok = ci_ok[2],
      n_p_finite_problem = n_problem, n_rejections_problem = reject_problem,
      rejection_rate_problem = if (n_problem > 0) reject_problem / n_problem else NA,
      ci_low_problem = ci_problem[1], ci_high_problem = ci_problem[2],
      median_interaction_coef = if (length(coefs)) median(coefs) else NA,
      mean_interaction_coef = if (length(coefs)) mean(coefs) else NA,
      sd_interaction_coef = if (length(coefs) > 1) sd(coefs) else NA,
      median_change_in_group_difference_outcome_units = if (length(did)) median(did) else NA,
      mean_change_in_group_difference_outcome_units = if (length(did)) mean(did) else NA,
      sd_change_in_group_difference_outcome_units = if (length(did) > 1) sd(did) else NA))
  }
}
simulation_summary$scenario <- factor(simulation_summary$scenario, levels = scenarios$scenario)
simulation_summary$model <- factor(simulation_summary$model, levels = model_names)
simulation_summary$rate_type <- ifelse(simulation_summary$model == "Chance-corrected binomial logit",
  "Nominal rejection rate", "Pseudo-interaction detection rate")
write.csv(simulation_summary, "tables/simulation-summary-forced-choice.csv", row.names = FALSE)

print(simulation_summary[, c("scenario", "model", "n_p_finite_ok", "rejection_rate_ok", "fit_problem_rate",
                             "median_change_in_group_difference_outcome_units")])
cat("median_change_in_group_difference_outcome_units is the model-implied change in the group gap from age",
    age_range[1], "to", age_range[2], "in correct responses out of", k_trials, "trials (a contrast, not a count)\n")

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
model_shapes <- c("Gaussian identity" = 16, "Standard binomial logit" = 15, "Standard binomial probit" = 18, "Chance-corrected binomial logit" = 17)

# grey band below the chance floor; light lines at equal steps on the chance-corrected logit scale
pA <- ggplot(plot_grid, aes(age, expected_accuracy, linetype = group, color = group)) +
  annotate("rect", xmin = age_range[1], xmax = age_range[2], ymin = 0, ymax = chance, fill = "grey92") +
  geom_hline(yintercept = chance + (1 - chance) * plogis(-5:5), color = "grey82", linewidth = 0.35) +
  geom_hline(yintercept = chance, linetype = "dashed", color = "grey35") +
  geom_line(linewidth = 0.95) +
  annotate("text", x = age_range[2] - 0.05, y = chance + 0.025, label = sprintf("chance floor = %.2f", chance),
           hjust = 1, size = 3, color = "grey25") +
  facet_wrap(~ scenario) +
  coord_cartesian(ylim = c(chance - 0.05, 1)) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), breaks = seq(chance, 1, by = 0.10)) +
  scale_color_manual(values = c("Group 0" = "#0072B2", "Group 1" = "#D55E00"), name = NULL) +
  scale_linetype_manual(values = c("Group 0" = "solid", "Group 1" = "longdash"), name = NULL) +
  labs(title = "A. Scenario curves generated above a chance floor",
       subtitle = "Curves are conditional at random intercept = 0; horizontal lines mark equal link-scale steps",
       x = "Age", y = "Expected accuracy") +
  theme_paper(10) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

pB <- ggplot(simulation_summary, aes(x = model, y = rejection_rate_ok, shape = model)) +
  geom_hline(yintercept = alpha, linetype = "dashed", color = "grey35") +
  geom_pointrange(aes(ymin = ci_low_ok, ymax = ci_high_ok), linewidth = 0.45) +
  coord_flip() +
  facet_wrap(~ scenario) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_shape_manual(values = model_shapes) +
  labs(title = "B. Product-term rejection rate",
       subtitle = "Primary rates use fits passing minimal lme4 checks; dashed line: nominal alpha",
       x = NULL, y = "Replications rejecting the age-by-group product term") +
  theme_paper(9) +
  theme(legend.position = "none")

# panel A on top of panel B, each taking half of the page
for (ext in c("pdf", "png")) {
  if (ext == "pdf") pdf("figs/forced-choice-simulation.pdf", width = 7.2, height = 5.9)
  if (ext == "png") png("figs/forced-choice-simulation.png", width = 7.2, height = 5.9, units = "in", res = 300)
  grid::grid.newpage()
  print(pA, vp = grid::viewport(y = 0.75, height = 0.5))
  print(pB, vp = grid::viewport(y = 0.25, height = 0.5))
  dev.off()
}

# inspection only: median model-implied change in the group difference
p_effect <- ggplot(simulation_summary, aes(x = model, y = median_change_in_group_difference_outcome_units, shape = model)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  coord_flip() +
  facet_wrap(~ scenario) +
  scale_shape_manual(values = model_shapes) +
  labs(title = "Inspection: median model-implied change in the group difference",
       subtitle = "Values are contrasts, not possible observed counts", x = NULL,
       y = paste0("Change in group gap from ", age_range[1], " to ", age_range[2], ", correct responses out of ", k_trials)) +
  theme_paper(10) +
  theme(legend.position = "none")
ggsave("outputs/inspection/forced-choice-effect-size-inspection.pdf", p_effect, width = 8, height = 4.5)
ggsave("outputs/inspection/forced-choice-effect-size-inspection.png", p_effect, width = 8, height = 4.5, dpi = 300)

settings <- list(N = N, k_trials = k_trials, chance = chance, age_range = age_range, age_center = age_center,
  beta_age = beta_age, beta_group = beta_group, beta_age_group = beta_age_group, target_icc = target_icc,
  sigma_u = sigma_u, B = B, n_cores = n_cores, alpha = alpha, seed = seed)
saveRDS(list(settings = settings, scenarios = scenarios, scenario_table = scenario_table, scenario_plot_data = plot_grid,
             example_dataset = example_data, simulation_results = simulation_results, simulation_summary = simulation_summary),
        "outputs/simulation-forced-choice.rds")
cat("\nDone.\n")
