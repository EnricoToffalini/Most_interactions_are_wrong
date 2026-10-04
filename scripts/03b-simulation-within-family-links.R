# Simulation 2: logit vs probit within the binomial family, repeated binary trials.
# Data are generated with NO group-by-condition product term on the generating
# link scale. Logit coefficients are scaled (x 1.65) so that the logit DGP has
# cell probabilities close to the probit reference scenario.
# All fitted models are random-intercept GLMMs (glmmTMB).
# Run from the repository root. N_SIM, N_CORES, ALPHA can be set as environment variables.

rm(list = ls())
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(ggplot2)
library(glmmTMB)
library(parallel)

B <- as.integer(Sys.getenv("N_SIM", "3000")) # replications per generating link (3000 for the paper)
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
seed <- 20260528
set.seed(seed)
for (path in c("tables", "figs", "outputs/inspection")) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# scenario parameters (probit reference scale)
n_subjects <- 700
k_trials <- 15 # trials per subject and condition
target_icc <- 0.30
links <- c("logit", "probit")
logit_probit_scale <- 1.65

scenario_parameters <- data.frame(generating_link = links, coefficient_scale = c(logit_probit_scale, 1))
scenario_parameters$beta_intercept <- scenario_parameters$coefficient_scale * 1.50
scenario_parameters$beta_group <- scenario_parameters$coefficient_scale * -1.00
scenario_parameters$beta_condition <- scenario_parameters$coefficient_scale * -1.00
scenario_parameters$beta_group_condition <- scenario_parameters$coefficient_scale * 0
# latent residual variance is pi^2/3 for logit and 1 for probit
scenario_parameters$random_intercept_sd <- sqrt(target_icc * c(pi^2 / 3, 1) / (1 - target_icc))
link_labels <- c("Generated with Logit link", "Generated with Probit link")

cat("\nSimulation 2: logit vs probit within the binomial family\n")
print(scenario_parameters)

####################################################
# Deterministic cell probabilities and pseudo-interactions
####################################################

# product contrast in a 2 x 2 table ordered (g0c0, g1c0, g0c1, g1c1)
product_contrast <- function(v) v[4] - v[2] - v[3] + v[1]

cell_probability_table <- scenario_contrasts <- scenario_table <- NULL
for (i in 1:2) {
  par <- scenario_parameters[i, ]
  g <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  g$generating_link <- links[i]
  g$random_intercept_sd <- par$random_intercept_sd
  g$linear_predictor_random_intercept_0 <- par$beta_intercept + par$beta_group * g$group_num +
    par$beta_condition * g$condition_num + par$beta_group_condition * g$group_num * g$condition_num
  g$expected_probability_random_intercept_0 <- if (links[i] == "logit") plogis(g$linear_predictor_random_intercept_0) else pnorm(g$linear_predictor_random_intercept_0)
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$condition <- factor(g$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  g$generating_link_label <- link_labels[i]
  cell_probability_table <- rbind(cell_probability_table, g)

  # same probabilities read on each candidate link scale
  p <- g$expected_probability_random_intercept_0
  contrasts <- data.frame(generating_link = links[i], fitted_link = links,
    link_match = ifelse(links == links[i], "Matched link", "Wrong link"),
    random_intercept_sd = par$random_intercept_sd,
    deterministic_product_on_fitted_link_scale = c(product_contrast(qlogis(p)), product_contrast(qnorm(p))),
    deterministic_response_scale_difference_in_differences = product_contrast(p))
  scenario_contrasts <- rbind(scenario_contrasts, contrasts)

  # each cell crossed with both fitted links
  long <- cbind(g[rep(1:4, each = 2), ], contrasts[rep(1:2, times = 4), c("fitted_link", "link_match",
    "deterministic_product_on_fitted_link_scale", "deterministic_response_scale_difference_in_differences")])
  scenario_table <- rbind(scenario_table, long[, c("generating_link", "fitted_link", "link_match", "random_intercept_sd",
    "group", "condition", "linear_predictor_random_intercept_0", "expected_probability_random_intercept_0",
    "deterministic_product_on_fitted_link_scale", "deterministic_response_scale_difference_in_differences")])
}
write.csv(scenario_table, "tables/scenario-table-within-family-links.csv", row.names = FALSE)
print(cell_probability_table[, c("generating_link", "group", "condition", "expected_probability_random_intercept_0")])
print(scenario_contrasts)

####################################################
# Monte Carlo simulation
####################################################

# One replication with generating link i: individual binary trials, both GLMMs
sim_one <- function(b, i) {
  set.seed(seed + 100000 * i + b)
  par <- scenario_parameters[i, ]
  d <- data.frame(id = factor(rep(1:n_subjects, each = 2 * k_trials)),
    group_num = rep(rep(c(0, 1), each = n_subjects / 2), each = 2 * k_trials),
    condition_num = rep(rep(c(0, 1), each = k_trials), times = n_subjects))
  u <- rnorm(n_subjects, 0, par$random_intercept_sd)
  eta <- par$beta_intercept + par$beta_group * d$group_num + par$beta_condition * d$condition_num +
    par$beta_group_condition * d$group_num * d$condition_num + u[as.integer(d$id)]
  d$y <- rbinom(nrow(d), 1, if (links[i] == "logit") plogis(eta) else pnorm(eta))
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))

  res <- data.frame(generating_link = links[i], fitted_link = links,
    link_match = ifelse(links == links[i], "Matched link", "Wrong link"), replication = b,
    interaction_coef = NA, interaction_se = NA, p_value = NA)
  for (j in 1:2) {
    fit <- try(glmmTMB(y ~ group * condition + (1 | id), data = d, family = binomial(links[j])), silent = TRUE)
    if (inherits(fit, "try-error")) next
    tab <- summary(fit)$coefficients$cond
    term <- "groupGroup 1:conditionCondition 1"
    if (term %in% rownames(tab)) res[j, c("interaction_coef", "interaction_se", "p_value")] <- tab[term, c(1, 2, 4)]
  }
  res
}

cat("\nRunning B =", B, "replications per generating link on", n_cores, "cores\n")
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(glmmTMB)))
clusterExport(cl, c("seed", "scenario_parameters", "links", "n_subjects", "k_trials"))
results <- list()
for (i in 1:2) {
  cat("Generating link:", links[i], "\n")
  results[[i]] <- do.call(rbind, parLapply(cl, 1:B, sim_one, i = i))
}
stopCluster(cl)
simulation_results <- do.call(rbind, results)

####################################################
# Summary
####################################################

simulation_summary <- NULL
for (gen in links) {
  for (fitted in links) {
    dat <- simulation_results[simulation_results$generating_link == gen & simulation_results$fitted_link == fitted, ]
    n <- sum(!is.na(dat$p_value))
    x <- sum(dat$p_value < alpha, na.rm = TRUE)
    # Wilson 95% interval
    z <- qnorm(0.975)
    rate <- if (n > 0) x / n else NA
    center <- (rate + z^2 / (2 * n)) / (1 + z^2 / n)
    half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
    contr <- scenario_contrasts[scenario_contrasts$generating_link == gen & scenario_contrasts$fitted_link == fitted, ]
    simulation_summary <- rbind(simulation_summary, data.frame(generating_link = gen, fitted_link = fitted,
      link_match = dat$link_match[1], n_successful_fits = n, n_rejections = x, rejection_rate = rate,
      ci_low = center - half, ci_high = center + half,
      median_interaction_coef = median(dat$interaction_coef, na.rm = TRUE),
      median_interaction_se = median(dat$interaction_se, na.rm = TRUE),
      deterministic_product_on_fitted_link_scale = contr$deterministic_product_on_fitted_link_scale,
      deterministic_response_scale_difference_in_differences = contr$deterministic_response_scale_difference_in_differences,
      rate_type = if (gen == fitted) "Nominal rejection rate" else "Pseudo-interaction detection rate"))
  }
}
write.csv(simulation_summary, "tables/simulation-summary-within-family-links.csv", row.names = FALSE)
print(simulation_summary[, c("generating_link", "fitted_link", "n_successful_fits", "rejection_rate", "ci_low", "ci_high")])

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

cell_probability_table$generating_link_label <- factor(cell_probability_table$generating_link_label, levels = link_labels)
# light lines at equal steps on each generating-link scale
link_grid <- rbind(
  data.frame(generating_link_label = link_labels[1], yintercept = plogis(seq(-3 * logit_probit_scale, 3 * logit_probit_scale, by = logit_probit_scale / 2))),
  data.frame(generating_link_label = link_labels[2], yintercept = pnorm(seq(-3, 3, by = 0.5))))
link_grid$generating_link_label <- factor(link_grid$generating_link_label, levels = link_labels)

p_scenario <- ggplot(cell_probability_table, aes(x = condition, y = expected_probability_random_intercept_0, color = group, linetype = group, group = group)) +
  geom_hline(data = link_grid, aes(yintercept = yintercept), color = "grey82", linewidth = 0.35) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ generating_link_label, nrow = 1) +
  scale_y_continuous(limits = c(0, 1), labels = function(x) paste0(round(100 * x), "%")) +
  scale_color_manual(values = c("Group 0" = "#0072B2", "Group 1" = "#D55E00"), name = NULL) +
  scale_linetype_manual(values = c("Group 0" = "solid", "Group 1" = "longdash"), name = NULL) +
  labs(title = "A. Generated cell probabilities", subtitle = "Horizontal lines mark equal steps on each generating-link scale",
       x = NULL, y = "Expected probability, random intercept = 0") +
  theme_paper(9) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

simulation_summary$generating_link_label <- factor(link_labels[match(simulation_summary$generating_link, links)], levels = link_labels)
simulation_summary$fitted_link_label <- factor(ifelse(simulation_summary$fitted_link == "logit", "Logit", "Probit"))
p_rejection <- ggplot(simulation_summary, aes(x = fitted_link_label, y = rejection_rate, color = link_match, shape = link_match)) +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  geom_pointrange(aes(ymin = ci_low, ymax = ci_high), linewidth = 0.45) +
  facet_wrap(~ generating_link_label, nrow = 1) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  scale_color_manual(values = c("Matched link" = "grey30", "Wrong link" = "#D55E00"), name = NULL) +
  scale_shape_manual(values = c("Matched link" = 16, "Wrong link" = 17), name = NULL) +
  labs(title = "B. Product-term rejection rate",
       subtitle = "Matched links: nominal rejection; wrong links: pseudo-interaction detection. Dashed line: nominal alpha",
       x = "Fitted link", y = "Rejection rate") +
  theme_paper(9)

for (ext in c("pdf", "png")) {
  if (ext == "pdf") pdf("figs/within-family-links.pdf", width = 7.2, height = 6.5)
  if (ext == "png") png("figs/within-family-links.png", width = 7.2, height = 6.5, units = "in", res = 300)
  grid::grid.newpage()
  print(p_scenario, vp = grid::viewport(y = 0.75, height = 0.5))
  print(p_rejection, vp = grid::viewport(y = 0.25, height = 0.5))
  dev.off()
}

# inspection: deterministic pseudo-interaction implied by reading one link's probabilities on the other link
scenario_contrasts$generating_link_label <- factor(link_labels[match(scenario_contrasts$generating_link, links)], levels = link_labels)
scenario_contrasts$fitted_link_label <- factor(ifelse(scenario_contrasts$fitted_link == "logit", "Logit", "Probit"))
p_pseudo <- ggplot(scenario_contrasts, aes(x = fitted_link_label, y = deterministic_product_on_fitted_link_scale, color = link_match, shape = link_match)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2.2) +
  facet_wrap(~ generating_link_label, nrow = 1) +
  scale_color_manual(values = c("Matched link" = "grey30", "Wrong link" = "#D55E00"), name = NULL) +
  scale_shape_manual(values = c("Matched link" = 16, "Wrong link" = 17), name = NULL) +
  labs(title = "Deterministic pseudo-interaction induced by link crossing",
       subtitle = "Matched-link contrasts are zero by construction; wrong-link contrasts can be nonzero",
       x = "Fitted link", y = "Product contrast on fitted link scale") +
  theme_paper(9)
ggsave("outputs/inspection/within-family-link-pseudo-interaction.pdf", p_pseudo, width = 7.2, height = 3.8)
ggsave("outputs/inspection/within-family-link-pseudo-interaction.png", p_pseudo, width = 7.2, height = 3.8, dpi = 300)

settings <- list(n_subjects = n_subjects, k_trials = k_trials, target_icc = target_icc,
  logit_probit_scale = logit_probit_scale, B = B, n_cores = n_cores, alpha = alpha, seed = seed)
saveRDS(list(settings = settings, scenario_parameters = scenario_parameters, cell_probability_table = cell_probability_table,
             scenario_contrasts = scenario_contrasts, scenario_table = scenario_table,
             simulation_results = simulation_results, simulation_summary = simulation_summary),
        "outputs/simulation-within-family-links.rds")
cat("\nDone.\n")
