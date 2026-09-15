# scripts/03b-simulation-within-family-links.R
# Simulation 3: within-family link choice with repeated binary trials.
# Logit coefficients are scaled so the logit DGP has cell probabilities close
# to the probit reference scenario, while preserving zero product term on the
# generating link scale.

rm(list = ls())

# Run from the repository root. Publication runs use 3000 replications.
B <- as.integer(Sys.getenv("N_SIM", "3000"))
default_alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
default_dpi <- 300
figure_width <- 7.2
figure_height <- 7.0

source("R/utils-plots.R")

library(glmmTMB)
library(ggplot2)

for (path in c("tables", "figs", "outputs", "outputs/inspection")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
set.seed(20260528)
cat("\n", "Simulation 3: logit vs probit within the binomial family", "\n")

# ---------------------------------------------------------------------
# 1. User-tunable settings
# ---------------------------------------------------------------------
settings <- list(
  n_subjects = 700,
  k_trials = 15,
  target_icc = 0.30,
  reference_beta_intercept = 1.50,
  reference_beta_group = -1.00,
  reference_beta_condition = -1.00,
  reference_beta_group_condition = 0,
  logit_probit_scale = 1.65,
  candidate_links = c("logit", "probit"),
  B = B,
  n_cores = as.integer(Sys.getenv(
    "N_CORES",
    Sys.getenv("SLURM_CPUS_PER_TASK", max(1, parallel::detectCores() - 1))
  )),
  alpha = default_alpha,
  scenario_table_path = "tables/scenario-table-within-family-links.csv",
  simulation_summary_path = "tables/simulation-summary-within-family-links.csv",
  figure_base = "figs/within-family-links",
  inspection_pseudo_base = "outputs/inspection/within-family-link-pseudo-interaction",
  rds_path = "outputs/simulation-within-family-links.rds"
)

cat("\n", "Simulation settings", "\n")
print(settings)
cat("\nThe true condition-by-group product term is zero on the generating link scale.\n")
cat("Logit coefficients are scaled to make the logit DGP close to the probit reference scenario.\n")
cat("All fitted models are random-intercept GLMMs fit with glmmTMB.\n")

# ---------------------------------------------------------------------
# 2. Links, scenario parameters, and deterministic quantities
# ---------------------------------------------------------------------
inv_link <- function(eta, link) {
  if (link == "logit") return(stats::plogis(eta))
  if (link == "probit") return(stats::pnorm(eta))
  stop("Unknown link.", call. = FALSE)
}

link_fun <- function(p, link) {
  eps <- sqrt(.Machine$double.eps)
  p <- pmin(pmax(p, eps), 1 - eps)
  if (link == "logit") return(stats::qlogis(p))
  if (link == "probit") return(stats::qnorm(p))
  stop("Unknown link.", call. = FALSE)
}

link_label <- function(link) ifelse(link == "logit", "Logit", "Probit")



scenario_parameters <- data.frame(
  generating_link = settings$candidate_links,
  coefficient_scale = ifelse(settings$candidate_links == "logit", settings$logit_probit_scale, 1),
  stringsAsFactors = FALSE
)
for (nm in c("intercept", "group", "condition", "group_condition")) {
  scenario_parameters[[paste0("beta_", nm)]] <-
    scenario_parameters$coefficient_scale * settings[[paste0("reference_beta_", nm)]]
}





interaction_contrast <- function(value, group_num, condition_num) {
  unname(
    value[group_num == 1 & condition_num == 1] - value[group_num == 1 & condition_num == 0] -
      value[group_num == 0 & condition_num == 1] + value[group_num == 0 & condition_num == 0]
  )
}





cell_rows <- contrast_rows <- list()
for (generating_link in settings$candidate_links) {
  p <- scenario_parameters[scenario_parameters$generating_link == generating_link, ]
  g <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  g$generating_link <- generating_link
  residual_variance <- if (generating_link == "logit") pi^2 / 3 else 1
  g$random_intercept_sd <- sqrt(settings$target_icc * residual_variance / (1 - settings$target_icc))
  g$linear_predictor_random_intercept_0 <- p$beta_intercept +
    p$beta_group * g$group_num + p$beta_condition * g$condition_num +
    p$beta_group_condition * g$group_num * g$condition_num
  g$expected_probability_random_intercept_0 <- inv_link(g$linear_predictor_random_intercept_0, generating_link)
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$condition <- factor(g$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  g$generating_link_label <- paste0("Generated with ", link_label(generating_link), " link")
  cell_rows[[length(cell_rows) + 1L]] <- g
  for (fitted_link in settings$candidate_links) {
    fitted_scale_value <- link_fun(g$expected_probability_random_intercept_0, fitted_link)
    contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
      generating_link = generating_link, fitted_link = fitted_link,
      link_match = ifelse(fitted_link == generating_link, "Matched link", "Wrong link"),
      random_intercept_sd = g$random_intercept_sd[1],
      deterministic_product_on_fitted_link_scale = interaction_contrast(fitted_scale_value, g$group_num, g$condition_num),
      deterministic_response_scale_difference_in_differences = interaction_contrast(g$expected_probability_random_intercept_0, g$group_num, g$condition_num),
      stringsAsFactors = FALSE)
  }
}
cell_probability_table <- do.call(rbind, cell_rows)
scenario_contrasts <- do.call(rbind, contrast_rows)
scenario_table <- merge(cell_probability_table, scenario_contrasts, by = c("generating_link", "random_intercept_sd"), all.x = TRUE, sort = FALSE)
scenario_table <- scenario_table[, c(
  "generating_link", "fitted_link", "link_match", "random_intercept_sd", "group", "condition",
  "linear_predictor_random_intercept_0", "expected_probability_random_intercept_0",
  "deterministic_product_on_fitted_link_scale", "deterministic_response_scale_difference_in_differences"
)]
utils::write.csv(scenario_table, settings$scenario_table_path, row.names = FALSE)

cat("\n", "Scenario parameters after link-specific scaling", "\n")
print(scenario_parameters)
cat("\n", "Cell probabilities at random intercept = 0", "\n")
print(cell_probability_table[, c(
  "generating_link", "random_intercept_sd", "group", "condition",
  "linear_predictor_random_intercept_0", "expected_probability_random_intercept_0"
)])
cat("\n", "Deterministic pseudo-interaction implied by link crossing", "\n")
print(scenario_contrasts)

# ---------------------------------------------------------------------
# 3. Data generation and model fitting
# ---------------------------------------------------------------------












# ---------------------------------------------------------------------
# 4. Monte Carlo simulation
# ---------------------------------------------------------------------
cat("\n", "Monte Carlo simulation", "\n")
cat("Running B = ", settings$B, " replications per generating-link cell.\n", sep = "")
cat("Using n_cores = ", settings$n_cores, ".\n", sep = "")

run_one_replication <- function(b, generating_link) {
  p <- scenario_parameters[scenario_parameters$generating_link == generating_link, ]
  n_per_group <- settings$n_subjects / 2
  id <- rep(seq_len(settings$n_subjects), each = 2 * settings$k_trials)
  group_by_subject <- rep(c(0, 1), each = n_per_group)
  d <- data.frame(
    id = factor(id),
    group_num = rep(group_by_subject, each = 2 * settings$k_trials),
    condition_num = rep(rep(c(0, 1), each = settings$k_trials), times = settings$n_subjects))
  residual_variance <- if (generating_link == "logit") pi^2 / 3 else 1
  u_sd <- sqrt(settings$target_icc * residual_variance / (1 - settings$target_icc))
  u <- stats::rnorm(settings$n_subjects, 0, u_sd)
  eta <- p$beta_intercept + p$beta_group * d$group_num +
    p$beta_condition * d$condition_num + p$beta_group_condition * d$group_num * d$condition_num +
    u[as.integer(d$id)]
  probability <- if (generating_link == "logit") stats::plogis(eta) else stats::pnorm(eta)
  d$y <- stats::rbinom(nrow(d), 1, probability)
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  fit_logit <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id), data = d,
                                 family = stats::binomial("logit")), silent = TRUE)
  fit_probit <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id), data = d,
                                  family = stats::binomial("probit")), silent = TRUE)
  coefficients <- standard_errors <- p_values <- rep(NA_real_, 2)
  if (!inherits(fit_logit, "try-error")) {
    sm <- summary(fit_logit)$coefficients$cond
    if ("groupGroup 1:conditionCondition 1" %in% rownames(sm)) {
      coefficients[1] <- sm["groupGroup 1:conditionCondition 1", "Estimate"]
      standard_errors[1] <- sm["groupGroup 1:conditionCondition 1", "Std. Error"]
      p_values[1] <- sm["groupGroup 1:conditionCondition 1", "Pr(>|z|)"]
    }
  }
  if (!inherits(fit_probit, "try-error")) {
    sm <- summary(fit_probit)$coefficients$cond
    if ("groupGroup 1:conditionCondition 1" %in% rownames(sm)) {
      coefficients[2] <- sm["groupGroup 1:conditionCondition 1", "Estimate"]
      standard_errors[2] <- sm["groupGroup 1:conditionCondition 1", "Std. Error"]
      p_values[2] <- sm["groupGroup 1:conditionCondition 1", "Pr(>|z|)"]
    }
  }
  data.frame(generating_link = generating_link, fitted_link = c("logit", "probit"),
             link_match = ifelse(c("logit", "probit") == generating_link, "Matched link", "Wrong link"),
             replication = b, interaction_coef = coefficients,
             interaction_se = standard_errors, p_value = p_values, stringsAsFactors = FALSE)
}



Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

# N_CORES overrides the SLURM allocation; otherwise use SLURM_CPUS_PER_TASK.
# Parallelize replications only. Each worker uses one BLAS/OpenMP thread.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
cluster <- NULL
if (settings$n_cores > 1 && .Platform$OS.type != "unix") {
  cluster <- parallel::makeCluster(settings$n_cores)
  parallel::clusterSetRNGStream(cluster, iseed = 20260528)
  parallel::clusterExport(cluster, c("settings", "scenario_parameters"))
}
scenario_results <- list()
for (generating_link in settings$candidate_links) {
  if (settings$n_cores > 1 && .Platform$OS.type == "unix") {
    replications <- parallel::mclapply(seq_len(settings$B), run_one_replication,
      generating_link = generating_link, mc.cores = settings$n_cores, mc.set.seed = TRUE)
  } else if (!is.null(cluster)) {
    replications <- parallel::parLapply(cluster, seq_len(settings$B), run_one_replication,
      generating_link = generating_link)
  } else {
    replications <- lapply(seq_len(settings$B), run_one_replication, generating_link = generating_link)
  }
  scenario_results[[length(scenario_results) + 1L]] <- do.call(rbind, replications)
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
simulation_results <- do.call(rbind, scenario_results)

simulation_summary <- do.call(rbind, lapply(
  split(simulation_results, list(simulation_results$generating_link, simulation_results$fitted_link), drop = TRUE),
  function(dat) {
    ok <- !is.na(dat$p_value)
    n <- sum(ok)
    n_rejections <- sum(dat$p_value[ok] < settings$alpha)
    rate <- n_rejections / n
    z <- stats::qnorm(0.975)
    denom <- 1 + z^2 / n
    center <- (rate + z^2 / (2 * n)) / denom
    half <- z * sqrt((rate * (1 - rate) + z^2 / (4 * n)) / n) / denom
    ci <- if (n == 0) c(NA_real_, NA_real_) else c(center - half, center + half)
    data.frame(generating_link = dat$generating_link[1], fitted_link = dat$fitted_link[1],
      link_match = dat$link_match[1], n_successful_fits = n, n_rejections = n_rejections,
      rejection_rate = rate, ci_low = ci[1], ci_high = ci[2],
      median_interaction_coef = stats::median(dat$interaction_coef, na.rm = TRUE),
      median_interaction_se = stats::median(dat$interaction_se, na.rm = TRUE), stringsAsFactors = FALSE)
  }
))

simulation_summary <- merge(
  simulation_summary,
  scenario_contrasts[, c(
    "generating_link", "fitted_link", "deterministic_product_on_fitted_link_scale",
    "deterministic_response_scale_difference_in_differences"
  )],
  by = c("generating_link", "fitted_link"),
  all.x = TRUE,
  sort = FALSE
)
simulation_summary$generating_link <- factor(simulation_summary$generating_link, levels = settings$candidate_links)
simulation_summary$fitted_link <- factor(simulation_summary$fitted_link, levels = settings$candidate_links)
simulation_summary$link_match <- factor(simulation_summary$link_match, levels = c("Matched link", "Wrong link"))
simulation_summary$rate_type <- ifelse(
  as.character(simulation_summary$fitted_link) == as.character(simulation_summary$generating_link),
  "Nominal rejection rate",
  "Pseudo-interaction detection rate"
)
simulation_summary <- simulation_summary[order(simulation_summary$generating_link, simulation_summary$fitted_link), ]
utils::write.csv(simulation_summary, settings$simulation_summary_path, row.names = FALSE)

cat("\n", "Simulation summary", "\n")
print(simulation_summary)

# ---------------------------------------------------------------------
# 5. Figure and inspection plot
# ---------------------------------------------------------------------
cell_plot_data <- unique(cell_probability_table[, c("generating_link_label", "group", "condition", "expected_probability_random_intercept_0")])
cell_plot_data$generating_link_label <- factor(cell_plot_data$generating_link_label, levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))

link_grid <- do.call(
  rbind,
  lapply(settings$candidate_links, function(link) {
    
    coefficient_scale <- scenario_parameters$coefficient_scale[scenario_parameters$generating_link == link]
    eta_step <- coefficient_scale / 2
    
    eta_values <- seq(
      from = -3 * coefficient_scale,
      to   =  3 * coefficient_scale,
      by   = eta_step
    )
    
    data.frame(
      generating_link_label = paste0(
        "Generated with ", link_label(link), " link"
      ),
      yintercept = inv_link(eta_values, link)
    )
  })
)

link_grid$generating_link_label <- factor(
  link_grid$generating_link_label,
  levels = paste0(
    "Generated with ",
    link_label(settings$candidate_links),
    " link"
  )
)

p_scenario <- ggplot2::ggplot(
  cell_plot_data,
  ggplot2::aes(x = condition, y = expected_probability_random_intercept_0, color = group, linetype = group, group = group)
) +
  ggplot2::geom_hline(
    data = link_grid,
    ggplot2::aes(yintercept = yintercept),
    inherit.aes = FALSE,
    color = "grey82",
    linewidth = 0.35
  ) +
  ggplot2::geom_line(linewidth = .9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = percent_labels()) +
  link_scale_color_discrete(name = NULL) +
  link_scale_linetype_discrete(name = NULL) +
  ggplot2::labs(
    title = "A. Generated cell probabilities",
    subtitle = "Horizontal lines mark equal steps on each generating-link scale",
    x = NULL,
    y = "Expected probability, random intercept = 0"
  ) +
  link_theme(base_size = 9) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )

plot_summary <- simulation_summary
plot_summary$generating_link_label <- factor(paste0("Generated with ", link_label(as.character(plot_summary$generating_link)), " link"), levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))
plot_summary$fitted_link_label <- factor(link_label(as.character(plot_summary$fitted_link)), levels = link_label(settings$candidate_links))

match_scales <- list(
  ggplot2::scale_color_manual(values = c("Matched link" = "grey30", "Wrong link" = "#D55E00"), name = NULL),
  ggplot2::scale_shape_manual(values = c("Matched link" = 16, "Wrong link" = 17), name = NULL)
)

p_rejection <- ggplot2::ggplot(plot_summary, ggplot2::aes(x = fitted_link_label, y = rejection_rate, color = link_match, shape = link_match)) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high), linewidth = .45) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  ggplot2::scale_y_continuous(labels = percent_labels()) +
  match_scales +
  ggplot2::labs(
    title = "B. Product-term rejection rate",
    subtitle = "Matched links: nominal rejection; wrong links: pseudo-interaction detection. Dashed line: nominal alpha",
    x = "Fitted link",
    y = "Rejection rate"
  ) +
  link_theme(base_size = 9)

save_plot_grid(list(p_scenario, p_rejection), filename_base = settings$figure_base, width = figure_width, height = 6.5, ncol = 1, dpi = default_dpi)

pseudo_plot_data <- scenario_contrasts
pseudo_plot_data$generating_link_label <- factor(paste0("Generated with ", link_label(pseudo_plot_data$generating_link), " link"), levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))
pseudo_plot_data$fitted_link_label <- factor(link_label(pseudo_plot_data$fitted_link), levels = link_label(settings$candidate_links))

p_pseudo <- ggplot2::ggplot(pseudo_plot_data, ggplot2::aes(x = fitted_link_label, y = deterministic_product_on_fitted_link_scale, color = link_match, shape = link_match)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  match_scales +
  ggplot2::labs(
    title = "Deterministic pseudo-interaction induced by link crossing",
    subtitle = "Matched-link contrasts are zero by construction; wrong-link contrasts can be nonzero",
    x = "Fitted link",
    y = "Product contrast on fitted link scale"
  ) +
  link_theme(base_size = 9)

save_single_plot(p_pseudo, settings$inspection_pseudo_base, width = figure_width, height = 3.8, dpi = default_dpi)

saveRDS(
  list(
    settings = settings,
    scenario_parameters = scenario_parameters,
    cell_probability_table = cell_probability_table,
    scenario_contrasts = scenario_contrasts,
    scenario_table = scenario_table,
    simulation_results = simulation_results,
    simulation_summary = simulation_summary
  ),
  file = settings$rds_path
)

cat("\n", "Saved files", "\n")
cat("- ", settings$scenario_table_path, "\n", sep = "")
cat("- ", settings$simulation_summary_path, "\n", sep = "")
cat("- ", settings$figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_pseudo_base, ".pdf/png\n", sep = "")
cat("- ", settings$rds_path, "\n", sep = "")
cat("\nDone.\n")
