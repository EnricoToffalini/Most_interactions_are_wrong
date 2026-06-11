# scripts/03c-simulation-within-family-links.R
# Simulation 3: within-family link choice with repeated binary trials.
# Logit coefficients are scaled so the logit DGP has cell probabilities close
# to the probit reference scenario, while preserving zero product term on the
# generating link scale.

rm(list = ls())

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-plots.R")

if (!requireNamespace("glmmTMB", quietly = TRUE)) {
  stop("Please install glmmTMB to fit random-intercept GLMMs.", call. = FALSE)
}

ensure_output_dirs()
set.seed(20260528)
report_header("Simulation 3: logit vs probit within the binomial family")

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
  B = as.integer(Sys.getenv("N_SIM", as.character(default_B))),
  n_cores = as.integer(Sys.getenv("N_CORES", "7")),
  alpha = default_alpha,
  scenario_table_path = "tables/scenario-table-within-family-links.csv",
  simulation_summary_path = "tables/simulation-summary-within-family-links.csv",
  figure_base = "figs/fig4-within-family-links",
  inspection_pseudo_base = "outputs/inspection/figS-within-family-link-pseudo-interaction",
  rds_path = "outputs/simulation-within-family-links.rds"
)

validate_settings <- function(s) {
  if (s$n_subjects <= 0 || s$n_subjects %% 2 != 0) stop("n_subjects must be a positive even number.", call. = FALSE)
  if (s$k_trials <= 0) stop("k_trials must be positive.", call. = FALSE)
  if (s$target_icc <= 0 || s$target_icc >= 1) stop("target_icc must be in (0, 1).", call. = FALSE)
  if (!all(s$candidate_links %in% c("logit", "probit"))) stop("candidate_links can only include logit and probit.", call. = FALSE)
  if (is.na(s$B) || s$B <= 0) stop("B must be positive.", call. = FALSE)
  if (is.na(s$n_cores) || s$n_cores <= 0) stop("n_cores must be positive.", call. = FALSE)
  if (!is.finite(s$logit_probit_scale) || s$logit_probit_scale <= 0) stop("logit_probit_scale must be positive.", call. = FALSE)
  invisible(s)
}
validate_settings(settings)

report_section("Simulation settings")
print_compact(list_to_table(settings))
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
latent_residual_variance <- function(link) ifelse(link == "logit", pi^2 / 3, 1)
random_intercept_sd <- function(link) sqrt(settings$target_icc * latent_residual_variance(link) / (1 - settings$target_icc))

scenario_parameters <- data.frame(
  generating_link = settings$candidate_links,
  coefficient_scale = ifelse(settings$candidate_links == "logit", settings$logit_probit_scale, 1),
  stringsAsFactors = FALSE
)
for (nm in c("intercept", "group", "condition", "group_condition")) {
  scenario_parameters[[paste0("beta_", nm)]] <-
    scenario_parameters$coefficient_scale * settings[[paste0("reference_beta_", nm)]]
}

get_scenario_parameters <- function(generating_link) {
  out <- scenario_parameters[scenario_parameters$generating_link == generating_link, ]
  if (nrow(out) != 1) stop("Could not find unique scenario parameters.", call. = FALSE)
  out
}

linear_predictor_fixed <- function(group_num, condition_num, generating_link) {
  p <- get_scenario_parameters(generating_link)
  p$beta_intercept + p$beta_group * group_num + p$beta_condition * condition_num +
    p$beta_group_condition * group_num * condition_num
}

interaction_contrast <- function(value, group_num, condition_num) {
  unname(
    value[group_num == 1 & condition_num == 1] - value[group_num == 1 & condition_num == 0] -
      value[group_num == 0 & condition_num == 1] + value[group_num == 0 & condition_num == 0]
  )
}

make_cell_table <- function(generating_link) {
  g <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  g$generating_link <- generating_link
  g$random_intercept_sd <- random_intercept_sd(generating_link)
  g$linear_predictor_random_intercept_0 <- linear_predictor_fixed(g$group_num, g$condition_num, generating_link)
  g$expected_probability_random_intercept_0 <- inv_link(g$linear_predictor_random_intercept_0, generating_link)
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$condition <- factor(g$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  g$generating_link_label <- paste0("Generated with ", link_label(g$generating_link), " link")
  g
}

make_contrasts <- function(generating_link) {
  g <- cell_probability_table[cell_probability_table$generating_link == generating_link, ]
  do.call(rbind, lapply(settings$candidate_links, function(fitted_link) {
    fitted_scale_value <- link_fun(g$expected_probability_random_intercept_0, fitted_link)
    data.frame(
      generating_link = generating_link,
      fitted_link = fitted_link,
      link_match = ifelse(fitted_link == generating_link, "Matched link", "Wrong link"),
      random_intercept_sd = random_intercept_sd(generating_link),
      deterministic_product_on_fitted_link_scale = interaction_contrast(fitted_scale_value, g$group_num, g$condition_num),
      deterministic_response_scale_difference_in_differences = interaction_contrast(g$expected_probability_random_intercept_0, g$group_num, g$condition_num),
      stringsAsFactors = FALSE
    )
  }))
}

cell_probability_table <- do.call(rbind, lapply(settings$candidate_links, make_cell_table))
scenario_contrasts <- do.call(rbind, lapply(settings$candidate_links, make_contrasts))
scenario_table <- merge(cell_probability_table, scenario_contrasts, by = c("generating_link", "random_intercept_sd"), all.x = TRUE, sort = FALSE)
scenario_table <- scenario_table[, c(
  "generating_link", "fitted_link", "link_match", "random_intercept_sd", "group", "condition",
  "linear_predictor_random_intercept_0", "expected_probability_random_intercept_0",
  "deterministic_product_on_fitted_link_scale", "deterministic_response_scale_difference_in_differences"
)]
utils::write.csv(scenario_table, settings$scenario_table_path, row.names = FALSE)

report_section("Scenario parameters after link-specific scaling")
print_compact(scenario_parameters)
report_section("Cell probabilities at random intercept = 0")
print_compact(cell_probability_table[, c(
  "generating_link", "random_intercept_sd", "group", "condition",
  "linear_predictor_random_intercept_0", "expected_probability_random_intercept_0"
)])
report_section("Deterministic pseudo-interaction implied by link crossing")
print_compact(scenario_contrasts)

# ---------------------------------------------------------------------
# 3. Data generation and model fitting
# ---------------------------------------------------------------------
make_design <- function() {
  n_per_group <- settings$n_subjects / 2
  id <- rep(seq_len(settings$n_subjects), each = 2 * settings$k_trials)
  group_by_subject <- rep(c(0, 1), each = n_per_group)
  data.frame(
    id = factor(id),
    group_num = rep(group_by_subject, each = 2 * settings$k_trials),
    condition_num = rep(rep(c(0, 1), each = settings$k_trials), times = settings$n_subjects),
    stringsAsFactors = FALSE
  )
}

simulate_one <- function(generating_link) {
  d <- make_design()
  u <- stats::rnorm(settings$n_subjects, 0, random_intercept_sd(generating_link))
  eta <- linear_predictor_fixed(d$group_num, d$condition_num, generating_link) + u[as.integer(d$id)]
  d$y <- stats::rbinom(nrow(d), 1, inv_link(eta, generating_link))
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  d
}

fit_glmm <- function(d, fitted_link) {
  suppressWarnings(try(
    glmmTMB::glmmTMB(y ~ group * condition + (1 | id), data = d, family = stats::binomial(link = fitted_link)),
    silent = TRUE
  ))
}

interaction_stats <- function(fit) {
  out <- data.frame(interaction_coef = NA_real_, interaction_se = NA_real_, p_value = NA_real_)
  if (inherits(fit, "try-error")) return(out)
  s <- summary(fit)$coefficients$cond
  row <- grep(":", rownames(s), value = TRUE)
  if (length(row) != 1) return(out)
  data.frame(interaction_coef = unname(s[row, "Estimate"]), interaction_se = unname(s[row, "Std. Error"]), p_value = unname(s[row, "Pr(>|z|)"]))
}

wilson_ci <- function(x, n, conf = .95) {
  if (n == 0) return(c(low = NA_real_, high = NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(low = center - half, high = center + half)
}

summarise_cell <- function(dat) {
  ok <- !is.na(dat$p_value)
  n <- sum(ok)
  n_sig <- sum(dat$p_value[ok] < settings$alpha)
  ci <- wilson_ci(n_sig, n)
  data.frame(
    n_successful_fits = n,
    n_significant = n_sig,
    false_positive_rate = n_sig / n,
    ci_low = unname(ci["low"]),
    ci_high = unname(ci["high"]),
    median_interaction_coef = stats::median(dat$interaction_coef, na.rm = TRUE),
    median_interaction_se = stats::median(dat$interaction_se, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------
# 4. Monte Carlo simulation
# ---------------------------------------------------------------------
report_section("Monte Carlo simulation")
cat("Running B = ", settings$B, " replications per generating-link cell.\n", sep = "")
cat("Using n_cores = ", settings$n_cores, ".\n", sep = "")

run_one_replication <- function(generating_link, b) {
  d <- simulate_one(generating_link)
  do.call(rbind, lapply(settings$candidate_links, function(fitted_link) {
    data.frame(
      generating_link = generating_link,
      fitted_link = fitted_link,
      link_match = ifelse(fitted_link == generating_link, "Matched link", "Wrong link"),
      replication = b,
      interaction_stats(fit_glmm(d, fitted_link)),
      stringsAsFactors = FALSE
    )
  }))
}

run_sequential <- function(generating_link) {
  cat("DGP link: ", generating_link, "\n", sep = "")
  do.call(rbind, lapply(seq_len(settings$B), function(b) {
    progress_tick(b, settings$B, label = "  replication ")
    run_one_replication(generating_link, b)
  }))
}

run_parallel <- function(generating_link, cl) {
  cat("DGP link: ", generating_link, "\n", sep = "")
  do.call(rbind, parallel::parLapply(cl, seq_len(settings$B), function(b) run_one_replication(generating_link, b)))
}

if (settings$n_cores == 1) {
  simulation_results <- do.call(rbind, lapply(settings$candidate_links, run_sequential))
} else {
  cl <- parallel::makeCluster(settings$n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
    library(glmmTMB)
    NULL
  })
  parallel::clusterExport(
    cl = cl,
    varlist = c(
      "settings", "scenario_parameters", "get_scenario_parameters", "inv_link",
      "latent_residual_variance", "random_intercept_sd", "linear_predictor_fixed",
      "make_design", "simulate_one", "fit_glmm", "interaction_stats", "run_one_replication"
    ),
    envir = environment()
  )
  parallel::clusterSetRNGStream(cl, iseed = 20260528)
  simulation_results <- do.call(rbind, lapply(settings$candidate_links, run_parallel, cl = cl))
}

simulation_summary <- do.call(rbind, lapply(
  split(simulation_results, list(simulation_results$generating_link, simulation_results$fitted_link), drop = TRUE),
  function(dat) data.frame(
    generating_link = dat$generating_link[1],
    fitted_link = dat$fitted_link[1],
    link_match = dat$link_match[1],
    summarise_cell(dat),
    stringsAsFactors = FALSE
  )
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
simulation_summary <- simulation_summary[order(simulation_summary$generating_link, simulation_summary$fitted_link), ]
utils::write.csv(simulation_summary, settings$simulation_summary_path, row.names = FALSE)

report_section("Simulation summary")
print_compact(simulation_summary)

# ---------------------------------------------------------------------
# 5. Figure and inspection plot
# ---------------------------------------------------------------------
cell_plot_data <- unique(cell_probability_table[, c("generating_link_label", "group", "condition", "expected_probability_random_intercept_0")])
cell_plot_data$generating_link_label <- factor(cell_plot_data$generating_link_label, levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))

p_scenario <- ggplot2::ggplot(
  cell_plot_data,
  ggplot2::aes(x = condition, y = expected_probability_random_intercept_0, color = group, linetype = group, group = group)
) +
  ggplot2::geom_line(linewidth = .9) +
  ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  ggplot2::scale_y_continuous(limits = c(0, 1), labels = percent_labels()) +
  link_scale_color_discrete(name = NULL) +
  link_scale_linetype_discrete(name = NULL) +
  ggplot2::labs(
    title = "A. Generated cell probabilities",
    subtitle = "Cell probabilities are calibrated to be similar across generating links",
    x = NULL,
    y = "Expected probability, random intercept = 0"
  ) +
  link_theme(base_size = 9)

plot_summary <- simulation_summary
plot_summary$generating_link_label <- factor(paste0("Generated with ", link_label(as.character(plot_summary$generating_link)), " link"), levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))
plot_summary$fitted_link_label <- factor(link_label(as.character(plot_summary$fitted_link)), levels = link_label(settings$candidate_links))

match_scales <- list(
  ggplot2::scale_color_manual(values = c("Matched link" = "grey30", "Wrong link" = "#D55E00"), name = NULL),
  ggplot2::scale_shape_manual(values = c("Matched link" = 16, "Wrong link" = 17), name = NULL)
)

p_fp <- ggplot2::ggplot(plot_summary, ggplot2::aes(x = fitted_link_label, y = false_positive_rate, color = link_match, shape = link_match)) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high), linewidth = .45) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  ggplot2::scale_y_continuous(labels = percent_labels()) +
  match_scales +
  ggplot2::labs(
    title = "B. False-positive interaction rate",
    subtitle = "Dashed line is nominal alpha",
    x = "Fitted link",
    y = "False-positive rate"
  ) +
  link_theme(base_size = 9)

save_plot_grid(list(p_scenario, p_fp), filename_base = settings$figure_base, width = figure_width, height = 6.5, ncol = 1, dpi = default_dpi)

pseudo_plot_data <- scenario_contrasts
pseudo_plot_data$generating_link_label <- factor(paste0("Generated with ", link_label(pseudo_plot_data$generating_link), " link"), levels = paste0("Generated with ", link_label(settings$candidate_links), " link"))
pseudo_plot_data$fitted_link_label <- factor(link_label(pseudo_plot_data$fitted_link), levels = link_label(settings$candidate_links))

p_pseudo <- ggplot2::ggplot(pseudo_plot_data, ggplot2::aes(x = fitted_link_label, y = deterministic_product_on_fitted_link_scale, color = link_match, shape = link_match)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::facet_wrap(~ generating_link_label, nrow = 1) +
  match_scales +
  ggplot2::labs(
    title = "Inspection: deterministic pseudo-interaction from link crossing",
    subtitle = "Zero product term on the generating link scale does not imply zero product term on the fitted link scale",
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

report_section("Saved files")
cat("- ", settings$scenario_table_path, "\n", sep = "")
cat("- ", settings$simulation_summary_path, "\n", sep = "")
cat("- ", settings$figure_base, ".pdf/png\n", sep = "")
cat("- ", settings$inspection_pseudo_base, ".pdf/png\n", sep = "")
cat("- ", settings$rds_path, "\n", sep = "")
cat("\nDone.\n")
