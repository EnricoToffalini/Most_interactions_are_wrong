# scripts/05-simulation-within-family-links.R
# Simulation 3: within-family link choice with repeated binary trials.
#
# Goal:
#   Test whether fitting logit instead of probit, or probit instead of logit,
#   inflates false-positive condition-by-group interactions in a realistic
#   repeated-trial mixed-effects design.
#
# Design:
#   - N = 150 subjects.
#   - Two between-subject groups, balanced.
#   - Two within-subject conditions.
#   - k = 20 binary trials per subject-condition cell.
#   - Subject random intercept SD is chosen to give latent ICC = .50 for
#     each generating link. For a probit DGP this gives SD = 1, matching
#     the original tuning example.
#   - True condition-by-group product term is zero on the generating link scale.
#   - DGP link and fitted link are crossed: logit/probit x logit/probit.
#   - Only mixed-effects models are fitted.

rm(list = ls())

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-plots.R")

if (!requireNamespace("glmmTMB", quietly = TRUE)) {
  stop("Please install glmmTMB to fit random-intercept GLMMs.", call. = FALSE)
}

ensure_output_dirs()
set.seed(20260528)
report_header("Simulation 3: logit vs probit with repeated binary trials")

# ---------------------------------------------------------------------
# 1. User-tunable settings
# ---------------------------------------------------------------------
settings <- list(
  n_subjects = 700,
  k_trials = 15,
  target_icc = 0.30,
  beta_intercept = 1.5,
  beta_group = -1.0,
  beta_condition = -1.0,
  beta_group_condition = 0,
  candidate_links = c("logit", "probit"),
  B = as.integer(Sys.getenv("N_SIM", as.character(default_B))),
  n_cores = as.integer(Sys.getenv("N_CORES", "7")),
  alpha = default_alpha,
  scenario_table_path = "tables/scenario-table-within-family-links.csv",
  simulation_summary_path = "tables/simulation-summary-within-family-links.csv",
  figure_base = "paper/figs/fig4-within-family-links",
  rds_path = "outputs/simulation-within-family-links.rds"
)

validate_settings <- function(settings) {
  if (settings$n_subjects <= 0 || settings$n_subjects %% 2 != 0) {
    stop("settings$n_subjects must be a positive even number.", call. = FALSE)
  }
  if (is.na(settings$n_cores) || settings$n_cores <= 0) {
    stop("settings$n_cores must be a positive integer.", call. = FALSE)
  }
  if (settings$k_trials <= 0) {
    stop("settings$k_trials must be positive.", call. = FALSE)
  }
  if (settings$target_icc <= 0 || settings$target_icc >= 1) {
    stop("settings$target_icc must be between 0 and 1.", call. = FALSE)
  }
  if (!all(settings$candidate_links %in% c("logit", "probit"))) {
    stop("settings$candidate_links can only include logit and probit.", call. = FALSE)
  }
  if (settings$B <= 0) {
    stop("settings$B must be positive.", call. = FALSE)
  }
  invisible(settings)
}

validate_settings(settings)

report_section("Simulation settings")
print_compact(list_to_table(settings))
cat("\nThe true condition-by-group product term is zero on the generating link scale.\n")
cat("All fitted models are random-intercept GLMMs fit with glmmTMB.\n")

# ---------------------------------------------------------------------
# 2. Data generation
# ---------------------------------------------------------------------
inv_link <- function(eta, link) {
  if (link == "logit") return(stats::plogis(eta))
  if (link == "probit") return(stats::pnorm(eta))
  stop("Unknown link.", call. = FALSE)
}

latent_residual_variance <- function(link) {
  if (link == "logit") return(pi^2 / 3)
  if (link == "probit") return(1)
  stop("Unknown link.", call. = FALSE)
}

random_intercept_sd <- function(link) {
  sqrt(settings$target_icc * latent_residual_variance(link) / (1 - settings$target_icc))
}

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
  u <- stats::rnorm(
    settings$n_subjects,
    mean = 0,
    sd = random_intercept_sd(generating_link)
  )
  
  eta <- settings$beta_intercept +
    settings$beta_group * d$group_num +
    settings$beta_condition * d$condition_num +
    settings$beta_group_condition * d$group_num * d$condition_num +
    u[as.integer(d$id)]
  
  d$y <- stats::rbinom(nrow(d), size = 1, prob = inv_link(eta, generating_link))
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$condition <- factor(
    d$condition_num,
    levels = c(0, 1),
    labels = c("Condition 0", "Condition 1")
  )
  d
}

scenario_table <- do.call(rbind, lapply(settings$candidate_links, function(link) {
  g <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  g$generating_link <- link
  g$random_intercept_sd <- random_intercept_sd(link)
  g$linear_predictor_random_intercept_0 <- settings$beta_intercept +
    settings$beta_group * g$group_num +
    settings$beta_condition * g$condition_num +
    settings$beta_group_condition * g$group_num * g$condition_num
  g$expected_probability_random_intercept_0 <- inv_link(
    g$linear_predictor_random_intercept_0,
    link
  )
  g$group <- factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  g$condition <- factor(
    g$condition_num,
    levels = c(0, 1),
    labels = c("Condition 0", "Condition 1")
  )
  g[, c(
    "generating_link", "random_intercept_sd", "group", "condition",
    "linear_predictor_random_intercept_0",
    "expected_probability_random_intercept_0"
  )]
}))

utils::write.csv(scenario_table, settings$scenario_table_path, row.names = FALSE)
report_section("Cell probabilities at random intercept = 0")
print_compact(scenario_table)

# ---------------------------------------------------------------------
# 3. Model fitting
# ---------------------------------------------------------------------
fit_glmm <- function(d, fitted_link) {
  suppressWarnings(try(
    glmmTMB::glmmTMB(
      y ~ group * condition + (1 | id),
      data = d,
      family = stats::binomial(link = fitted_link)
    ),
    silent = TRUE
  ))
}

interaction_stats <- function(fit) {
  if (inherits(fit, "try-error")) {
    return(data.frame(
      interaction_coef = NA_real_,
      interaction_se = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  s <- summary(fit)$coefficients$cond
  row <- grep(":", rownames(s), value = TRUE)
  
  if (length(row) != 1) {
    return(data.frame(
      interaction_coef = NA_real_,
      interaction_se = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  
  data.frame(
    interaction_coef = unname(s[row, "Estimate"]),
    interaction_se = unname(s[row, "Std. Error"]),
    p_value = unname(s[row, "Pr(>|z|)"]),
    stringsAsFactors = FALSE
  )
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
    false_positive_rate = n_sig / n,
    ci_low = ci["low"],
    ci_high = ci["high"],
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
    fit <- fit_glmm(d, fitted_link)
    st <- interaction_stats(fit)
    
    data.frame(
      generating_link = generating_link,
      fitted_link = fitted_link,
      link_match = ifelse(fitted_link == generating_link, "Matched link", "Wrong link"),
      replication = b,
      st,
      stringsAsFactors = FALSE
    )
  }))
}

run_one_generating_link_sequential <- function(generating_link) {
  cat("DGP link: ", generating_link, "\n", sep = "")
  
  do.call(rbind, lapply(seq_len(settings$B), function(b) {
    progress_tick(b, settings$B, label = "  replication ")
    run_one_replication(generating_link, b)
  }))
}

run_one_generating_link_parallel <- function(generating_link, cl) {
  cat("DGP link: ", generating_link, "\n", sep = "")
  
  res <- parallel::parLapply(
    cl = cl,
    X = seq_len(settings$B),
    fun = function(b) {
      run_one_replication(generating_link, b)
    }
  )
  
  do.call(rbind, res)
}

if (settings$n_cores == 1) {
  
  simulation_results <- do.call(rbind, lapply(
    settings$candidate_links,
    run_one_generating_link_sequential
  ))
  
} else {
  
  cl <- parallel::makeCluster(settings$n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, {
    Sys.setenv(
      OMP_NUM_THREADS = "1",
      OPENBLAS_NUM_THREADS = "1",
      MKL_NUM_THREADS = "1"
    )
    library(glmmTMB)
    NULL
  })
  
  parallel::clusterExport(
    cl = cl,
    varlist = c(
      "settings",
      "inv_link",
      "latent_residual_variance",
      "random_intercept_sd",
      "make_design",
      "simulate_one",
      "fit_glmm",
      "interaction_stats",
      "run_one_replication"
    ),
    envir = environment()
  )
  
  parallel::clusterSetRNGStream(cl, iseed = 20260528)
  
  simulation_results <- do.call(rbind, lapply(settings$candidate_links, function(generating_link) {
    run_one_generating_link_parallel(generating_link, cl)
  }))
}

simulation_summary <- do.call(rbind, lapply(
  split(
    simulation_results,
    list(simulation_results$generating_link, simulation_results$fitted_link),
    drop = TRUE
  ),
  function(dat) {
    data.frame(
      generating_link = dat$generating_link[1],
      fitted_link = dat$fitted_link[1],
      link_match = dat$link_match[1],
      summarise_cell(dat),
      stringsAsFactors = FALSE
    )
  }
))

simulation_summary$generating_link <- factor(
  simulation_summary$generating_link,
  levels = settings$candidate_links
)
simulation_summary$fitted_link <- factor(
  simulation_summary$fitted_link,
  levels = settings$candidate_links
)
simulation_summary$link_match <- factor(
  simulation_summary$link_match,
  levels = c("Matched link", "Wrong link")
)
simulation_summary <- simulation_summary[order(
  simulation_summary$generating_link,
  simulation_summary$fitted_link
), ]

utils::write.csv(simulation_summary, settings$simulation_summary_path, row.names = FALSE)
report_section("Simulation summary")
print_compact(simulation_summary)

# ---------------------------------------------------------------------
# 5. Figure and saved objects
# ---------------------------------------------------------------------
p_fp <- ggplot2::ggplot(
  simulation_summary,
  ggplot2::aes(x = fitted_link, y = false_positive_rate, shape = link_match)
) +
  ggplot2::geom_hline(yintercept = settings$alpha, linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_low, ymax = ci_high)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ generating_link, nrow = 1) +
  ggplot2::labs(
    title = "False-positive interaction rate in repeated-trial GLMMs",
    subtitle = "True condition-by-group product term is zero; dashed line is nominal alpha",
    x = "Fitted link",
    y = "False-positive rate",
    shape = "Link status"
  ) +
  link_theme(base_size = 9)

p_fp

save_single_plot(
  p_fp,
  settings$figure_base,
  width = figure_width,
  height = 3.8,
  dpi = default_dpi
)

saveRDS(
  list(
    settings = settings,
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
cat("- ", settings$rds_path, "\n", sep = "")
cat("\nDone.\n")
