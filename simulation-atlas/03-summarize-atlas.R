# Summarize the precomputed atlas results (no simulation here) into the compact
# CSV/RDS files read by Supplement B and the Shiny app.
# Run from the repository root with the same ATLAS_MODE and N_SIM used by the runners.

mode <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (mode == "smoke") "3" else "3000"))
alpha <- 0.05
atlas_version <- "0.1.0"
suffix <- if (mode == "smoke") "-smoke" else ""

# Wilson 95% interval, clipped to [0, 1]
wilson <- function(x, n) {
  if (!is.finite(n) || n <= 0) return(c(NA_real_, NA_real_))
  z <- qnorm(0.975)
  p <- x / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  c(max(0, center - half), min(1, center + half))
}
finite_median <- function(x) median(x[is.finite(x)])

####################################################
# Core summary: one row per scenario and fitted model
####################################################

grid <- read.csv("simulation-atlas/data/scenario-grid.csv")
if (mode == "smoke") grid <- grid[grid$scenario_id %in% c("FC-002", "SS-002", "WF-002"), ]
names(grid)[names(grid) == "fitted_model"] <- "fitted_model_set"
names(grid)[names(grid) == "fitted_link"] <- "fitted_link_set"

core_summary <- NULL
for (i in 1:nrow(grid)) {
  raw <- readRDS(sprintf("simulation-atlas/raw/core-%s-%s-B%d.rds", grid$scenario_id[i], mode, B))
  for (m in sort(unique(raw$model_label))) {
    dat <- raw[raw$model_label == m, ]
    # "ok" = finite p-value from a fit without problems (primary); "problem" = finite p-value from a flagged fit
    problem <- dat$convergence_problem %in% TRUE
    ok <- !problem & is.finite(dat$interaction_p)
    flagged <- problem & is.finite(dat$interaction_p)
    n_ok <- sum(ok)
    n_flagged <- sum(flagged)
    reject_ok <- sum(dat$interaction_p[ok] < alpha)
    reject_flagged <- sum(dat$interaction_p[flagged] < alpha)
    rate <- if (n_ok) reject_ok / n_ok else NA_real_
    rate_flagged <- if (n_flagged) reject_flagged / n_flagged else NA_real_
    mc_se <- if (n_ok) sqrt(rate * (1 - rate) / n_ok) else NA_real_
    ci <- wilson(reject_ok, n_ok)
    ci_flagged <- wilson(reject_flagged, n_flagged)
    messages <- unique(dat$problem_message[problem & nzchar(dat$problem_message)])
    # several columns are aliases kept for the app and Supplement B
    core_summary <- rbind(core_summary, data.frame(grid[i, ],
      model_label = m, fitted_link = dat$fitted_link[1],
      fit_structure = if ("fit_structure" %in% names(dat)) dat$fit_structure[1] else NA_character_,
      B_requested = dat$B_requested[1], n_attempted = nrow(dat),
      n_fit_ok = sum(!problem), n_fit_problem = sum(problem), fit_problem_rate = mean(problem),
      n_p_finite_ok = n_ok, n_rejections_ok = reject_ok, rejection_rate_ok = rate, rejection_mc_se_ok = mc_se,
      rejection_ci_low_ok = ci[1], rejection_ci_high_ok = ci[2],
      n_successful_fits = n_ok, n_failed_fits = nrow(dat) - n_ok, fit_success_rate = n_ok / nrow(dat),
      false_positive_count = reject_ok, false_positive_rate = rate, false_positive_mc_se = mc_se,
      false_positive_ci_low = ci[1], false_positive_ci_high = ci[2],
      n_p_finite_problem = n_flagged, n_rejections_problem = reject_flagged, rejection_rate_problem = rate_flagged,
      rejection_ci_low_problem = ci_flagged[1], rejection_ci_high_problem = ci_flagged[2],
      apparent_rejection_count_problem = reject_flagged, apparent_rejection_rate_problem = rate_flagged,
      apparent_rejection_ci_low_problem = ci_flagged[1], apparent_rejection_ci_high_problem = ci_flagged[2],
      median_interaction_coefficient = finite_median(dat$interaction_coef),
      median_interaction_se = finite_median(dat$interaction_se),
      median_response_scale_did = finite_median(dat$response_scale_did),
      median_outcome_scale_did = finite_median(dat$outcome_scale_did),
      deterministic_pseudo_interaction = finite_median(dat$deterministic_pseudo_interaction),
      deterministic_response_scale_did = finite_median(dat$deterministic_response_scale_did),
      n_convergence_problems = sum(problem), convergence_problem_rate = mean(problem),
      fit_problem_messages = paste(messages, collapse = " | ")))
  }
}
# matched (generating) scale: nominal rejection; otherwise pseudo-interaction detection
core_summary$rate_type <- ifelse(
  (core_summary$family == "forced_choice" & core_summary$fitted_link == "chance-corrected logit") |
  (core_summary$family == "sum_scores" & core_summary$model_label == "Latent generating scale") |
  (core_summary$family == "within_family" & core_summary$fitted_link == core_summary$generating_link),
  "Nominal generating-scale rejection rate", "Pseudo-interaction detection rate")
core_summary$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
core_summary$atlas_version <- atlas_version
core_summary$run_type <- mode
rownames(core_summary) <- NULL

####################################################
# Diagnostic summary: one row per diagnostic scenario and check
####################################################

plan <- read.csv("simulation-atlas/data/diagnostic-grid.csv")
if (mode == "smoke") plan <- plan[plan$diagnostic_paper_anchor | plan$family == "sum_scores", ]
supported <- plan$scenario_id[plan$family != "sum_scores"]
# prefer the files with DHARMa checks if all are present, else the DHARMa-free files
paths <- sprintf("simulation-atlas/raw/diagnostic-%s-%s-B%d.rds", supported, mode, B)
if (!all(file.exists(paths))) paths <- sprintf("simulation-atlas/raw/diagnostic-nodharma-%s-%s-B%d.rds", supported, mode, B)
diagnostic_raw <- do.call(rbind, lapply(paths, readRDS))

diagnostics <- data.frame(
  diagnostic = c("AIC favors generating link", "DHARMa uniformity", "DHARMa dispersion",
                 "DHARMa residual quantiles over fitted values", "DHARMa residual quantiles over focal predictor",
                 "DHARMa residual distribution across design cells", "Pregibon-style added-term link check"),
  column = c("aic_favors_generating", "dharma_uniformity_p", "dharma_dispersion_p", "dharma_quantile_fitted_p",
             "dharma_quantile_predictor_p", "dharma_categorical_design_p", "pregibon_p"),
  forced_choice = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE), # structural applicability by family
  within_family = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))

# detection rate: p < alpha, or TRUE for the logical AIC flag
detection <- function(values) {
  ok <- if (is.logical(values)) !is.na(values) else is.finite(values)
  detected <- if (is.logical(values)) values[ok] %in% TRUE else values[ok] < alpha
  n <- sum(ok)
  count <- sum(detected)
  rate <- if (n) count / n else NA_real_
  ci <- wilson(count, n)
  list(n = n, count = count, rate = rate, mc_se = if (n) sqrt(rate * (1 - rate) / n) else NA_real_, low = ci[1], high = ci[2])
}
not_computed <- list(n = 0L, count = 0L, rate = NA_real_, mc_se = NA_real_, low = NA_real_, high = NA_real_)

diagnostic_summary <- NULL
for (i in 1:nrow(plan)) {
  raw <- diagnostic_raw[diagnostic_raw$scenario_id == plan$scenario_id[i], ]
  delta <- raw$aic_wrong - raw$aic_generating
  delta <- delta[is.finite(delta)]
  pseudo <- if (nrow(raw)) detection(raw$interaction_p) else not_computed
  dharma_computed <- nrow(raw) > 0 && all(raw$dharma_computed %in% TRUE)
  for (j in 1:nrow(diagnostics)) {
    # "applicable" is structural; "computed" says whether this run produced it (a DHARMa-free run leaves DHARMa uncomputed)
    applicable <- if (plan$family[i] == "sum_scores") FALSE else diagnostics[[plan$family[i]]][j]
    computed <- applicable && nrow(raw) > 0 && (!startsWith(diagnostics$diagnostic[j], "DHARMa") || dharma_computed)
    det <- if (computed) detection(raw[[diagnostics$column[j]]]) else not_computed
    diagnostic_summary <- rbind(diagnostic_summary, data.frame(plan[i, ],
      diagnostic = diagnostics$diagnostic[j], applicable = applicable, computed = computed,
      B_requested = if (nrow(raw)) raw$B_requested[1] else 0L,
      dharma_n_sim = if (nrow(raw)) raw$dharma_n_sim[1] else NA_integer_,
      n_attempted = nrow(raw), n_successful = det$n, detection_count = det$count, detection_rate = det$rate,
      detection_mc_se = det$mc_se, detection_ci_low = det$low, detection_ci_high = det$high,
      pseudo_interaction_n_successful = pseudo$n, pseudo_interaction_detection_count = pseudo$count,
      pseudo_interaction_detection_rate = pseudo$rate, pseudo_interaction_mc_se = pseudo$mc_se,
      pseudo_interaction_ci_low = pseudo$low, pseudo_interaction_ci_high = pseudo$high,
      aic_delta_n = length(delta),
      aic_delta_median = if (length(delta)) median(delta) else NA_real_,
      aic_delta_q25 = if (length(delta)) unname(quantile(delta, 0.25)) else NA_real_,
      aic_delta_q75 = if (length(delta)) unname(quantile(delta, 0.75)) else NA_real_,
      aic_delta_within_two_rate = if (length(delta)) mean(abs(delta) < 2) else NA_real_,
      fit_success_rate = if (nrow(raw)) mean(raw$fit_success %in% TRUE) else NA_real_))
  }
}
diagnostic_summary$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
diagnostic_summary$atlas_version <- atlas_version
diagnostic_summary$run_type <- mode
rownames(diagnostic_summary) <- NULL

write.csv(core_summary, paste0("simulation-atlas/data/atlas-summary", suffix, ".csv"), row.names = FALSE, na = "")
saveRDS(core_summary, paste0("simulation-atlas/data/atlas-summary", suffix, ".rds"), compress = "xz")
write.csv(diagnostic_summary, paste0("simulation-atlas/data/diagnostic-atlas-summary", suffix, ".csv"), row.names = FALSE, na = "")
saveRDS(diagnostic_summary, paste0("simulation-atlas/data/diagnostic-atlas-summary", suffix, ".rds"), compress = "xz")
cat("Wrote", mode, "core and diagnostic summaries to simulation-atlas/data/\n")
