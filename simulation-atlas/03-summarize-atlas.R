# Aggregate precomputed Atlas results; no simulation is performed here.
# Use the same ATLAS_MODE and N_SIM as in the family runners.
MODE <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (MODE == "smoke") "3" else "3000"))
ATLAS_ALPHA <- 0.05
ATLAS_VERSION <- "0.1.0"
run_type <- MODE
grid <- utils::read.csv("simulation-atlas/data/scenario-grid.csv", stringsAsFactors = FALSE)
summary_grid <- if (MODE == "smoke") grid[grid$scenario_id %in% c("FC-002", "SS-002", "WF-002"), ] else grid
raw_dir <- "simulation-atlas/raw"
paths <- file.path(raw_dir, sprintf("core-%s-%s-B%d.rds", summary_grid$scenario_id, MODE, B))
values <- lapply(paths, readRDS)
# Only within-family raw files have this descriptive field.
for (i in seq_along(values)) {
  if (!"fit_structure" %in% names(values[[i]])) values[[i]]$fit_structure <- NA_character_
}
core_raw <- do.call(rbind, values)

# Repeated binomial proportion interval used by core and diagnostic summaries.
wilson_ci <- function(x, n, conf = 0.95) {

  if (!is.finite(n) || n <= 0) return(c(low = NA_real_, high = NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denominator <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denominator
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denominator
  c(low = max(0, centre - half), high = min(1, centre + half))

}
core_split <- split(core_raw, interaction(core_raw$scenario_id, core_raw$model_label, drop = TRUE))
core_metrics <- do.call(rbind, lapply(core_split, function(data) {

  attempted <- nrow(data)
  successful <- data$fit_success %in% TRUE & is.finite(data$interaction_p)
  n_successful <- sum(successful)
  false_positive_count <- sum(data$interaction_p[successful] < ATLAS_ALPHA)
  rate <- if (n_successful) false_positive_count / n_successful else NA_real_
  ci <- wilson_ci(false_positive_count, n_successful)
  problem_messages <- unique(data$problem_message[nzchar(data$problem_message)])
  data.frame(
    scenario_id = data$scenario_id[1],
    model_label = data$model_label[1],
    fitted_link = data$fitted_link[1],
    fit_structure = if ("fit_structure" %in% names(data)) data$fit_structure[1] else NA_character_,
    B_requested = data$B_requested[1],
    n_attempted = attempted,
    n_successful_fits = n_successful,
    n_failed_fits = attempted - n_successful,
    fit_success_rate = n_successful / attempted,
    false_positive_count = false_positive_count,
    false_positive_rate = rate,
    false_positive_mc_se = if (n_successful) sqrt(rate * (1 - rate) / n_successful) else NA_real_,
    false_positive_ci_low = unname(ci["low"]),
    false_positive_ci_high = unname(ci["high"]),
    median_interaction_coefficient = stats::median((data$interaction_coef)[is.finite(data$interaction_coef)], na.rm = TRUE),
    median_interaction_se = stats::median((data$interaction_se)[is.finite(data$interaction_se)], na.rm = TRUE),
    median_response_scale_did = stats::median((data$response_scale_did)[is.finite(data$response_scale_did)], na.rm = TRUE),
    median_outcome_scale_did = stats::median((data$outcome_scale_did)[is.finite(data$outcome_scale_did)], na.rm = TRUE),
    deterministic_pseudo_interaction = stats::median((data$deterministic_pseudo_interaction)[is.finite(data$deterministic_pseudo_interaction)], na.rm = TRUE),
    deterministic_response_scale_did = stats::median((data$deterministic_response_scale_did)[is.finite(data$deterministic_response_scale_did)], na.rm = TRUE),
    n_convergence_problems = sum(data$convergence_problem %in% TRUE, na.rm = TRUE),
    convergence_problem_rate = mean(data$convergence_problem %in% TRUE, na.rm = TRUE),
    fit_problem_messages = paste(problem_messages, collapse = " | "),
    stringsAsFactors = FALSE
  )

}))
merge_grid <- summary_grid
names(merge_grid)[names(merge_grid) == "fitted_model"] <- "fitted_model_set"
names(merge_grid)[names(merge_grid) == "fitted_link"] <- "fitted_link_set"
core_summary <- merge(merge_grid, core_metrics, by = "scenario_id", all.y = TRUE, sort = FALSE)
core_summary <- core_summary[order(match(core_summary$scenario_id, summary_grid$scenario_id),
                                   core_summary$model_label), , drop = FALSE]
core_summary$rate_type <- ifelse(
  (core_summary$family == "forced_choice" & core_summary$fitted_link == "chance-corrected logit") |
    (core_summary$family == "sum_scores" & core_summary$model_label == "Latent generating scale") |
    (core_summary$family == "within_family" & core_summary$fitted_link == core_summary$generating_link),
  "Nominal generating-scale rejection rate",
  "Pseudo-interaction detection rate"
)
core_summary$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
core_summary$atlas_version <- ATLAS_VERSION
core_summary$run_type <- run_type
rownames(core_summary) <- NULL

diagnostic_plan <- utils::read.csv("simulation-atlas/data/diagnostic-grid.csv", stringsAsFactors = FALSE)
if (MODE == "smoke") diagnostic_plan <- diagnostic_plan[
  diagnostic_plan$diagnostic_paper_anchor | diagnostic_plan$family == "sum_scores", ]
supported <- diagnostic_plan[diagnostic_plan$family != "sum_scores", ]
paths <- file.path(raw_dir, sprintf("diagnostic-%s-%s-B%d.rds", supported$scenario_id, MODE, B))
if (!all(file.exists(paths))) {
  paths <- file.path(raw_dir, sprintf("diagnostic-nodharma-%s-%s-B%d.rds", supported$scenario_id, MODE, B))
}
diagnostic_raw <- do.call(rbind, lapply(paths, readRDS))
diagnostic_names <- c(
  "AIC favors generating link", "DHARMa uniformity", "DHARMa dispersion",
  "DHARMa residual quantiles over fitted values", "DHARMa residual quantiles over focal predictor",
  "DHARMa residual distribution across design cells", "Pregibon-style added-term link check")
diagnostic_columns <- c("aic_favors_generating", "dharma_uniformity_p", "dharma_dispersion_p",
  "dharma_quantile_fitted_p", "dharma_quantile_predictor_p", "dharma_categorical_design_p", "pregibon_p")
summarise_detection <- function(values, kind) {

  if (kind == "logical") {
    ok <- !is.na(values)
    detected <- values[ok] %in% TRUE
  } else {
    ok <- is.finite(values)
    detected <- values[ok] < ATLAS_ALPHA
  }
  n <- sum(ok)
  count <- sum(detected)
  rate <- if (n) count / n else NA_real_
  ci <- wilson_ci(count, n)
  list(n = n, count = count, rate = rate,
       mc_se = if (n) sqrt(rate * (1 - rate) / n) else NA_real_,
       low = unname(ci["low"]), high = unname(ci["high"]))

}
diagnostic_rows <- lapply(seq_len(nrow(diagnostic_plan)), function(i) {
  scenario <- diagnostic_plan[i, , drop = FALSE]
  raw <- diagnostic_raw[diagnostic_raw$scenario_id == scenario$scenario_id, , drop = FALSE]
  delta <- raw$aic_wrong - raw$aic_generating
  delta <- delta[is.finite(delta)]
  aic_delta <- list(n = length(delta), median = NA_real_, q25 = NA_real_, q75 = NA_real_, within_two_rate = NA_real_)
  if (length(delta)) {
    aic_delta$median <- stats::median(delta)
    aic_delta$q25 <- unname(stats::quantile(delta, 0.25))
    aic_delta$q75 <- unname(stats::quantile(delta, 0.75))
    aic_delta$within_two_rate <- mean(abs(delta) < 2)
  }
  if (nrow(raw)) {
    pseudo <- summarise_detection(raw$interaction_p, "p")
    fit_success_rate <- mean(raw$fit_success %in% TRUE)
    B_requested <- raw$B_requested[1]
    n_attempted <- nrow(raw)
    dharma_n_sim <- raw$dharma_n_sim[1]
    scenario_dharma_computed <- isTRUE(all(raw$dharma_computed %in% TRUE))
  } else {
    pseudo <- list(n = 0L, count = 0L, rate = NA_real_, mc_se = NA_real_, low = NA_real_, high = NA_real_)
    fit_success_rate <- NA_real_
    B_requested <- 0L
    n_attempted <- 0L
    dharma_n_sim <- NA_integer_
    scenario_dharma_computed <- FALSE
  }
  applicability <- data.frame(diagnostic = diagnostic_names,
    applicable = if (scenario$family == "forced_choice") c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
      else if (scenario$family == "within_family") c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
      else rep(FALSE, 7))
  rows <- lapply(seq_len(nrow(applicability)), function(j) {
    applicable <- applicability$applicable[j]
    # "Applicable" is a structural property of the family; "computed" says
    # whether this particular run produced it. A deferred DHARMa pass leaves
    # applicable checks uncomputed, which must not read as inapplicable.
    is_dharma <- startsWith(applicability$diagnostic[j], "DHARMa")
    computed <- applicable && nrow(raw) > 0 && (!is_dharma || scenario_dharma_computed)
    if (computed) {
      detection <- summarise_detection(raw[[diagnostic_columns[j]]], if (j == 1) "logical" else "p")
    } else {
      detection <- list(n = 0L, count = 0L, rate = NA_real_, mc_se = NA_real_,
                        low = NA_real_, high = NA_real_)
    }
    cbind(
      scenario,
      data.frame(
        diagnostic = applicability$diagnostic[j],
        applicable = applicable,
        computed = computed,
        B_requested = B_requested,
        dharma_n_sim = dharma_n_sim,
        n_attempted = n_attempted,
        n_successful = detection$n,
        detection_count = detection$count,
        detection_rate = detection$rate,
        detection_mc_se = detection$mc_se,
        detection_ci_low = detection$low,
        detection_ci_high = detection$high,
        pseudo_interaction_n_successful = pseudo$n,
        pseudo_interaction_detection_count = pseudo$count,
        pseudo_interaction_detection_rate = pseudo$rate,
        pseudo_interaction_mc_se = pseudo$mc_se,
        pseudo_interaction_ci_low = pseudo$low,
        pseudo_interaction_ci_high = pseudo$high,
        aic_delta_n = aic_delta$n,
        aic_delta_median = aic_delta$median,
        aic_delta_q25 = aic_delta$q25,
        aic_delta_q75 = aic_delta$q75,
        aic_delta_within_two_rate = aic_delta$within_two_rate,
        fit_success_rate = fit_success_rate,
        stringsAsFactors = FALSE
      )
    )
  })
  do.call(rbind, rows)
})
diagnostic_summary <- do.call(rbind, diagnostic_rows)
diagnostic_summary$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
diagnostic_summary$atlas_version <- ATLAS_VERSION
diagnostic_summary$run_type <- run_type
rownames(diagnostic_summary) <- NULL

dir.create("simulation-atlas/data", recursive = TRUE, showWarnings = FALSE)
suffix <- if (run_type == "smoke") "-smoke" else ""
core_csv <- file.path("simulation-atlas", "data", paste0("atlas-summary", suffix, ".csv"))
core_rds <- file.path("simulation-atlas", "data", paste0("atlas-summary", suffix, ".rds"))
diagnostic_csv <- file.path("simulation-atlas", "data", paste0("diagnostic-atlas-summary", suffix, ".csv"))
diagnostic_rds <- file.path("simulation-atlas", "data", paste0("diagnostic-atlas-summary", suffix, ".rds"))
utils::write.csv(core_summary, core_csv, row.names = FALSE, na = "")
saveRDS(core_summary, core_rds, compress = "xz")
utils::write.csv(diagnostic_summary, diagnostic_csv, row.names = FALSE, na = "")
saveRDS(diagnostic_summary, diagnostic_rds, compress = "xz")
cat("Wrote", run_type, "core summary:", core_csv, "\n")
cat("Wrote", run_type, "diagnostic summary:", diagnostic_csv, "\n")
