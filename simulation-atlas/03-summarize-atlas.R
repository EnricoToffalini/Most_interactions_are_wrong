# Convert replication-level RDS files into compact app-facing summaries.

# ==========================================================================
#  SETTINGS - you can normally leave this block alone.
# ==========================================================================

# "auto" summarizes whatever 02-run-atlas.R left in raw/: the full results if
# they are there, otherwise the smoke results. Set "smoke" or "full" to force
# one of the two.
MODE <- "auto"

# Replications per scenario. Leave NA to read them off the raw file names, so
# this script automatically follows the B you used in 02-run-atlas.R.
B <- NA
B_WITHIN <- NA

# ==========================================================================
#  Nothing below here needs to be edited.
# ==========================================================================

atlas_dir <- if (file.exists("simulation-atlas/R/atlas-common.R")) "simulation-atlas" else "."
source(file.path(atlas_dir, "R", "atlas-common.R"))
atlas_source("atlas-forced-choice.R")
atlas_source("atlas-sum-scores.R")
atlas_source("atlas-within-family.R")
atlas_source("atlas-diagnostics.R")

grid_path <- file.path(atlas_root(), "data", "scenario-grid.csv")
if (!file.exists(grid_path)) stop("scenario-grid.csv is missing.", call. = FALSE)
grid <- utils::read.csv(grid_path, stringsAsFactors = FALSE, check.names = FALSE)

raw_dir <- file.path(atlas_root(), "raw")
run_type <- if (identical(tolower(as.character(MODE)), "auto")) {
  if (length(list.files(raw_dir, pattern = "^core-.*-full-B[0-9]+[.]rds$"))) "full" else "smoke"
} else {
  atlas_check_mode(MODE)
}

# Reading B off the raw file names keeps this script in step with whatever was
# set in 02-run-atlas.R, so the two settings blocks cannot silently disagree.
detected_B <- function(family_codes) {
  pattern <- sprintf("^core-(%s)-[0-9]+-%s-B[0-9]+[.]rds$",
                     paste(family_codes, collapse = "|"), run_type)
  files <- list.files(raw_dir, pattern = pattern)
  if (!length(files)) return(NA_integer_)
  max(as.integer(sub("^.*-B([0-9]+)[.]rds$", "\\1", files)))
}
counts <- atlas_replication_counts(
  run_type,
  B = if (all(is.na(B))) detected_B(c("FC", "SS")) else B,
  B_within = if (all(is.na(B_WITHIN))) detected_B("WF") else B_WITHIN
)
cat("Summarizing", run_type, "results: B =", counts$core,
    "(within-family B =", paste0(counts$within, ")"), "\n")
summary_grid <- if (run_type == "smoke") {
  grid[match(atlas_smoke_scenario_ids(grid), grid$scenario_id), , drop = FALSE]
} else {
  grid
}

read_expected_raw <- function(kind, scenarios) {
  expected_B <- atlas_requested_B(scenarios$family, counts)
  paths <- vapply(seq_len(nrow(scenarios)), function(i) {
    atlas_raw_path(kind, scenarios$scenario_id[i], run_type, expected_B[i])
  }, character(1))
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing expected ", run_type, " raw files: ",
         paste(basename(missing), collapse = ", "), call. = FALSE)
  }
  # Validate the deserialized object rather than re-reading each file.
  values <- lapply(paths, readRDS)
  for (i in seq_along(values)) {
    if (!atlas_raw_is_complete(values[[i]], expected_B[i])) {
      stop("Raw file is incomplete: ", paths[i], call. = FALSE)
    }
  }
  atlas_bind_rows(values)
}

core_raw <- read_expected_raw("core", summary_grid)

summarise_core_cell <- function(data) {
  attempted <- nrow(data)
  successful <- data$fit_success %in% TRUE & is.finite(data$interaction_p)
  n_successful <- sum(successful)
  false_positive_count <- sum(data$interaction_p[successful] < ATLAS_ALPHA)
  rate <- if (n_successful) false_positive_count / n_successful else NA_real_
  ci <- atlas_wilson_ci(false_positive_count, n_successful)
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
    median_interaction_coefficient = atlas_safe_median(data$interaction_coef),
    median_interaction_se = atlas_safe_median(data$interaction_se),
    median_response_scale_did = atlas_safe_median(data$response_scale_did),
    median_outcome_scale_did = atlas_safe_median(data$outcome_scale_did),
    deterministic_pseudo_interaction = atlas_safe_median(data$deterministic_pseudo_interaction),
    deterministic_response_scale_did = atlas_safe_median(data$deterministic_response_scale_did),
    n_convergence_problems = sum(data$convergence_problem %in% TRUE, na.rm = TRUE),
    convergence_problem_rate = mean(data$convergence_problem %in% TRUE, na.rm = TRUE),
    fit_problem_messages = paste(problem_messages, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

core_split <- split(core_raw, interaction(core_raw$scenario_id, core_raw$model_label, drop = TRUE))
core_metrics <- do.call(rbind, lapply(core_split, summarise_core_cell))
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

diagnostic_plan <- atlas_diagnostic_plan(grid)
if (run_type == "smoke") {
  diagnostic_plan <- diagnostic_plan[
    diagnostic_plan$diagnostic_paper_anchor | diagnostic_plan$family == "sum_scores",
    , drop = FALSE
  ]
}
supported <- diagnostic_plan[diagnostic_plan$family != "sum_scores", , drop = FALSE]
# Prefer a pass that included DHARMa; fall back to a DHARMa-free pass so the
# atlas stays consultable before those checks have been run.
diagnostic_kind <- atlas_diagnostic_kind(TRUE)
if (nrow(supported) && !all(file.exists(vapply(seq_len(nrow(supported)), function(i) {
  atlas_raw_path(diagnostic_kind, supported$scenario_id[i], run_type,
                 atlas_requested_B(supported$family[i], counts))
}, character(1))))) {
  diagnostic_kind <- atlas_diagnostic_kind(FALSE)
}
diagnostic_raw <- read_expected_raw(diagnostic_kind, supported)
dharma_in_raw <- isTRUE(all(diagnostic_raw$dharma_computed %in% TRUE))
cat("Diagnostic source:", diagnostic_kind,
    if (dharma_in_raw) "(DHARMa included)" else "(DHARMa not computed)", "\n")

diagnostic_value <- function(data, diagnostic) {
  switch(
    diagnostic,
    "AIC favors generating link" = list(values = data$aic_favors_generating, kind = "logical"),
    "DHARMa uniformity" = list(values = data$dharma_uniformity_p, kind = "p"),
    "DHARMa dispersion" = list(values = data$dharma_dispersion_p, kind = "p"),
    "DHARMa residual quantiles over fitted values" = list(values = data$dharma_quantile_fitted_p, kind = "p"),
    "DHARMa residual quantiles over focal predictor" = list(values = data$dharma_quantile_predictor_p, kind = "p"),
    "DHARMa residual distribution across design cells" = list(values = data$dharma_categorical_design_p, kind = "p"),
    "Pregibon-style added-term link check" = list(values = data$pregibon_p, kind = "p"),
    stop("Unknown diagnostic.", call. = FALSE)
  )
}

# The manuscript argues that the proportion of replications favouring the
# target link is not informative on its own, because winning by under one AIC
# unit and winning by fifty describe different situations. Carry the magnitude
# of the difference through to the app, not just the win rate.
summarise_aic_delta <- function(raw) {
  empty <- list(n = 0L, median = NA_real_, q25 = NA_real_, q75 = NA_real_,
                within_two_rate = NA_real_)
  if (!nrow(raw) || !all(c("aic_generating", "aic_wrong") %in% names(raw))) return(empty)
  # Positive values favour the generating (target) link, as in the manuscript.
  delta <- raw$aic_wrong - raw$aic_generating
  delta <- delta[is.finite(delta)]
  if (!length(delta)) return(empty)
  quartiles <- unname(stats::quantile(delta, c(0.25, 0.75), names = FALSE))
  list(n = length(delta), median = stats::median(delta),
       q25 = quartiles[1], q75 = quartiles[2],
       within_two_rate = mean(abs(delta) < 2))
}

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
  ci <- atlas_wilson_ci(count, n)
  list(n = n, count = count, rate = rate,
       mc_se = if (n) sqrt(rate * (1 - rate) / n) else NA_real_,
       low = unname(ci["low"]), high = unname(ci["high"]))
}

diagnostic_rows <- lapply(seq_len(nrow(diagnostic_plan)), function(i) {
  scenario <- diagnostic_plan[i, , drop = FALSE]
  raw <- diagnostic_raw[diagnostic_raw$scenario_id == scenario$scenario_id, , drop = FALSE]
  aic_delta <- summarise_aic_delta(raw)
  if (nrow(raw)) {
    pseudo <- summarise_detection(raw$interaction_p, "p")
    fit_success_rate <- mean(raw$fit_success %in% TRUE)
    B_requested <- raw$B_requested[1]
    n_attempted <- nrow(raw)
    dharma_values <- unique(raw$dharma_n_sim)
    if (length(dharma_values) != 1L) {
      stop("Diagnostic raw file has inconsistent DHARMa simulation counts: ",
           scenario$scenario_id, call. = FALSE)
    }
    # NA is legitimate: it marks a pass in which DHARMa was deferred.
    if (!is.na(dharma_values) && !is.finite(dharma_values)) {
      stop("Diagnostic raw file has a non-finite DHARMa simulation count: ",
           scenario$scenario_id, call. = FALSE)
    }
    dharma_n_sim <- as.integer(dharma_values)
    scenario_dharma_computed <- isTRUE(all(raw$dharma_computed %in% TRUE))
  } else {
    pseudo <- list(n = 0L, count = 0L, rate = NA_real_, mc_se = NA_real_, low = NA_real_, high = NA_real_)
    fit_success_rate <- NA_real_
    B_requested <- 0L
    n_attempted <- 0L
    dharma_n_sim <- NA_integer_
    scenario_dharma_computed <- FALSE
  }
  applicability <- atlas_diagnostic_applicability(scenario$family)
  rows <- lapply(seq_len(nrow(applicability)), function(j) {
    applicable <- applicability$applicable[j]
    # "Applicable" is a structural property of the family; "computed" says
    # whether this particular run produced it. A deferred DHARMa pass leaves
    # applicable checks uncomputed, which must not read as inapplicable.
    is_dharma <- startsWith(applicability$diagnostic[j], "DHARMa")
    computed <- applicable && nrow(raw) > 0 && (!is_dharma || scenario_dharma_computed)
    if (computed) {
      value <- diagnostic_value(raw, applicability$diagnostic[j])
      detection <- summarise_detection(value$values, value$kind)
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

required_core <- c(
  "scenario_id", "family", "slice_membership", "paper_anchor", "model_label",
  "generating_link", "B_requested", "n_successful_fits", "fit_success_rate",
  "false_positive_count", "false_positive_rate", "false_positive_mc_se",
  "false_positive_ci_low", "false_positive_ci_high", "median_interaction_coefficient",
  "median_response_scale_did", "deterministic_pseudo_interaction", "generated_at",
  "atlas_version", "run_type"
)
missing_core <- setdiff(required_core, names(core_summary))
if (length(missing_core)) stop("Core summary missing: ", paste(missing_core, collapse = ", "), call. = FALSE)
required_diagnostic <- c(
  "scenario_id", "family", "diagnostic", "applicable", "computed", "n_successful",
  "dharma_n_sim",
  "detection_rate", "detection_mc_se", "detection_ci_low", "detection_ci_high",
  "pseudo_interaction_detection_rate", "aic_delta_n", "aic_delta_median",
  "aic_delta_q25", "aic_delta_q75", "aic_delta_within_two_rate",
  "generated_at", "atlas_version", "run_type"
)
missing_diagnostic <- setdiff(required_diagnostic, names(diagnostic_summary))
if (length(missing_diagnostic)) {
  stop("Diagnostic summary missing: ", paste(missing_diagnostic, collapse = ", "), call. = FALSE)
}

suffix <- if (run_type == "smoke") "-smoke" else ""
core_csv <- file.path(atlas_root(), "data", paste0("atlas-summary", suffix, ".csv"))
core_rds <- file.path(atlas_root(), "data", paste0("atlas-summary", suffix, ".rds"))
diagnostic_csv <- file.path(atlas_root(), "data", paste0("diagnostic-atlas-summary", suffix, ".csv"))
diagnostic_rds <- file.path(atlas_root(), "data", paste0("diagnostic-atlas-summary", suffix, ".rds"))
utils::write.csv(core_summary, core_csv, row.names = FALSE, na = "")
saveRDS(core_summary, core_rds, compress = "xz")
utils::write.csv(diagnostic_summary, diagnostic_csv, row.names = FALSE, na = "")
saveRDS(diagnostic_summary, diagnostic_rds, compress = "xz")
cat("Wrote", run_type, "core summary:", core_csv, "\n")
cat("Wrote", run_type, "diagnostic summary:", diagnostic_csv, "\n")
