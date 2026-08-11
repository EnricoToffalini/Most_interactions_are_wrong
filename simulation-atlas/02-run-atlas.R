# Offline Monte Carlo runner. The Shiny app never sources this file.
# Run from the repository root, after building the grid.

# ==========================================================================
#  SETTINGS - this is the only block you need to edit.
# ==========================================================================

# "smoke" = quick check: one manuscript anchor per family, 2-3 replications.
# "full"  = the real simulation: every scenario in the grid.
MODE <- "smoke"

# FALSE = skip the DHARMa residual checks. They dominate the runtime, and the
#         AIC and Pregibon diagnostics are still computed. You can add DHARMa
#         later by setting this to TRUE and re-running this script.
# TRUE  = also run the DHARMa residual checks.
RUN_DHARMA <- FALSE

# Scenarios computed in parallel. 1 = no parallelism.
N_CORES <- 1

# TRUE re-computes scenarios whose raw file is already complete.
# FALSE skips them, so an interrupted run can simply be started again.
OVERWRITE <- FALSE

# Replications per scenario. Leave NA to use the default for the chosen MODE:
#   smoke -> 3 replications (2 for the slower within-family scenarios)
#   full  -> 500 replications (300 for the within-family scenarios)
B <- NA
B_WITHIN <- NA

# Simulated data sets per DHARMa check; only used when RUN_DHARMA = TRUE.
# Leave NA for the default: 25 in smoke mode, 250 in full mode.
DHARMA_N_SIM <- NA

# ==========================================================================
#  Nothing below here needs to be edited.
# ==========================================================================

atlas_dir <- if (file.exists("simulation-atlas/R/atlas-common.R")) "simulation-atlas" else "."
source(file.path(atlas_dir, "R", "atlas-common.R"))
atlas_source("atlas-forced-choice.R")
atlas_source("atlas-sum-scores.R")
atlas_source("atlas-within-family.R")
atlas_source("atlas-diagnostics.R")

required_packages <- c("glmmTMB", "DHARMa")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required atlas package(s): ", paste(missing_packages, collapse = ", "),
    ". Restore renv.lock before running the atlas.", call. = FALSE
  )
}

grid_path <- file.path(atlas_root(), "data", "scenario-grid.csv")
if (!file.exists(grid_path)) {
  stop("scenario-grid.csv is missing. Run 01-build-scenario-grid.R first.", call. = FALSE)
}
grid <- utils::read.csv(grid_path, stringsAsFactors = FALSE, check.names = FALSE)
required_grid_columns <- c(
  "scenario_id", "family", "generating_interaction", "beta_age_group",
  "beta_x_group", "beta_group_condition"
)
missing_grid_columns <- setdiff(required_grid_columns, names(grid))
if (length(missing_grid_columns)) {
  stop("scenario-grid.csv is missing: ", paste(missing_grid_columns, collapse = ", "),
       call. = FALSE)
}
if (anyDuplicated(grid$scenario_id)) stop("scenario-grid.csv contains duplicate scenario_id values.", call. = FALSE)
if (any(!is.finite(grid$generating_interaction)) || any(grid$generating_interaction != 0) ||
    any(grid$beta_age_group[grid$family == "forced_choice"] != 0) ||
    any(grid$beta_x_group[grid$family == "sum_scores"] != 0) ||
    any(grid$beta_group_condition[grid$family == "within_family"] != 0)) {
  stop("Every generating interaction in scenario-grid.csv must be exactly zero.",
       call. = FALSE)
}

mode <- atlas_check_mode(MODE)
counts <- atlas_replication_counts(mode, B, B_WITHIN)
dharma_enabled <- atlas_check_flag(RUN_DHARMA, "RUN_DHARMA")
dharma_n_sim <- if (!dharma_enabled) {
  NA_integer_
} else if (all(is.na(DHARMA_N_SIM))) {
  atlas_default_dharma_n_sim(mode)
} else {
  atlas_check_count(DHARMA_N_SIM, "DHARMA_N_SIM", minimum = 10L)
}
diagnostic_kind <- atlas_diagnostic_kind(dharma_enabled)
n_cores <- atlas_check_count(N_CORES, "N_CORES")
overwrite <- atlas_check_flag(OVERWRITE, "OVERWRITE")

core_grid <- if (mode == "smoke") {
  grid[match(atlas_smoke_scenario_ids(grid), grid$scenario_id), , drop = FALSE]
} else {
  grid
}

atlas_failed_core_rows <- function(scenario, replication, message) {
  links <- switch(
    scenario$family,
    forced_choice = FORCED_LINKS,
    sum_scores = SUM_SCORE_LINKS,
    within_family = stats::setNames(WITHIN_LINKS, paste("Binomial", WITHIN_LINKS))
  )
  models <- names(links)
  rows <- lapply(seq_along(models), function(i) {
    deterministic <- switch(
      scenario$family,
      forced_choice = forced_deterministic_quantities(scenario, models[i]),
      sum_scores = sum_score_deterministic_quantities(scenario, models[i]),
      within_family = within_deterministic_quantities(scenario, unname(links[i]))
    )
    data.frame(
      scenario_id = scenario$scenario_id, family = scenario$family,
      replication = replication,
      replication_seed = atlas_seed(scenario$family, scenario$scenario_id, replication),
      model_label = models[i], fitted_link = unname(links[i]),
      interaction_p = NA_real_, interaction_coef = NA_real_, interaction_se = NA_real_,
      response_scale_did = NA_real_, outcome_scale_did = NA_real_,
      deterministic_pseudo_interaction = deterministic$deterministic_pseudo_interaction,
      deterministic_response_scale_did = deterministic$deterministic_response_scale_did,
      fit_success = FALSE, convergence_problem = TRUE,
      problem_message = message, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

atlas_run_core_scenario <- function(scenario, B_requested, run_type, overwrite) {
  path <- atlas_raw_path("core", scenario$scenario_id, run_type, B_requested)
  if (!overwrite && atlas_raw_complete(path, B_requested)) {
    return(paste("skip", scenario$scenario_id, "(complete)"))
  }
  result <- lapply(seq_len(B_requested), function(replication) {
    tryCatch(
      switch(
        scenario$family,
        forced_choice = run_forced_choice_replication(scenario, replication),
        sum_scores = run_sum_score_replication(scenario, replication),
        within_family = run_within_family_replication(scenario, replication),
        stop("Unknown family: ", scenario$family, call. = FALSE)
      ),
      error = function(e) atlas_failed_core_rows(scenario, replication, conditionMessage(e))
    )
  })
  result <- do.call(rbind, result)
  result$B_requested <- B_requested
  result$run_type <- run_type
  result$base_seed <- ATLAS_BASE_SEED
  result$atlas_version <- ATLAS_VERSION
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  atlas_write_rds_atomic(result, path)
  paste("wrote", scenario$scenario_id, "to", basename(path))
}

atlas_run_diagnostic_scenario <- function(scenario, B_requested, run_type,
                                          overwrite, dharma_n_sim, kind) {
  path <- atlas_raw_path(kind, scenario$scenario_id, run_type, B_requested)
  if (!overwrite && atlas_raw_complete(path, B_requested)) {
    return(paste("skip", scenario$scenario_id, "(complete diagnostic)"))
  }
  result <- lapply(seq_len(B_requested), function(replication) {
    tryCatch(
      run_atlas_diagnostic_replication(scenario, replication, dharma_n_sim),
      error = function(e) data.frame(
        scenario_id = scenario$scenario_id, family = scenario$family,
        replication = replication,
        replication_seed = atlas_seed(scenario$family, scenario$scenario_id, replication,
                                      stream = "diagnostic"),
        interaction_p = NA_real_, interaction_coef = NA_real_, fit_success = FALSE,
        convergence_problem = TRUE, problem_message = conditionMessage(e),
        aic_generating = NA_real_, aic_wrong = NA_real_, aic_favors_generating = NA,
        dharma_uniformity_p = NA_real_, dharma_dispersion_p = NA_real_,
        dharma_quantile_fitted_p = NA_real_, dharma_quantile_predictor_p = NA_real_,
        dharma_categorical_design_p = NA_real_,
        dharma_computed = !is.na(dharma_n_sim), pregibon_p = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  })
  result <- do.call(rbind, result)
  result$B_requested <- B_requested
  result$run_type <- run_type
  result$base_seed <- ATLAS_BASE_SEED
  result$atlas_version <- ATLAS_VERSION
  result$dharma_n_sim <- dharma_n_sim
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  atlas_write_rds_atomic(result, path)
  paste("wrote", scenario$scenario_id, "to", basename(path))
}

atlas_apply_jobs <- function(rows, worker, n_cores) {
  indices <- seq_len(nrow(rows))
  if (n_cores <= 1L || length(indices) <= 1L) return(lapply(indices, worker))
  workers <- min(n_cores, length(indices))
  cluster <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterEvalQ(cluster, {
    Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
    if (requireNamespace("glmmTMB", quietly = TRUE)) NULL
    if (requireNamespace("DHARMa", quietly = TRUE)) NULL
    NULL
  })
  parallel::clusterExport(cluster, varlist = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  # Scenario cost is very uneven: one within-family replication costs roughly
  # twenty to forty times a forced-choice or sum-score one, and the grid is
  # ordered by family. Static chunking would leave whole workers idle while a
  # few finish the within-family block, so dispatch one scenario at a time.
  # Seeds are per scenario and replication, so results do not depend on order.
  parallel::clusterApplyLB(cluster, indices, worker)
}

cat("Atlas mode:", mode, "- B =", counts$core,
    "(within-family B =", paste0(counts$within, ")"), "\n")
cat("Core scenarios:", nrow(core_grid), "using", n_cores, "worker(s)\n")
core_messages <- atlas_apply_jobs(
  core_grid,
  function(i) {
    scenario <- core_grid[i, , drop = FALSE]
    atlas_run_core_scenario(scenario, atlas_requested_B(scenario$family, counts),
                            mode, overwrite)
  },
  n_cores
)
cat(paste(unlist(core_messages), collapse = "\n"), "\n")

diagnostic_plan <- atlas_diagnostic_plan(grid)
if (mode == "smoke") {
  diagnostic_plan <- diagnostic_plan[
    diagnostic_plan$diagnostic_paper_anchor & diagnostic_plan$family %in% c("forced_choice", "within_family"),
    , drop = FALSE
  ]
} else {
  diagnostic_plan <- diagnostic_plan[diagnostic_plan$family != "sum_scores", , drop = FALSE]
}
cat("Diagnostic scenarios:", nrow(diagnostic_plan),
    if (dharma_enabled) paste("with DHARMa n =", dharma_n_sim)
    else "with DHARMa SKIPPED (RUN_DHARMA = FALSE); AIC and Pregibon still computed",
    "\n")
diagnostic_messages <- atlas_apply_jobs(
  diagnostic_plan,
  function(i) {
    scenario <- diagnostic_plan[i, , drop = FALSE]
    atlas_run_diagnostic_scenario(scenario, atlas_requested_B(scenario$family, counts),
                                  mode, overwrite, dharma_n_sim, diagnostic_kind)
  },
  n_cores
)
cat(paste(unlist(diagnostic_messages), collapse = "\n"), "\n")
cat("Raw atlas run complete. Run 03-summarize-atlas.R next.\n")
