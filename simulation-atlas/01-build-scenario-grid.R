# Build the declared partial-factorial atlas grid. This script does not simulate.
# Run from the repository root with:
#   Rscript simulation-atlas/01-build-scenario-grid.R

atlas_dir <- if (file.exists("simulation-atlas/R/atlas-common.R")) "simulation-atlas" else "."
source(file.path(atlas_dir, "R", "atlas-common.R"))
atlas_source("atlas-forced-choice.R")
atlas_source("atlas-sum-scores.R")
atlas_source("atlas-within-family.R")

atlas_extract_setting <- function(path, parameter) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  pattern <- paste0("\\b", parameter, "\\s*=\\s*(-?[0-9]+(?:\\.[0-9]+)?)")
  match <- regexec(pattern, text, perl = TRUE)
  value <- regmatches(text, match)[[1]]
  if (length(value) != 2) stop("Could not read ", parameter, " from ", path, call. = FALSE)
  as.numeric(value[2])
}

atlas_extract_vector <- function(path, parameter) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  pattern <- paste0("\\b", parameter, "\\s*=\\s*c\\(([^)]*)\\)")
  match <- regexec(pattern, text, perl = TRUE)
  value <- regmatches(text, match)[[1]]
  if (length(value) != 2) stop("Could not read vector ", parameter, " from ", path, call. = FALSE)
  as.numeric(trimws(strsplit(value[2], ",", fixed = TRUE)[[1]]))
}

# The manuscript scripts execute simulations when sourced, so validation reads
# their text instead. Parameter names below are the current names in the source
# files; the 03b/03c filenames in this checkout reflect their actual contents.
validate_manuscript_anchors <- function(grid, repository = atlas_repo_root()) {
  forced_path <- file.path(repository, "scripts", "03a-simulation-forced-choice.R")
  sum_path <- file.path(repository, "scripts", "03c-simulation-sum-scores.R")
  within_path <- file.path(repository, "scripts", "03b-simulation-within-family-links.R")
  paths <- c(forced_path, sum_path, within_path)
  if (!all(file.exists(paths))) stop("One or more manuscript simulation scripts are missing.", call. = FALSE)

  forced_expected <- list(
    N = atlas_extract_setting(forced_path, "N"),
    k_trials = atlas_extract_setting(forced_path, "k_trials"),
    chance = atlas_extract_setting(forced_path, "chance"),
    age_center = atlas_extract_setting(forced_path, "age_center"),
    beta_age = atlas_extract_setting(forced_path, "beta_age"),
    beta_group = atlas_extract_setting(forced_path, "beta_group"),
    beta_age_group = atlas_extract_setting(forced_path, "beta_age_group"),
    beta_intercept = atlas_extract_vector(forced_path, "beta_intercept")
  )
  forced <- grid[grid$family == "forced_choice" & grid$paper_anchor, , drop = FALSE]
  stopifnot(
    nrow(forced) == length(forced_expected$beta_intercept),
    setequal(forced$beta_intercept, forced_expected$beta_intercept),
    all(forced$N == forced_expected$N),
    all(forced$k_trials == forced_expected$k_trials),
    all(abs(forced$chance - forced_expected$chance) < 1e-14),
    all(forced$age_center == forced_expected$age_center),
    all(forced$beta_age == forced_expected$beta_age),
    all(forced$beta_group == forced_expected$beta_group),
    all(forced$beta_age_group == forced_expected$beta_age_group)
  )

  sum_expected <- list(
    N = atlas_extract_setting(sum_path, "N"),
    J = atlas_extract_setting(sum_path, "J"),
    item_max = atlas_extract_setting(sum_path, "item_max"),
    theta_sd = atlas_extract_setting(sum_path, "theta_sd"),
    beta_x = atlas_extract_setting(sum_path, "beta_x"),
    beta_group = atlas_extract_setting(sum_path, "beta_group"),
    beta_x_group = atlas_extract_setting(sum_path, "beta_x_group"),
    threshold_shift = atlas_extract_vector(sum_path, "threshold_shift")
  )
  sum_rows <- grid[grid$family == "sum_scores" & grid$paper_anchor, , drop = FALSE]
  stopifnot(
    nrow(sum_rows) == length(sum_expected$threshold_shift),
    setequal(sum_rows$threshold_shift, sum_expected$threshold_shift),
    all(sum_rows$N == sum_expected$N),
    all(sum_rows$J == sum_expected$J),
    all(sum_rows$item_max == sum_expected$item_max),
    all(sum_rows$theta_sd == sum_expected$theta_sd),
    all(sum_rows$beta_x == sum_expected$beta_x),
    all(sum_rows$beta_group == sum_expected$beta_group),
    all(sum_rows$beta_x_group == sum_expected$beta_x_group)
  )
  invisible(lapply(seq_len(nrow(sum_rows)), function(i) sum_score_thresholds(sum_rows[i, ])))

  within_expected <- list(
    n_subjects = atlas_extract_setting(within_path, "n_subjects"),
    k_trials = atlas_extract_setting(within_path, "k_trials"),
    target_icc = atlas_extract_setting(within_path, "target_icc"),
    reference_beta_intercept = atlas_extract_setting(within_path, "reference_beta_intercept"),
    reference_beta_group = atlas_extract_setting(within_path, "reference_beta_group"),
    reference_beta_condition = atlas_extract_setting(within_path, "reference_beta_condition"),
    reference_beta_group_condition = atlas_extract_setting(within_path, "reference_beta_group_condition"),
    logit_probit_scale = atlas_extract_setting(within_path, "logit_probit_scale")
  )
  within <- grid[grid$family == "within_family" & grid$paper_anchor, , drop = FALSE]
  stopifnot(
    nrow(within) == 2,
    setequal(within$generating_link, c("logit", "probit")),
    all(within$n_subjects == within_expected$n_subjects),
    all(within$k_trials == within_expected$k_trials),
    all(within$target_icc == within_expected$target_icc),
    all(within$reference_beta_intercept == within_expected$reference_beta_intercept),
    all(within$reference_beta_group == within_expected$reference_beta_group),
    all(within$reference_beta_condition == within_expected$reference_beta_condition),
    all(within$reference_beta_group_condition == within_expected$reference_beta_group_condition),
    all(within$logit_probit_scale == within_expected$logit_probit_scale),
    all(grepl("logit", within$fitted_link) & grepl("probit", within$fitted_link))
  )
  TRUE
}

validate_atlas_grid <- function(grid) {
  required <- c("scenario_id", "family", "slice", "slice_membership", "paper_anchor",
                "generating_model", "generating_link", "fitted_model", "fitted_link",
                "base_seed", "seed_rule", "B", "alpha", "generating_interaction")
  missing <- setdiff(required, names(grid))
  if (length(missing)) stop("Scenario grid missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (anyDuplicated(grid$scenario_id)) stop("scenario_id is not unique.", call. = FALSE)
  if (any(!is.finite(grid$generating_interaction)) || any(grid$generating_interaction != 0)) {
    stop("Every generating interaction must be exactly zero.", call. = FALSE)
  }
  stopifnot(
    all(grid$alpha == 0.05),
    all(grid$base_seed == ATLAS_BASE_SEED),
    all(grid$B[grid$family != "within_family"] == 500L),
    all(grid$B[grid$family == "within_family"] == 300L)
  )
  forced <- grid[grid$family == "forced_choice", , drop = FALSE]
  stopifnot(all(forced$chance >= 0 & forced$chance < 1),
            all(forced$beta_age_group == 0))
  forced_valid <- vapply(seq_len(nrow(forced)), function(i) {
    scenario <- forced[i, , drop = FALSE]
    values <- forced_expected_probability(
      scenario,
      c(scenario$age_min, scenario$age_max, scenario$age_min, scenario$age_max),
      c(0, 0, 1, 1)
    )
    all(is.finite(values) & values >= scenario$chance & values <= 1)
  }, logical(1))
  stopifnot(all(forced_valid))
  sum_rows <- grid[grid$family == "sum_scores", , drop = FALSE]
  stopifnot(all(sum_rows$beta_x_group == 0))
  invisible(lapply(seq_len(nrow(sum_rows)), function(i) sum_score_thresholds(sum_rows[i, ])))
  within <- grid[grid$family == "within_family", , drop = FALSE]
  stopifnot(all(within$beta_group_condition == 0), all(within$target_icc >= 0 & within$target_icc < 1))
  within_valid <- vapply(seq_len(nrow(within)), function(i) {
    scenario <- within[i, , drop = FALSE]
    values <- within_inverse_link(
      within_fixed_eta(scenario, c(0, 0, 1, 1), c(0, 1, 0, 1)),
      scenario$generating_link
    )
    all(is.finite(values) & values >= 0 & values <= 1)
  }, logical(1))
  stopifnot(all(within_valid))

  expanded <- atlas_expand_slice_membership(grid)
  expected_counts <- data.frame(
    family = c(rep("forced_choice", 6), rep("sum_scores", 6), rep("within_family", 6)),
    slice_view = c(
      "paper_anchor", "main_effect_surface", "sample_size", "scale_location", "trial_count", "chance_level",
      "paper_anchor", "main_effect_surface", "sample_size", "scale_location", "item_count", "latent_dispersion",
      "paper_anchor", "main_effect_surface", "sample_size", "trial_count", "latent_location", "icc"
    ),
    # Main-effect surfaces are 3x3 per generating link; sample-size slices are
    # deliberately denser below each manuscript anchor.
    n = c(3, 9, 7, 5, 4, 3, 3, 9, 7, 5, 3, 3, 2, 18, 14, 6, 8, 8),
    stringsAsFactors = FALSE
  )
  observed <- stats::aggregate(scenario_id ~ family + slice_view, expanded, length)
  names(observed)[3] <- "n"
  comparison <- merge(expected_counts, observed, by = c("family", "slice_view"),
                      suffixes = c("_expected", "_observed"), all = TRUE)
  if (any(is.na(comparison$n_expected)) || any(is.na(comparison$n_observed)) ||
      any(comparison$n_expected != comparison$n_observed)) {
    stop("Unexpected family/slice counts after deduplication.", call. = FALSE)
  }

  slice_rows <- function(family, slice) {
    expanded[expanded$family == family & expanded$slice_view == slice, , drop = FALSE]
  }
  fc_main <- slice_rows("forced_choice", "main_effect_surface")
  fc_n <- slice_rows("forced_choice", "sample_size")
  fc_location <- slice_rows("forced_choice", "scale_location")
  fc_trials <- slice_rows("forced_choice", "trial_count")
  fc_chance <- slice_rows("forced_choice", "chance_level")
  stopifnot(
    all(fc_main$N == 250 & fc_main$k_trials == 20 & fc_main$chance == 0.5 & fc_main$beta_intercept == 0),
    all(fc_n$k_trials == 20 & fc_n$chance == 0.5 & fc_n$beta_intercept == 0 &
          fc_n$beta_age == 0.6 & fc_n$beta_group == -0.9),
    all(fc_location$N == 250 & fc_location$k_trials == 20 & fc_location$chance == 0.5),
    all(fc_trials$N == 250 & fc_trials$chance == 0.5 & fc_trials$beta_intercept == 0),
    all(fc_chance$N == 250 & fc_chance$k_trials == 20 & fc_chance$beta_intercept == 0)
  )

  ss_main <- slice_rows("sum_scores", "main_effect_surface")
  ss_n <- slice_rows("sum_scores", "sample_size")
  ss_location <- slice_rows("sum_scores", "scale_location")
  ss_items <- slice_rows("sum_scores", "item_count")
  ss_sd <- slice_rows("sum_scores", "latent_dispersion")
  stopifnot(
    all(ss_main$N == 600 & ss_main$J == 9 & ss_main$theta_sd == 1 & ss_main$threshold_shift == 0),
    all(ss_n$J == 9 & ss_n$theta_sd == 1 & ss_n$threshold_shift == 0 &
          ss_n$beta_x == 0.85 & ss_n$beta_group == -0.9),
    all(ss_location$N == 600 & ss_location$J == 9 & ss_location$theta_sd == 1),
    all(ss_items$N == 600 & ss_items$theta_sd == 1 & ss_items$threshold_shift == 0),
    all(ss_sd$N == 600 & ss_sd$J == 9 & ss_sd$threshold_shift == 0)
  )

  wf_main <- slice_rows("within_family", "main_effect_surface")
  wf_n <- slice_rows("within_family", "sample_size")
  wf_trials <- slice_rows("within_family", "trial_count")
  wf_location <- slice_rows("within_family", "latent_location")
  wf_icc <- slice_rows("within_family", "icc")
  stopifnot(
    all(wf_main$n_subjects == 700 & wf_main$k_trials == 15 & wf_main$target_icc == 0.30 &
          wf_main$reference_beta_intercept == 1.50),
    all(wf_n$k_trials == 15 & wf_n$target_icc == 0.30 & wf_n$reference_beta_intercept == 1.50 &
          wf_n$reference_beta_group == -1 & wf_n$reference_beta_condition == -1),
    all(wf_trials$n_subjects == 700 & wf_trials$target_icc == 0.30 &
          wf_trials$reference_beta_intercept == 1.50),
    all(wf_location$n_subjects == 700 & wf_location$k_trials == 15 & wf_location$target_icc == 0.30),
    all(wf_icc$n_subjects == 700 & wf_icc$k_trials == 15 &
          wf_icc$reference_beta_intercept == 1.50)
  )

  forced_deterministic <- unlist(lapply(seq_len(nrow(forced)), function(i) {
    vapply(FORCED_MODELS, function(model) {
      forced_deterministic_quantities(forced[i, , drop = FALSE], model)$deterministic_pseudo_interaction
    }, numeric(1))
  }))
  sum_deterministic <- unlist(lapply(seq_len(nrow(sum_rows)), function(i) {
    vapply(SUM_SCORE_MODELS, function(model) {
      sum_score_deterministic_quantities(sum_rows[i, , drop = FALSE], model)$deterministic_pseudo_interaction
    }, numeric(1))
  }))
  within_deterministic <- unlist(lapply(seq_len(nrow(within)), function(i) {
    vapply(WITHIN_LINKS, function(link) {
      within_deterministic_quantities(within[i, , drop = FALSE], link)$deterministic_pseudo_interaction
    }, numeric(1))
  }))
  stopifnot(all(is.finite(c(forced_deterministic, sum_deterministic, within_deterministic))))
  TRUE
}

grid <- atlas_bind_rows(list(
  atlas_forced_choice_rows(),
  atlas_sum_score_rows(),
  atlas_within_family_rows()
))

front <- c("scenario_id", "family", "slice", "slice_membership", "paper_anchor",
           "scenario_label", "generating_model", "generating_link", "fitted_model",
           "fitted_link", "base_seed", "seed_rule", "B", "alpha",
           "generating_interaction")
grid <- grid[c(front, setdiff(names(grid), front))]

validate_atlas_grid(grid)
validate_manuscript_anchors(grid)

output <- file.path(atlas_root(), "data", "scenario-grid.csv")
utils::write.csv(grid, output, row.names = FALSE, na = "")
cat("Wrote", nrow(grid), "unique scenarios to", output, "\n")
print(with(grid, table(family)))
