# Lightweight smoke test for the companion Shiny app.
# Parses app.R and exercises the helper functions. No Shiny session is
# started and no simulation is run. Run from the repository root:
#   Rscript shiny-app/smoke-test.R

app_dir <- if (file.exists("shiny-app/app.R")) "shiny-app" else "."
repo_dir <- if (identical(app_dir, "shiny-app")) "." else ".."

# 1. app.R must parse.
invisible(parse(file.path(app_dir, "app.R")))
cat("app.R parsed OK\n")

# 2. Helpers must load and behave.
source(file.path(app_dir, "R", "app_helpers.R"))
source(file.path(app_dir, "R", "atlas_helpers.R"))

# All links return finite values at a benign eta.
for (link in setdiff(LINKS_ALL, "inverse")) {
  stopifnot(is.finite(inv_link(0.5, link, chance = 0.5)))
}
stopifnot(is.finite(inv_link(2, "inverse")))

# Known values.
stopifnot(identical(inv_link(1.3, "identity"), 1.3))
stopifnot(abs(inv_link(0, "logit") - 0.5) < 1e-12)
stopifnot(abs(inv_link(0, "probit") - 0.5) < 1e-12)
stopifnot(abs(inv_link(0, "cloglog") - (1 - exp(-1))) < 1e-12)
stopifnot(abs(inv_link(0, "log-log") - exp(-1)) < 1e-12)
stopifnot(abs(inv_link(0, "log") - 1) < 1e-12)
stopifnot(abs(inv_link(0, "chance-corrected logit", chance = 0.5) - 0.75) < 1e-12)

# Chance-corrected logit rejects chance outside [0, 0.95].
bad <- try(inv_link(0, "chance-corrected logit", chance = 0.99), silent = TRUE)
stopifnot(inherits(bad, "try-error"))

# clamp_probability clamps to [0, 1].
stopifnot(identical(clamp_probability(c(-0.2, 0.4, 1.7)), c(0, 0.4, 1)))

# Four-cell values and difference-in-differences.
cells <- four_cell_values(-1, 1.5, 1.5, 0, "logit")
stopifnot(nrow(cells) == 4, all(c("X", "Z", "eta", "mu") %in% names(cells)))
dd <- diff_in_diff(cells)
stopifnot(abs(dd$link_scale) < 1e-12)          # equals beta_xz = 0
stopifnot(abs(dd$response_scale) > 1e-6)       # nonzero on the response scale

cells2 <- four_cell_values(0.2, 0.4, -0.3, 0.7, "identity")
dd2 <- diff_in_diff(cells2)
stopifnot(abs(dd2$link_scale - 0.7) < 1e-12)
stopifnot(abs(dd2$response_scale - 0.7) < 1e-12)  # identity: same on both scales

# Forward link inverts inv_link on each four-cell link.
for (link in LINKS_FOUR_CELL) {
  mu <- inv_link(0.7, link, chance = 0.2)
  stopifnot(abs(link_fun(mu, link, chance = 0.2) - 0.7) < 1e-9)
}

# Cells at or below the chance level yield NaN, not an error.
stopifnot(is.nan(suppressWarnings(
  link_fun(0.1, "chance-corrected logit", chance = 0.25)
)))

# Reverse four-cell calculator: these cells have odds 1/9, 1, 1/3, 3, so
# the logit product term is exactly zero while identity's is 0.10.
cf_logit <- four_cell_coefs(0.10, 0.50, 0.25, 0.75, "logit")
stopifnot(abs(cf_logit[["beta_xz"]]) < 1e-12)
cf_ident <- four_cell_coefs(0.10, 0.50, 0.25, 0.75, "identity")
stopifnot(abs(cf_ident[["beta_xz"]] - 0.10) < 1e-12)

# Round trip: coefficients recovered from four_cell_values cells.
cells_rt <- four_cell_values(-1, 1.5, 0.8, 0.4, "probit")
cf_rt <- four_cell_coefs(cells_rt$mu[1], cells_rt$mu[2],
                         cells_rt$mu[3], cells_rt$mu[4], "probit")
stopifnot(max(abs(cf_rt - c(-1, 1.5, 0.8, 0.4))) < 1e-9)

cat("helpers OK\n")

# 3. Atlas grid and compact summaries must be scientifically self-consistent.
atlas_data <- file.path(repo_dir, "simulation-atlas", "data")
grid_path <- file.path(atlas_data, "scenario-grid.csv")
stopifnot(file.exists(grid_path))
grid <- read.csv(grid_path, stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(!anyDuplicated(grid$scenario_id))
stopifnot(all(grid$generating_interaction == 0))
stopifnot(all(grid$beta_age_group[grid$family == "forced_choice"] == 0))
stopifnot(all(grid$beta_x_group[grid$family == "sum_scores"] == 0))
stopifnot(all(grid$beta_group_condition[grid$family == "within_family"] == 0))

forced_anchor <- grid[grid$family == "forced_choice" & grid$paper_anchor, ]
sum_anchor <- grid[grid$family == "sum_scores" & grid$paper_anchor, ]
within_anchor <- grid[grid$family == "within_family" & grid$paper_anchor, ]
stopifnot(
  nrow(forced_anchor) == 3,
  setequal(forced_anchor$beta_intercept, c(-0.8, 0, 0.8)),
  all(forced_anchor$N == 250), all(forced_anchor$k_trials == 20),
  nrow(sum_anchor) == 3,
  setequal(sum_anchor$threshold_shift, c(1.6, 0, -1.6)),
  all(sum_anchor$N == 600), all(sum_anchor$J == 9),
  nrow(within_anchor) == 2,
  setequal(within_anchor$generating_link, c("logit", "probit")),
  all(within_anchor$n_subjects == 700), all(within_anchor$k_trials == 15),
  all(grepl("logit", within_anchor$fitted_link) & grepl("probit", within_anchor$fitted_link))
)

summary_files <- c(
  file.path(atlas_data, "atlas-summary.csv"),
  file.path(atlas_data, "atlas-summary-smoke.csv")
)
summary_files <- summary_files[file.exists(summary_files)]
for (path in summary_files) {
  summary <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot(!length(setdiff(ATLAS_CORE_REQUIRED, names(summary))))
  stopifnot(!length(setdiff(c(
    "n_attempted", "n_successful_fits", "false_positive_count",
    "false_positive_mc_se", "false_positive_ci_low", "false_positive_ci_high",
    "median_interaction_coefficient", "median_response_scale_did",
    "deterministic_response_scale_did", "generated_at", "atlas_version"
  ), names(summary))))
  stopifnot(all(summary$generating_interaction == 0))
}
diagnostic_files <- c(
  file.path(atlas_data, "diagnostic-atlas-summary.csv"),
  file.path(atlas_data, "diagnostic-atlas-summary-smoke.csv")
)
diagnostic_files <- diagnostic_files[file.exists(diagnostic_files)]
for (path in diagnostic_files) {
  summary <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  stopifnot(!length(setdiff(ATLAS_DIAGNOSTIC_REQUIRED, names(summary))))
  stopifnot(!length(setdiff(c(
    "n_successful", "dharma_n_sim", "detection_mc_se", "detection_ci_low", "detection_ci_high",
    "pseudo_interaction_detection_rate", "aic_delta_n", "aic_delta_median",
    "aic_delta_q25", "aic_delta_q75", "aic_delta_within_two_rate",
    "generated_at", "atlas_version"
  ), names(summary))))
  stopifnot(all(summary$generating_interaction == 0))
  # "Not applicable" and "not computed in this run" are different claims.
  stopifnot(all(summary$computed[summary$computed %in% TRUE] %in% TRUE))
  stopifnot(!any(summary$computed %in% TRUE & !(summary$applicable %in% TRUE)))
  # Non-DHARMa checks never depend on the deferred pass.
  non_dharma <- summary[!startsWith(summary$diagnostic, "DHARMa") &
                          summary$applicable %in% TRUE & summary$n_attempted > 0, , drop = FALSE]
  stopifnot(all(non_dharma$computed %in% TRUE))
  # The AIC magnitude must survive summarization, not just the win rate.
  finite_delta <- summary[is.finite(summary$aic_delta_median), , drop = FALSE]
  if (nrow(finite_delta)) {
    stopifnot(
      all(finite_delta$aic_delta_n > 0),
      all(finite_delta$aic_delta_q25 <= finite_delta$aic_delta_median),
      all(finite_delta$aic_delta_median <= finite_delta$aic_delta_q75),
      all(finite_delta$aic_delta_within_two_rate >= 0 &
            finite_delta$aic_delta_within_two_rate <= 1)
    )
    stopifnot(nrow(atlas_aic_delta_table(finite_delta)) > 0)
  }
}

# The diagnostic crosswalk must name a case that exists in the paper's Table 3,
# rather than a label invented by the app. Supplement S5 additionally numbers the
# same scenarios D1 to D5; the app may cite that numbering but not instead of the
# case name the main table prints.
diag_case_path <- file.path(repo_dir, "tables", "simulation-summary-diagnostic-scenarios.csv")
if (file.exists(diag_case_path)) {
  paper_cases <- read.csv(diag_case_path, stringsAsFactors = FALSE)$scenario
  for (family in c("forced_choice", "within_family")) {
    guide <- atlas_guide(family)
    stopifnot(any(vapply(
      paper_cases,
      function(case) grepl(case, guide$diagnostic_case, fixed = TRUE),
      logical(1)
    )))
  }
}

atlas_bundle <- atlas_load_data()
if (atlas_bundle$available) {
  full_installed <- file.exists(file.path(atlas_data, "atlas-summary.rds")) ||
    file.exists(file.path(atlas_data, "atlas-summary.csv"))
  if (!full_installed) {
    stopifnot(atlas_bundle$run_type == "smoke", grepl("smoke-test data", atlas_bundle$message))
  }
  families <- unique(atlas_bundle$core$family)

  # Every family reachable from the single top-level control must have a
  # manuscript crosswalk, otherwise the app would show an unlabelled case.
  cases <- atlas_case_choices(families)
  stopifnot(setequal(unname(cases), families), all(nzchar(names(cases))))
  for (family in families) {
    guide <- atlas_guide(family)
    stopifnot(
      !is.null(guide),
      all(vapply(c("case", "choice", "short", "section", "figure", "figure_panel",
                   "supplement", "design", "models", "beyond", "diagnostic_case"),
                 function(field) is.character(guide[[field]]) && nzchar(guide[[field]]),
                 logical(1)))
    )
  }

  # Exactly one fitted model per scenario is on the generating scale.
  matched_per_scenario <- tapply(atlas_is_matched(atlas_bundle$core),
                                 atlas_bundle$core$scenario_id, sum)
  stopifnot(all(matched_per_scenario == 1))
  # That model is the one whose deterministic contrast is exactly zero.
  matched_rows <- atlas_bundle$core[atlas_is_matched(atlas_bundle$core), , drop = FALSE]
  stopifnot(all(abs(matched_rows$deterministic_pseudo_interaction) < 1e-12))

  for (family in families) {
    anchors <- atlas_anchor_data(atlas_bundle$core, family)
    if (nrow(anchors)) {
      stopifnot(
        inherits(atlas_anchor_plot(anchors), "ggplot"),
        nrow(atlas_anchor_table(anchors)) == nrow(anchors),
        any(anchors$matched)
      )
    }
    slices <- atlas_slice_overview_data(atlas_bundle$core, family)
    if (nrow(slices)) {
      stopifnot(
        all(is.finite(slices$x_value)),
        !anyNA(slices$panel),
        inherits(atlas_slice_overview_plot(slices, "detection"), "ggplot"),
        inherits(atlas_slice_overview_plot(slices, "deterministic"), "ggplot")
      )
    }
    surface <- atlas_surface_data(atlas_bundle$core, family)
    if (nrow(surface)) {
      stopifnot(
        inherits(atlas_surface_plot(surface, "detection"), "ggplot"),
        inherits(atlas_surface_plot(surface, "deterministic"), "ggplot")
      )
    }
    # The single grouped dropdown must list every scenario of the family once.
    choices <- atlas_scenario_choices(atlas_bundle$core, family)
    listed <- unlist(choices, use.names = FALSE)
    expected_ids <- unique(atlas_bundle$core$scenario_id[atlas_bundle$core$family == family])
    stopifnot(!anyDuplicated(listed), setequal(listed, expected_ids))

    # Scenario names must reach the screen unaltered: a reader comparing the app
    # with the paper has to see the same string in both. Any renaming belongs in
    # the grid, so the displayed label is the stored label, verbatim.
    family_rows <- atlas_bundle$core[atlas_bundle$core$family == family, , drop = FALSE]
    stopifnot(identical(
      atlas_scenario_display_label(family_rows),
      as.character(family_rows$scenario_label)
    ))
    # The dropdown option text is the stored label plus the scenario id only.
    for (group in names(choices)) {
      option_ids <- choices[[group]]
      stored <- vapply(option_ids, function(id) {
        as.character(family_rows$scenario_label[family_rows$scenario_id == id][1])
      }, character(1))
      stopifnot(identical(unname(names(option_ids)),
                          unname(sprintf("%s  (%s)", stored, option_ids))))
    }

    scenario <- atlas_scenario_rows(atlas_bundle$core, listed[1])
    stopifnot(
      nrow(scenario) > 0,
      nzchar(atlas_scenario_headline(scenario)),
      nrow(atlas_scenario_table(scenario)) == nrow(scenario),
      inherits(atlas_expected_plot(scenario[1, , drop = FALSE]), "ggplot")
    )

    if (nrow(atlas_bundle$diagnostic)) {
      diagnostic_rows <- atlas_bundle$diagnostic[atlas_bundle$diagnostic$family == family, , drop = FALSE]
      overview <- atlas_diagnostic_overview_data(diagnostic_rows, family)
      if (nrow(overview)) {
        stopifnot(
          "Pseudo-interaction detection" %in% levels(overview$series),
          inherits(atlas_diagnostic_overview_plot(overview), "ggplot")
        )
      }
    }
  }
}
cat("atlas grid and summaries OK\n")

# 4. Sourcing helpers and app.R must not create raw simulation files. The app
# object must construct when only smoke data (or no atlas summaries) are present.
raw_dir <- file.path(repo_dir, "simulation-atlas", "raw")
raw_before <- sort(list.files(raw_dir, all.files = TRUE, no.. = TRUE))
app_result <- source(file.path(app_dir, "app.R"), local = new.env(parent = globalenv()))$value
raw_after <- sort(list.files(raw_dir, all.files = TRUE, no.. = TRUE))
stopifnot(inherits(app_result, "shiny.appobj"), identical(raw_before, raw_after))
cat("app object constructed without running simulations\n")
cat("smoke test passed\n")
