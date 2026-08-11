# Helpers for reading and displaying the precomputed simulation atlas.
# This file contains no simulation or model-fitting code: every number shown
# comes from the compact summaries written by simulation-atlas/03-summarize-atlas.R.
#
# The display layer is organised around the manuscript rather than around the
# storage format: a reader with the paper open picks one paper case, sees the
# manuscript anchors first, and only then the sensitivity slices that go beyond
# them.

ATLAS_CORE_REQUIRED <- c(
  "scenario_id", "family", "slice_membership", "paper_anchor", "scenario_label",
  "generating_link", "model_label", "fitted_link", "B_requested",
  "fit_success_rate", "false_positive_rate", "false_positive_ci_low",
  "false_positive_ci_high", "deterministic_pseudo_interaction", "run_type"
)
ATLAS_DIAGNOSTIC_REQUIRED <- c(
  "scenario_id", "family", "slice_membership", "diagnostic", "applicable",
  "computed", "detection_rate", "pseudo_interaction_detection_rate", "run_type"
)

# Okabe-Ito, extended with two neutrals. Series identity is never colour-alone:
# every plot also varies linetype or shape.
ATLAS_COLORS <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                  "#E69F00", "#56B4E9", "#666666", "#000000")
ATLAS_LINETYPES <- c("solid", "22", "42", "1343", "73", "12", "solid", "22")

# Applicable checks that this run did not produce, e.g. a deferred DHARMa pass.
atlas_uncomputed_diagnostics <- function(data) {
  if (!nrow(data) || !all(c("applicable", "computed") %in% names(data))) return(character())
  sort(unique(data$diagnostic[data$applicable %in% TRUE & !(data$computed %in% TRUE)]))
}

# ------------------------------------------------------------------ loading ---

atlas_repository_root <- function() {
  candidates <- c(".", "..")
  is_root <- vapply(candidates, function(path) {
    file.exists(file.path(path, "renv.lock")) ||
      length(list.files(path, pattern = "[.]Rproj$", full.names = TRUE)) > 0
  }, logical(1))
  hit <- candidates[is_root]
  if (!length(hit)) NA_character_ else
    normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

atlas_data_dir <- function() {
  root <- atlas_repository_root()
  if (is.na(root)) return(NA_character_)
  directory <- file.path(root, "simulation-atlas", "data")
  if (!dir.exists(directory)) NA_character_ else directory
}

atlas_read_summary_pair <- function(stem, suffix = "") {
  directory <- atlas_data_dir()
  if (is.na(directory)) return(NULL)
  rds <- file.path(directory, paste0(stem, suffix, ".rds"))
  csv <- file.path(directory, paste0(stem, suffix, ".csv"))
  value <- try(
    if (file.exists(rds)) readRDS(rds) else if (file.exists(csv))
      utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE) else NULL,
    silent = TRUE
  )
  if (inherits(value, "try-error")) NULL else value
}

atlas_validate_summary <- function(data, required) {
  is.data.frame(data) && nrow(data) > 0 && !length(setdiff(required, names(data)))
}

atlas_load_data <- function() {
  core_full <- atlas_read_summary_pair("atlas-summary")
  diagnostic_full <- atlas_read_summary_pair("diagnostic-atlas-summary")
  if (atlas_validate_summary(core_full, ATLAS_CORE_REQUIRED) &&
      all(core_full$run_type == "full")) {
    diagnostic_ok <- atlas_validate_summary(diagnostic_full, ATLAS_DIAGNOSTIC_REQUIRED) &&
      all(diagnostic_full$run_type == "full")
    return(list(
      available = TRUE, run_type = "full", core = core_full,
      diagnostic = if (diagnostic_ok) diagnostic_full else data.frame(),
      message = if (diagnostic_ok) "Full precomputed atlas loaded."
      else "Full core atlas loaded; the diagnostic summary is not installed."
    ))
  }

  core_smoke <- atlas_read_summary_pair("atlas-summary", "-smoke")
  diagnostic_smoke <- atlas_read_summary_pair("diagnostic-atlas-summary", "-smoke")
  if (atlas_validate_summary(core_smoke, ATLAS_CORE_REQUIRED) &&
      all(core_smoke$run_type == "smoke")) {
    diagnostic_ok <- atlas_validate_summary(diagnostic_smoke, ATLAS_DIAGNOSTIC_REQUIRED) &&
      all(diagnostic_smoke$run_type == "smoke")
    return(list(
      available = TRUE, run_type = "smoke", core = core_smoke,
      diagnostic = if (diagnostic_ok) diagnostic_smoke else data.frame(),
      message = paste(
        "The full precomputed atlas is not installed.",
        "The displays below use clearly labeled smoke-test data only."
      )
    ))
  }

  list(
    available = FALSE, run_type = "missing", core = data.frame(), diagnostic = data.frame(),
    message = paste(
      "The full precomputed atlas has not been generated or installed.",
      "See simulation-atlas/README.md for offline build instructions."
    )
  )
}

# ----------------------------------------------------- manuscript crosswalk ---

# One entry per atlas family, written so that every field can be read with the
# manuscript open. Figure and table numbers follow the order of the labelled
# floats in paper/paper-v2.qmd.
ATLAS_PAPER_GUIDE <- list(
  forced_choice = list(
    case = "Simulation 1",
    short = "Forced-choice accuracy and the non-zero chance floor",
    choice = "Simulation 1 - forced-choice accuracy (Figure 3)",
    section = paste(
      "Section \"The Link Function Problem in Three Common Cases\",",
      "subsection \"Forced-Choice Accuracy and the Non-Zero Chance Floor\",",
      "then \"Simulation 1: Design and Results\""
    ),
    figure = "Figure 3",
    figure_panel = "Panel B holds the product-term rejection rates by fitted model and scenario",
    supplement = paste(
      "Supplement S4, \"Simulation 1: forced-choice accuracy with a chance floor\",",
      "which also tabulates the per-scenario product-term rejection rates"
    ),
    design = paste(
      "N = 250 participants with k = 20 trials each, ages 6 to 10 centered at 8.",
      "Accuracy is generated from a chance-corrected logit model with a .50 chance floor:",
      "age effect 0.60, group effect -0.90, and an age-by-group product term of exactly zero",
      "on that generating scale. The three manuscript scenarios differ only in the intercept",
      "(-0.80, 0.00, +0.80), which is where the Lower performance, Middle performance and",
      "Higher performance scenarios come from: overall accuracy sits just above the .50",
      "chance floor, in the middle of the above-chance range, or close to 1."
    ),
    models = paste(
      "Four models are fitted to each dataset, one row per participant: a Gaussian identity",
      "model on the observed proportions, standard binomial logit and probit models on the",
      "correct-response counts, and the chance-corrected binomial logit that matches the",
      "generating scale and therefore supplies the nominal rejection rate."
    ),
    beyond = paste(
      "Holding the generating product term at zero, the atlas moves the sample size",
      "(N = 50 to 1000), the intercept, the number of trials per participant (5 to 50),",
      "the chance level (.25, .33, .50), and the two additive main effects."
    ),
    diagnostic_case = paste(
      "The \"Chance-floor accuracy\" row of Table 3, listed as scenario D2 in Supplement S5:",
      "chance-corrected logit generated, standard binomial logit fitted."
    )
  ),
  within_family = list(
    case = "Simulation 2",
    short = "Within-family link choice: logit versus probit",
    choice = "Simulation 2 - logit versus probit GLMMs (Figure 4)",
    section = paste(
      "Section \"The Link Function Problem in Three Common Cases\",",
      "subsection \"Within-Family Link Choice\",",
      "then \"Simulation 2: Design and Results\""
    ),
    figure = "Figure 4",
    figure_panel = "Panel B holds the product-term rejection rates for each generating-fitted link combination",
    supplement = paste(
      "Supplement S4, \"Simulation 2: within-family link choice\", and Supplement S5,",
      "\"Deterministic link-scale decomposition, Simulation 2\""
    ),
    design = paste(
      "700 participants in a two-group, two-condition design, 15 repeated binary trials per",
      "participant-condition cell, and a subject random intercept with latent intraclass",
      "correlation .30. The probit reference coefficients are intercept 1.50, group -1.00 and",
      "condition -1.00; logit-generated data use the same values multiplied by 1.65. The",
      "condition-by-group product term is exactly zero on the generating link scale."
    ),
    models = paste(
      "The generating link is crossed with the fitted link, logit and probit in both roles,",
      "and every model is a random-intercept GLMM. The two matched combinations supply the",
      "nominal rejection rate; the two crossed combinations supply pseudo-interaction rates."
    ),
    beyond = paste(
      "The atlas moves the number of subjects (100 to 2000), the trials per cell (5 to 30),",
      "the reference intercept, the latent intraclass correlation (0 to .45), and the two",
      "additive main effects, separately for logit-generated and probit-generated data."
    ),
    diagnostic_case = paste(
      "The \"Binary repeated trials\" row of Table 3, listed as scenario D3 in Supplement S5:",
      "probit generated, logit fitted.",
      "The paper's diagnostic anchor uses 300 subjects rather than the 700 of Simulation 2."
    )
  ),
  sum_scores = list(
    case = "Simulation 3",
    short = "Sum scores as bounded and discrete outcomes",
    choice = "Simulation 3 - bounded sum scores (Figure 5)",
    section = paste(
      "Section \"The Link Function Problem in Three Common Cases\",",
      "subsection \"Sum Scores\", then \"Simulation 3: Design and Results\""
    ),
    figure = "Figure 5",
    figure_panel = "Panel B holds the product-term rejection rates for the latent benchmark and the two observed-score models",
    supplement = paste(
      "Supplement S4, \"Simulation 3: sum scores\", which also tabulates the",
      "per-scenario product-term rejection rates"
    ),
    design = paste(
      "N = 600 participants on a latent continuous scale (SD 1) with a continuous predictor",
      "(effect 0.85), a group effect of -0.90 and no x-by-group product term on that latent",
      "scale. Each latent value becomes 9 ordinal items scored 0 to 3, summed to a total on",
      "a 0-to-27 scale. The three manuscript scenarios shift the item thresholds by +1.60,",
      "0.00 and -1.60, which is where the Lower range, Middle range and Upper range",
      "scenarios come from: harder items push the expected totals down the observed scale,",
      "easier items push them up."
    ),
    models = paste(
      "Three analysis scales: the observed sum-score identity model (a Gaussian linear model",
      "of the manifest total), the Gaussian-probit bounded-score sensitivity model, and the",
      "latent generating scale, an idealized benchmark available only because the true theta",
      "is known. The latent benchmark supplies the nominal rejection rate."
    ),
    beyond = paste(
      "The atlas moves the sample size (N = 75 to 1200), the threshold shift, the number of",
      "items (5, 9, 15), the latent dispersion (SD 0.70 to 1.30), and the two additive",
      "main effects."
    ),
    diagnostic_case = paste(
      "None. The paper deliberately defines no same-response, same-likelihood diagnostic",
      "comparison for its sum-score models, so Table 3 contains no sum-score scenario."
    )
  )
)

atlas_guide <- function(family) {
  guide <- ATLAS_PAPER_GUIDE[[family]]
  if (is.null(guide)) NULL else guide
}

# Radio-button choices for the single top-level atlas control, labelled by the
# manuscript case rather than by the storage-level family name.
atlas_case_choices <- function(families) {
  families <- intersect(names(ATLAS_PAPER_GUIDE), families)
  if (!length(families)) return(c("Atlas data unavailable" = ""))
  labels <- vapply(families, function(family) ATLAS_PAPER_GUIDE[[family]]$choice, character(1))
  stats::setNames(families, unname(labels))
}

# ------------------------------------------------------------------- labels ---

atlas_slice_label <- function(slice) {
  labels <- c(
    paper_anchor = "Manuscript anchors",
    main_effect_surface = "Main-effect surface",
    sample_size = "Sample size",
    scale_location = "Scale location",
    trial_count = "Trial count",
    chance_level = "Chance level",
    item_count = "Item count",
    latent_dispersion = "Latent dispersion",
    latent_location = "Latent location",
    icc = "Intraclass correlation",
    severity = "Misspecification severity",
    diagnostic_anchor = "Paper diagnostic anchor",
    diagnostic_not_defined = "No paper diagnostic defined"
  )
  value <- unname(labels[slice])
  ifelse(is.na(value), gsub("_", " ", slice), value)
}

# One-dimensional slices, in the order their panels should appear.
ATLAS_ONE_D_SLICES <- c("sample_size", "scale_location", "trial_count",
                        "chance_level", "item_count", "latent_dispersion",
                        "latent_location", "icc")

atlas_expand_membership <- function(data) {
  if (!nrow(data)) return(data)
  rows <- lapply(seq_len(nrow(data)), function(i) {
    membership <- strsplit(as.character(data$slice_membership[i]), ";", fixed = TRUE)[[1]]
    out <- data[rep(i, length(membership)), , drop = FALSE]
    out$slice_view <- membership
    out
  })
  do.call(rbind, rows)
}

atlas_slice_data <- function(data, family, slice) {
  expanded <- atlas_expand_membership(data[data$family == family, , drop = FALSE])
  expanded[expanded$slice_view == slice, , drop = FALSE]
}

atlas_slice_parameter <- function(family, slice) {
  key <- paste(family, slice, sep = ":")
  unname(c(
    "forced_choice:sample_size" = "N",
    "forced_choice:scale_location" = "beta_intercept",
    "forced_choice:trial_count" = "k_trials",
    "forced_choice:chance_level" = "chance",
    "sum_scores:sample_size" = "N",
    "sum_scores:scale_location" = "threshold_shift",
    "sum_scores:item_count" = "J",
    "sum_scores:latent_dispersion" = "theta_sd",
    "within_family:sample_size" = "n_subjects",
    "within_family:trial_count" = "k_trials",
    "within_family:latent_location" = "reference_beta_intercept",
    "within_family:icc" = "target_icc"
  )[key])
}

# Every non-sample-size diagnostic slice varies the scale location.
atlas_diagnostic_parameter <- function(family, slice) {
  if (identical(slice, "sample_size")) {
    if (identical(family, "forced_choice")) "N" else "n_subjects"
  } else if (identical(family, "forced_choice")) {
    "beta_intercept"
  } else {
    "reference_beta_intercept"
  }
}

atlas_parameter_label <- function(parameter) {
  labels <- c(
    N = "Participants (N)", n_subjects = "Subjects", k_trials = "Trials per cell/person",
    beta_intercept = "Intercept", threshold_shift = "Threshold shift", chance = "Chance level",
    J = "Items", theta_sd = "Latent SD", reference_beta_intercept = "Reference intercept",
    target_icc = "Target ICC", beta_main_1_multiplier = "First main-effect multiplier",
    beta_main_2_multiplier = "Second main-effect multiplier"
  )
  value <- unname(labels[parameter])
  ifelse(is.na(value), parameter, value)
}

atlas_format_value <- function(x, digits = 4) {
  ifelse(is.finite(x), format(signif(x, digits), trim = TRUE, scientific = FALSE), "NA")
}

# Rates are printed in the manuscript's style, without the leading zero.
atlas_format_rate <- function(x) {
  ifelse(is.finite(x), sub("^0[.]", ".", sprintf("%.2f", x)), "")
}

# Scenario names are shown exactly as the atlas stores them, which is exactly
# how the manuscript scripts write them. The app deliberately adds no gloss and
# no alias: a reader comparing app and paper must see the same string in both,
# so any renaming belongs upstream in the grid, never here.
atlas_scenario_display_label <- function(data) {
  as.character(data$scenario_label)
}

# The one fitted model per scenario whose scale matches the generating scale.
# The summariser already records this as the rate type, so the app does not
# re-derive it from coefficients.
atlas_is_matched <- function(data) {
  if ("rate_type" %in% names(data)) {
    return(grepl("^Nominal", as.character(data$rate_type)))
  }
  is.finite(data$deterministic_pseudo_interaction) &
    abs(data$deterministic_pseudo_interaction) < 1e-12
}

# Series identity for a single fitted model within one generating scenario.
atlas_series_label <- function(data) {
  if (identical(data$family[1], "within_family")) {
    paste("Fitted", data$fitted_link)
  } else {
    as.character(data$model_label)
  }
}

# Series identity when the generating scenario itself varies along an axis, so
# the generating link has to travel with the fitted link.
atlas_pair_label <- function(data) {
  if (identical(data$family[1], "within_family")) {
    sprintf("%s generated, %s fitted", data$generating_link, data$fitted_link)
  } else {
    as.character(data$model_label)
  }
}

atlas_series_scales <- function(values) {
  values <- unique(values)
  n <- length(values)
  list(
    ggplot2::scale_color_manual(values = stats::setNames(ATLAS_COLORS[seq_len(n)], values), name = NULL),
    ggplot2::scale_linetype_manual(values = stats::setNames(ATLAS_LINETYPES[seq_len(n)], values), name = NULL),
    ggplot2::scale_fill_manual(values = stats::setNames(ATLAS_COLORS[seq_len(n)], values), name = NULL)
  )
}

# Both labels are the manuscript's own: "Pseudo-interaction detection rate" is
# the rate type the summariser stores, and the deterministic quantity is the
# "induced product term" of Supplement S5, evaluated on the fitted link scale.
ATLAS_DETECTION_LABEL <- "Pseudo-interaction detection rate"
ATLAS_DETERMINISTIC_LABEL <- "Deterministic induced product term (fitted link scale)"

atlas_metric_column <- function(metric) {
  if (identical(metric, "detection")) "false_positive_rate" else "deterministic_pseudo_interaction"
}

atlas_metric_label <- function(metric) {
  if (identical(metric, "detection")) ATLAS_DETECTION_LABEL else ATLAS_DETERMINISTIC_LABEL
}

# ------------------------------------------------- manuscript anchor views ---

atlas_anchor_data <- function(core, family) {
  rows <- core[core$family == family & core$paper_anchor %in% TRUE, , drop = FALSE]
  if (!nrow(rows)) return(rows)
  rows$matched <- atlas_is_matched(rows)
  rows$series <- atlas_series_label(rows)
  rows$anchor_x <- if (identical(family, "within_family")) {
    paste0(rows$generating_link, "-generated data")
  } else {
    atlas_scenario_display_label(rows)
  }
  rows <- rows[order(rows$scenario_id, rows$series), , drop = FALSE]
  rows$anchor_x <- factor(rows$anchor_x, levels = unique(rows$anchor_x))
  rows$series <- factor(rows$series, levels = unique(rows$series))
  rows
}

atlas_anchor_plot <- function(rows) {
  if (!nrow(rows)) return(NULL)
  dodge <- ggplot2::position_dodge(width = 0.8)
  # The outline marks the matched model. It is drawn inside the single bar layer
  # rather than as a second layer, because a dodged layer holding only the
  # matched rows would not keep its slot in the dodge.
  rows$outline <- ifelse(rows$matched, "grey10", NA_character_)
  # group is set explicitly: without it the outline aesthetic would enter the
  # implicit grouping of the bar layer only, and the layers would dodge apart.
  ggplot2::ggplot(rows, ggplot2::aes(anchor_x, false_positive_rate,
                                     fill = series, group = series)) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey40") +
    ggplot2::geom_col(ggplot2::aes(color = outline), position = dodge,
                      width = 0.75, linewidth = 0.9) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = false_positive_ci_low, ymax = false_positive_ci_high),
      position = dodge, width = 0.18, color = "grey25", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(y = false_positive_ci_high, label = atlas_format_rate(false_positive_rate)),
      position = dodge, vjust = -0.6, size = 3.4, color = "grey20", na.rm = TRUE
    ) +
    ggplot2::scale_color_identity(guide = "none") +
    atlas_series_scales(levels(rows$series))[[3]] +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      x = NULL, y = "Product-term rejection rate",
      title = "Manuscript anchors, recomputed by the atlas",
      subtitle = paste(
        "Bars with a black outline are fitted on the generating scale: their rate is nominal.",
        "\nEvery other bar is a pseudo-interaction rate. The dashed line marks alpha = .05."
      )
    )
}

atlas_anchor_table <- function(rows) {
  if (!nrow(rows)) return(data.frame())
  out <- data.frame(
    Scenario = as.character(rows$anchor_x),
    `Generating scale` = as.character(rows$generating_link),
    `Fitted model` = as.character(rows$model_label),
    `Matches generating scale` = ifelse(rows$matched, "yes", "no"),
    `Rejection rate [95% CI]` = sprintf(
      "%.3f [%.3f, %.3f]", rows$false_positive_rate,
      rows$false_positive_ci_low, rows$false_positive_ci_high
    ),
    `Induced product term` = atlas_format_value(rows$deterministic_pseudo_interaction, 4),
    `Fit success` = sprintf("%.1f%%", 100 * rows$fit_success_rate),
    `Replications` = rows$B_requested,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# ------------------------------------------------ sensitivity beyond anchors --

# All one-dimensional slices of a family in a single long frame, with the panel
# named after the parameter that actually varies. This replaces a slice / value
# / model dropdown cascade with one small-multiples figure.
atlas_slice_overview_data <- function(core, family) {
  expanded <- atlas_expand_membership(core[core$family == family, , drop = FALSE])
  if (!nrow(expanded)) return(expanded[0, , drop = FALSE])
  expanded <- expanded[expanded$slice_view %in% ATLAS_ONE_D_SLICES, , drop = FALSE]
  if (!nrow(expanded)) return(expanded)
  parameter <- vapply(expanded$slice_view, function(slice) {
    value <- atlas_slice_parameter(family, slice)
    if (is.na(value)) NA_character_ else value
  }, character(1), USE.NAMES = FALSE)
  expanded$parameter <- parameter
  expanded$x_value <- vapply(seq_len(nrow(expanded)), function(i) {
    column <- expanded$parameter[i]
    if (is.na(column) || !column %in% names(expanded)) return(NA_real_)
    as.numeric(expanded[[column]][i])
  }, numeric(1))
  expanded <- expanded[is.finite(expanded$x_value), , drop = FALSE]
  if (!nrow(expanded)) return(expanded)
  panel_order <- intersect(ATLAS_ONE_D_SLICES, unique(expanded$slice_view))
  expanded$panel <- factor(
    atlas_parameter_label(expanded$parameter),
    levels = atlas_parameter_label(vapply(panel_order, function(slice)
      atlas_slice_parameter(family, slice), character(1), USE.NAMES = FALSE))
  )
  expanded$series <- atlas_pair_label(expanded)
  expanded$matched <- atlas_is_matched(expanded)
  expanded$series <- factor(expanded$series, levels = sort(unique(expanded$series)))
  expanded[order(expanded$panel, expanded$series, expanded$x_value), , drop = FALSE]
}

atlas_slice_overview_plot <- function(data, metric = "detection") {
  if (!nrow(data)) return(NULL)
  column <- atlas_metric_column(metric)
  if (!column %in% names(data)) return(NULL)
  scales <- atlas_series_scales(levels(data$series))
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x_value, .data[[column]], color = series, linetype = series, shape = series)
  )
  if (identical(metric, "detection")) {
    plot <- plot +
      ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey45") +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = false_positive_ci_low, ymax = false_positive_ci_high),
        width = 0, alpha = 0.4, linetype = "solid", na.rm = TRUE
      )
  } else {
    plot <- plot + ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "grey45")
  }
  plot <- plot +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, na.rm = TRUE) +
    ggplot2::facet_wrap(~panel, scales = "free_x", nrow = 2) +
    scales[c(1, 2)] +
    ggplot2::scale_shape_manual(
      values = stats::setNames(c(16, 17, 15, 18, 8, 3, 4, 7)[seq_along(levels(data$series))],
                               levels(data$series)),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Value of the parameter named in each panel",
      y = atlas_metric_label(metric),
      title = "One panel per sensitivity slice; inside a panel only the named parameter moves",
      subtitle = paste(
        "The generating-scale product term is exactly zero everywhere.",
        if (identical(metric, "detection")) "\nBars are Wilson 95% intervals; the dashed line marks alpha = .05."
        else "\nThe dotted line marks an induced product term of exactly zero, as under a matched link."
      )
    )
  if (identical(metric, "detection")) {
    plot <- plot + ggplot2::coord_cartesian(ylim = c(0, 1))
  }
  plot
}

atlas_surface_data <- function(core, family) {
  rows <- atlas_slice_data(core, family, "main_effect_surface")
  if (!nrow(rows)) return(rows)
  rows$series <- atlas_pair_label(rows)
  rows$matched <- atlas_is_matched(rows)
  rows[order(rows$series, rows$beta_main_1_multiplier, rows$beta_main_2_multiplier), , drop = FALSE]
}

atlas_surface_plot <- function(data, metric = "detection") {
  if (!nrow(data)) return(NULL)
  column <- atlas_metric_column(metric)
  if (!column %in% names(data)) return(NULL)
  data$value <- data[[column]]
  bar <- ggplot2::guide_colorbar(
    barheight = grid::unit(9, "lines"), title.position = "top"
  )
  if (identical(metric, "detection")) {
    data$label <- atlas_format_rate(data$value)
    fill_scale <- ggplot2::scale_fill_gradient(
      low = "#F2F7FB", high = "#08519C", limits = c(0, 1),
      name = "Detection\nrate", na.value = "grey90", guide = bar
    )
    data$text_color <- ifelse(is.finite(data$value) & data$value > 0.55, "white", "grey15")
  } else {
    # sprintf would print a value rounding to zero from below as "-0.00".
    data$label <- ifelse(is.finite(data$value),
                         sub("^-0[.]00$", "0.00", sprintf("%.2f", data$value)), "")
    limit <- max(abs(data$value[is.finite(data$value)]), na.rm = TRUE)
    if (!is.finite(limit) || limit == 0) limit <- 1
    fill_scale <- ggplot2::scale_fill_gradient2(
      low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0,
      limits = c(-limit, limit), name = "Induced\nproduct term",
      na.value = "grey90", guide = bar
    )
    data$text_color <- "grey15"
  }
  ggplot2::ggplot(data, ggplot2::aes(factor(beta_main_1_multiplier),
                                     factor(beta_main_2_multiplier), fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = label, color = text_color), size = 3.4) +
    ggplot2::scale_color_identity(guide = "none") +
    fill_scale +
    ggplot2::facet_wrap(~series) +
    ggplot2::labs(
      x = "First main-effect multiplier", y = "Second main-effect multiplier",
      title = "Main-effect surface: both main effects move, the product term stays zero",
      subtitle = paste(
        "One panel per fitted model.",
        "\nTiles are precomputed scenarios, not interpolated;", atlas_metric_label(metric), "shown."
      )
    )
}

# ------------------------------------------------------- single-scenario view --

# One flat, grouped dropdown replaces the slice / parameter / model cascade.
atlas_scenario_choices <- function(core, family) {
  scenarios <- core[core$family == family, , drop = FALSE]
  if (!nrow(scenarios)) return(list())
  scenarios <- scenarios[!duplicated(scenarios$scenario_id), , drop = FALSE]
  preferred <- c("paper_anchor", ATLAS_ONE_D_SLICES, "main_effect_surface")
  slices <- c(intersect(preferred, unique(scenarios$slice)),
              setdiff(unique(scenarios$slice), preferred))
  groups <- lapply(slices, function(slice) {
    rows <- scenarios[scenarios$slice == slice, , drop = FALSE]
    rows <- rows[order(rows$scenario_id), , drop = FALSE]
    stats::setNames(
      rows$scenario_id,
      sprintf("%s  (%s)", atlas_scenario_display_label(rows), rows$scenario_id)
    )
  })
  names(groups) <- atlas_slice_label(slices)
  groups
}

atlas_scenario_rows <- function(core, scenario_id) {
  rows <- core[core$scenario_id == scenario_id, , drop = FALSE]
  if (!nrow(rows)) return(rows)
  rows$matched <- atlas_is_matched(rows)
  rows[order(!rows$matched, rows$model_label), , drop = FALSE]
}

atlas_scenario_headline <- function(rows) {
  if (!nrow(rows)) return("No matching precomputed scenario.")
  row <- rows[1, , drop = FALSE]
  guide <- atlas_guide(row$family)
  anchor <- if (isTRUE(row$paper_anchor)) {
    sprintf("Manuscript anchor of %s. ", if (is.null(guide)) "the paper" else guide$case)
  } else ""
  sprintf(
    "%s%s (%s). Generating scale: %s, with a product term of exactly zero. %d replications per fitted model.",
    anchor, atlas_scenario_display_label(row), row$scenario_id,
    row$generating_link, row$B_requested
  )
}

atlas_scenario_table <- function(rows) {
  if (!nrow(rows)) return(data.frame())
  out <- data.frame(
    `Fitted model` = as.character(rows$model_label),
    `Matches generating scale` = ifelse(rows$matched, "yes", "no"),
    `Rejection rate [95% CI]` = sprintf(
      "%.3f [%.3f, %.3f]", rows$false_positive_rate,
      rows$false_positive_ci_low, rows$false_positive_ci_high
    ),
    `Rate type` = as.character(rows$rate_type),
    `Induced product term` = atlas_format_value(rows$deterministic_pseudo_interaction, 4),
    `Fit success` = sprintf("%.1f%%", 100 * rows$fit_success_rate),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

atlas_expected_data <- function(row) {
  if (!nrow(row)) return(data.frame())
  row <- row[1, , drop = FALSE]
  if (row$family == "forced_choice") {
    age <- seq(row$age_min, row$age_max, length.out = 121)
    grid <- expand.grid(age = age, group_num = c(0, 1))
    eta <- row$beta_intercept + row$beta_age * (grid$age - row$age_center) +
      row$beta_group * grid$group_num +
      row$beta_age_group * (grid$age - row$age_center) * grid$group_num
    grid$value <- row$chance + (1 - row$chance) * stats::plogis(eta)
    grid$group <- factor(grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    grid$x <- grid$age
    return(grid)
  }
  if (row$family == "sum_scores") {
    x <- seq(-2.5, 2.5, length.out = 101)
    grid <- expand.grid(x = x, group_num = c(0, 1))
    offsets <- seq(-0.45, 0.45, length.out = row$J)
    thresholds <- matrix(rep(c(-1, 0, 1), each = row$J), nrow = row$J) +
      offsets + row$threshold_shift
    quadrature <- stats::qnorm((seq_len(81) - 0.5) / 81) * row$theta_sd
    latent <- row$beta_x * grid$x + row$beta_group * grid$group_num
    grid$value <- vapply(latent, function(mu) {
      mean(vapply(mu + quadrature, function(theta) {
        sum(stats::plogis(theta - as.vector(thresholds)))
      }, numeric(1)))
    }, numeric(1))
    grid$group <- factor(grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    return(grid)
  }
  grid <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  eta <- row$beta_intercept + row$beta_group * grid$group_num +
    row$beta_condition * grid$condition_num
  grid$value <- if (row$generating_link == "logit") stats::plogis(eta) else stats::pnorm(eta)
  grid$group <- factor(grid$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  grid$condition <- factor(grid$condition_num, levels = c(0, 1),
                           labels = c("Condition 0", "Condition 1"))
  grid
}

atlas_expected_plot <- function(row) {
  data <- atlas_expected_data(row)
  if (!nrow(data)) return(NULL)
  common <- list(
    ggplot2::scale_color_manual(values = ATLAS_COLORS[1:2], name = NULL),
    ggplot2::scale_linetype_manual(values = c("solid", "42"), name = NULL)
  )
  if (row$family[1] == "forced_choice") {
    return(ggplot2::ggplot(data, ggplot2::aes(x, value, color = group, linetype = group)) +
      ggplot2::geom_hline(yintercept = row$chance[1], linetype = "dotted", color = "grey55") +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      common +
      ggplot2::labs(x = "Age", y = "Expected accuracy",
                    title = "Generating expected values",
                    subtitle = "Dotted line marks the chance floor") +
      ggplot2::theme_minimal(base_size = 13))
  }
  if (row$family[1] == "sum_scores") {
    return(ggplot2::ggplot(data, ggplot2::aes(x, value, color = group, linetype = group)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::coord_cartesian(ylim = c(0, row$J[1] * row$item_max[1])) +
      common +
      ggplot2::labs(x = "x", y = "Expected sum score",
                    title = "Generating expected values",
                    subtitle = "The axis spans the full 0-to-maximum score range") +
      ggplot2::theme_minimal(base_size = 13))
  }
  ggplot2::ggplot(data, ggplot2::aes(condition, value, color = group, group = group,
                                     linetype = group)) +
    ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 3) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    common +
    ggplot2::labs(x = NULL, y = "Probability at random intercept = 0",
                  title = "Generating four-cell probabilities") +
    ggplot2::theme_minimal(base_size = 13)
}

# ------------------------------------------------------------- diagnostics ----

# Diagnostic slices of one family in a single long frame: the pseudo-interaction
# rate and every computed check share one panel per varied parameter.
atlas_diagnostic_overview_data <- function(diagnostic, family) {
  if (!nrow(diagnostic)) return(data.frame())
  expanded <- atlas_expand_membership(diagnostic[diagnostic$family == family, , drop = FALSE])
  if (!nrow(expanded)) return(data.frame())
  expanded <- expanded[expanded$slice_view %in% c("sample_size", "severity"), , drop = FALSE]
  if (!nrow(expanded)) return(data.frame())
  expanded$parameter <- vapply(expanded$slice_view, atlas_diagnostic_parameter,
                               character(1), family = family, USE.NAMES = FALSE)
  expanded$x_value <- vapply(seq_len(nrow(expanded)), function(i) {
    column <- expanded$parameter[i]
    if (is.na(column) || !column %in% names(expanded)) return(NA_real_)
    as.numeric(expanded[[column]][i])
  }, numeric(1))
  expanded <- expanded[is.finite(expanded$x_value), , drop = FALSE]
  if (!nrow(expanded)) return(data.frame())
  expanded$panel <- atlas_parameter_label(expanded$parameter)

  interaction <- unique(expanded[c("panel", "x_value", "pseudo_interaction_detection_rate")])
  names(interaction)[names(interaction) == "pseudo_interaction_detection_rate"] <- "rate"
  interaction$series <- "Pseudo-interaction detection"
  computed <- expanded[expanded$computed %in% TRUE, , drop = FALSE]
  checks <- data.frame(
    panel = computed$panel, x_value = computed$x_value,
    rate = computed$detection_rate, series = as.character(computed$diagnostic),
    stringsAsFactors = FALSE
  )
  out <- rbind(interaction, checks)
  out <- out[is.finite(out$rate), , drop = FALSE]
  if (!nrow(out)) return(data.frame())
  series_levels <- c("Pseudo-interaction detection",
                     sort(setdiff(unique(out$series), "Pseudo-interaction detection")))
  out$series <- factor(out$series, levels = series_levels)
  out$panel <- factor(out$panel, levels = unique(expanded$panel))
  out[order(out$panel, out$series, out$x_value), , drop = FALSE]
}

atlas_diagnostic_overview_plot <- function(data) {
  if (!nrow(data)) return(NULL)
  scales <- atlas_series_scales(levels(data$series))
  ggplot2::ggplot(data, ggplot2::aes(x_value, rate, color = series,
                                     linetype = series, shape = series)) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dotted", color = "grey50") +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_point(size = 2.4, na.rm = TRUE) +
    ggplot2::facet_wrap(~panel, scales = "free_x") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    scales[c(1, 2)] +
    ggplot2::scale_shape_manual(
      values = stats::setNames(c(16, 17, 15, 18, 8, 3, 4, 7)[seq_along(levels(data$series))],
                               levels(data$series)),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Value of the parameter named in each panel",
      y = "Detection / flagging rate",
      title = "Does a routine check fire where the wrong link invents an interaction?",
      subtitle = paste(
        "First series: how often the misspecified model reported an interaction.",
        "\nOther series: how often each check flagged a problem. The dotted line marks .05."
      )
    )
}

# One row per scenario: the AIC comparison is a scenario-level quantity, so it
# is repeated across the diagnostic rows of the same scenario.
atlas_aic_delta_table <- function(data) {
  columns <- c("aic_delta_n", "aic_delta_median", "aic_delta_q25", "aic_delta_q75",
               "aic_delta_within_two_rate")
  if (!nrow(data) || !all(columns %in% names(data))) return(data.frame())
  rows <- unique(data[c("scenario_id", "scenario_label", columns)])
  # A scenario with no finite comparison contributes no information; the app
  # reports its absence instead of printing an all-missing row.
  rows <- rows[is.finite(rows$aic_delta_n) & rows$aic_delta_n > 0, , drop = FALSE]
  if (!nrow(rows)) return(data.frame())
  rows <- rows[order(rows$scenario_id), , drop = FALSE]
  out <- data.frame(
    Scenario = rows$scenario_id,
    Description = rows$scenario_label,
    `Median dAIC (wrong - target)` = atlas_format_value(rows$aic_delta_median, 3),
    IQR = ifelse(
      is.finite(rows$aic_delta_q25) & is.finite(rows$aic_delta_q75),
      paste0("[", atlas_format_value(rows$aic_delta_q25, 3), ", ",
             atlas_format_value(rows$aic_delta_q75, 3), "]"),
      "--"
    ),
    `Within 2 units` = ifelse(is.finite(rows$aic_delta_within_two_rate),
                              sprintf("%.1f%%", 100 * rows$aic_delta_within_two_rate), "--"),
    Comparisons = rows$aic_delta_n,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}
