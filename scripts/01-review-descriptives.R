# Review descriptives. Run from the repository root.
library(readxl)
dir.create("tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

# The screening and eligibility workbooks store one sheet per journal
# (DS, JEPG, JPSP, PM, PS). Reading only sheet 1 counts a single journal, so
# both the screening-record count and the SEM keyword search must span all
# sheets. Columns are coerced to character so sheets with different inferred
# types row-bind cleanly; both consumers (row counting and keyword search)
# treat cells as text, so this is lossless for those uses.

as01 <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  out <- rep(NA_real_, length(x))
  out[x %in% c("1", "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y")] <- 1
  out[x %in% c("0", "FALSE", "False", "false", "No", "NO", "no", "N", "n")] <- 0
  suppressWarnings(num <- as.numeric(x))
  use_num <- is.na(out) & !is.na(num) & num %in% c(0, 1)
  out[use_num] <- num[use_num]
  out
}

pct <- function(num, den, digits = 1) {
  ifelse(den > 0, round(100 * num / den, digits), NA_real_)
}

wilson_ci_percent <- function(x, n, conf.level = 0.95, digits = 1) {
  if (is.na(n) || n <= 0 || is.na(x)) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / denom
  round(100 * c(max(0, center - half), min(1, center + half)), digits)
}

normalize_journal <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Journal of experimental psychology. General"] <- "Journal of Experimental Psychology: General"
  x[x == "Journal of Experimental Psychology General"] <- "Journal of Experimental Psychology: General"
  x
}

settings <- list(
  data_file = "Literature_review/final-dataset-review.csv",
  screening_file = "Literature_review/0_screeningDataset.xlsx",
  eligibility_file = "Literature_review/1_screeningEligibilityDataset_2coders.xlsx",
  output_files = c(
    "tables/review-summary.csv",
    "tables/review-outcome-types.csv",
    "tables/review-by-journal.csv",
    "tables/review-intercoder-agreement.csv",
    "tables/review-screening-flow.csv",
    "outputs/review-summary.rds"
  ),
  sem_keywords = c(
    "sem", "structural equation", "latent factor", "latent variable",
    "latent variables", "latent trajectory", "latent growth"
  )
)

final_data <- utils::read.csv(settings$data_file, stringsAsFactors = FALSE, check.names = FALSE)
# Read every journal sheet, retaining character cells for row counts and keyword search.
sheets <- readxl::excel_sheets(settings$screening_file)
sheet_data <- list()
for (sheet in sheets) {
  df <- as.data.frame(readxl::read_excel(settings$screening_file, sheet = sheet), stringsAsFactors = FALSE)
  df[] <- lapply(df, as.character)
  df$source_sheet <- sheet
  sheet_data[[length(sheet_data) + 1L]] <- df
}
columns <- unique(unlist(lapply(sheet_data, names)))
for (i in seq_along(sheet_data)) {
  for (column in setdiff(columns, names(sheet_data[[i]]))) sheet_data[[i]][[column]] <- NA_character_
  sheet_data[[i]] <- sheet_data[[i]][columns]
}
screening_data <- do.call(rbind, sheet_data)
# Read every journal sheet, retaining character cells for row counts and keyword search.
sheets <- readxl::excel_sheets(settings$eligibility_file)
sheet_data <- list()
for (sheet in sheets) {
  df <- as.data.frame(readxl::read_excel(settings$eligibility_file, sheet = sheet), stringsAsFactors = FALSE)
  df[] <- lapply(df, as.character)
  df$source_sheet <- sheet
  sheet_data[[length(sheet_data) + 1L]] <- df
}
columns <- unique(unlist(lapply(sheet_data, names)))
for (i in seq_along(sheet_data)) {
  for (column in setdiff(columns, names(sheet_data[[i]]))) sheet_data[[i]][[column]] <- NA_character_
  sheet_data[[i]] <- sheet_data[[i]][columns]
}
eligibility_data <- do.call(rbind, sheet_data)

# Exact column names in the checked-in final review dataset.
journal_col <- "Source.title"
required_main_vars <- c(
  "Uses_non_identity_link_function",
  "Explicit_link_function",
  "Incorrect_identity_link_function",
  "Finds_significant_interaction"
)

for (v in required_main_vars) {
  final_data[[v]] <- as01(final_data[[v]])
}

eligible_col <- "Eligible"
tests_interactions_col <- "Tests_interactions"
eligible_data <- final_data[as01(final_data$Eligible) == 1, , drop = FALSE]
interaction_data <- eligible_data[as01(eligible_data$Tests_interactions) == 1, , drop = FALSE]

eligible_data[[journal_col]] <- normalize_journal(eligible_data[[journal_col]])
interaction_data[[journal_col]] <- normalize_journal(interaction_data[[journal_col]])

n_eligible <- nrow(eligible_data)
n_interactions <- nrow(interaction_data)
n_non_interactions <- n_eligible - n_interactions
n_non_identity <- sum(as01(interaction_data$Uses_non_identity_link_function) == 1, na.rm = TRUE)
n_explicit <- sum(as01(interaction_data$Explicit_link_function) == 1, na.rm = TRUE)
n_incorrect_identity <- sum(as01(interaction_data$Incorrect_identity_link_function) == 1, na.rm = TRUE)
n_significant_incorrect_identity <- sum(
  interaction_data$Incorrect_identity_link_function == 1 &
  interaction_data$Finds_significant_interaction == 1,
  na.rm = TRUE
)

review_summary_table <- data.frame(
  row_id = c("eligible_empirical", "testing_interactions", "non_identity_link", "explicit_link",
    "incorrect_identity", "significant_incorrect_identity", "eligible_not_testing_interactions"),
  quantity = c("Eligible empirical articles", "Eligible empirical articles testing at least one interaction",
    "Interaction-testing articles using at least one non-identity link",
    "Interaction-testing articles with clearly identifiable link function",
    "Interaction-testing articles with Gaussian-identity analyses on constrained observed outcomes",
    "Gaussian-identity cases with at least one significant interaction", "Eligible empirical articles not testing interactions"),
  n = c(n_eligible, n_interactions, n_non_identity, n_explicit, n_incorrect_identity, n_significant_incorrect_identity, n_non_interactions),
  denominator_n = c(n_eligible, n_eligible, n_interactions, n_interactions, n_interactions, n_incorrect_identity, n_eligible),
  stringsAsFactors = FALSE)
review_summary_table$percent <- pct(review_summary_table$n, review_summary_table$denominator_n)
review_summary_table$ci_low <- review_summary_table$ci_high <- NA_real_
for (i in seq_len(nrow(review_summary_table))) {
  ci <- wilson_ci_percent(review_summary_table$n[i], review_summary_table$denominator_n[i])
  review_summary_table$ci_low[i] <- ci[1]
  review_summary_table$ci_high[i] <- ci[2]
}

response_types_col <- "Response_variable_types"
response_types <- tolower(trimws(as.character(interaction_data[[response_types_col]])))
response_types[is.na(response_types)] <- ""

# Short heuristic classifier for broad outcome types. Categories are non-exclusive.
# Matching is based on concise keyword patterns in Response_variable_types.
outcome_spec <- data.frame(
  row_id = c(
    "binary_accuracy_proportion",
    "sum_scores_composites",
    "ordinal_likert_ratings",
    "response_times_durations",
    "counts_error_counts",
    "neural_physiological",
    "correlations_associations",
    "difference_distance_scores"
  ),
  outcome_type = c(
    "Binary / accuracy / proportions",
    "Sum scores / composites",
    "Ordinal / Likert / ratings",
    "Response times / durations",
    "Counts / error counts",
    "Neural / physiological measures",
    "Correlations / associations",
    "Difference / distance scores"
  ),
  patterns = I(list(
      c("\\bbinary\\b", "\\baccuracy\\b", "\\baccuracies\\b", "\\bproportion", "\\bproportions\\b", "\\bpercent\\b", "\\bpercentage\\b"),
      c("\\bsum score", "\\bsum scores", "\\bcomposite", "\\bcomposites", "\\bindex\\b", "\\bindices\\b", "\\bscale score", "\\btotal score"),
      c("\\bordinal\\b", "\\blikert\\b", "\\brating\\b", "\\bratings\\b", "\\branked\\b", "\\brankings\\b"),
      c("\\bresponse time", "\\bresponse times", "\\brt\\b", "\\blatency\\b", "\\blatencies\\b", "\\bduration\\b", "\\bdurations\\b", "\\btime\\b", "\\b1/rt\\b", "\\blog\\(rt\\)\\b", "\\blogrt\\b", "\\bzrt\\b", "\\btimes\\b"),
      c("\\bcount\\b", "\\bcounts\\b", "\\berror count", "\\berror counts", "\\bfrequency\\b", "\\bfrequencies\\b"),
      c("\\bneural\\b", "\\bphysiological\\b", "\\beeg\\b", "\\bfmri\\b", "\\beri?p\\b", "\\bheart rate\\b", "\\bscr\\b", "\\bemg\\b", "\\bpupil", "\\bamplitude\\b", "\\blogamplitude\\b", "\\bhr\\b", "\\bblood pressure\\b"),
      c("\\bcorrelation\\b", "\\bcorrelations\\b", "\\bassociation\\b", "\\bassociations\\b", "\\bcovariance\\b", "\\bcor\\b", "\\bzcor\\b"),
      c("\\bdifference score", "\\bdifference scores", "\\bdistance\\b", "\\bdistances\\b", "\\bdiscrepancy\\b", "\\bdiscrepancies\\b", "\\bdifferences\\b", "\\bdifference-score\\b", "\\bangular\\b")
  )),
  stringsAsFactors = FALSE
)

outcome_table <- do.call(
  rbind,
  lapply(seq_len(nrow(outcome_spec)), function(i) {
      n_i <- sum(grepl(paste(outcome_spec$patterns[[i]], collapse = "|"), response_types, perl = TRUE), na.rm = TRUE)
      data.frame(
        row_id = outcome_spec$row_id[[i]],
        outcome_type = outcome_spec$outcome_type[[i]],
        n = n_i,
        denominator_n = n_interactions,
        percent = pct(n_i, n_interactions),
        stringsAsFactors = FALSE
      )
  })
)

journals <- sort(unique(eligible_data[[journal_col]]))
by_journal_table <- do.call(
  rbind,
  lapply(journals, function(j) {
      eligible_j <- eligible_data[eligible_data[[journal_col]] == j, , drop = FALSE]
      interaction_j <- interaction_data[interaction_data[[journal_col]] == j, , drop = FALSE]
      incorrect_identity_j <- interaction_j$Incorrect_identity_link_function == 1
      significant_incorrect_identity_j <- incorrect_identity_j & interaction_j$Finds_significant_interaction == 1
      data.frame(
        journal = j,
        eligible_n = nrow(eligible_j),
        interaction_testing_n = nrow(interaction_j),
        interaction_testing_percent_of_eligible = pct(nrow(interaction_j), nrow(eligible_j)),
        non_identity_link_n = sum(as01(interaction_j$Uses_non_identity_link_function) == 1, na.rm = TRUE),
        non_identity_link_percent_interaction = pct(sum(as01(interaction_j$Uses_non_identity_link_function) == 1, na.rm = TRUE), nrow(interaction_j)),
        explicit_link_n = sum(as01(interaction_j$Explicit_link_function) == 1, na.rm = TRUE),
        explicit_link_percent_interaction = pct(sum(as01(interaction_j$Explicit_link_function) == 1, na.rm = TRUE), nrow(interaction_j)),
        incorrect_identity_n = sum(as01(interaction_j$Incorrect_identity_link_function) == 1, na.rm = TRUE),
        incorrect_identity_percent_interaction = pct(sum(as01(interaction_j$Incorrect_identity_link_function) == 1, na.rm = TRUE), nrow(interaction_j)),
        significant_incorrect_identity_n = sum(significant_incorrect_identity_j, na.rm = TRUE),
        significant_incorrect_identity_percent_incorrect = pct(sum(significant_incorrect_identity_j, na.rm = TRUE), sum(incorrect_identity_j, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
  })
)

agreement_spec <- data.frame(
  variable = required_main_vars,
  label = c(
    "Uses non-identity link function",
    "Explicit link function",
    "Gaussian-identity analyses on constrained observed outcomes",
    "Finds significant interaction"
  ),
  stringsAsFactors = FALSE
)

intercoder_agreement_table <- do.call(
  rbind,
  lapply(seq_len(nrow(agreement_spec)), function(i) {
      v <- agreement_spec$variable[[i]]
      cd1 <- paste0("CD1_", v)
      cd2 <- paste0("CD2_", v)
      x <- interaction_data[[cd1]]
      y <- interaction_data[[cd2]]

      keep <- !is.na(x) & !is.na(y)
      x <- as01(x[keep])
      y <- as01(y[keep])
      keep <- !is.na(x) & !is.na(y)
      x <- x[keep]
      y <- y[keep]
      n <- length(x)
      if (!n) {
        out <- data.frame(
          n_double_coded = 0,
          agreement = NA_real_,
          kappa = NA_real_,
          coder1_yes_percent = NA_real_,
          coder2_yes_percent = NA_real_,
          coder1_no_percent = NA_real_,
          coder2_no_percent = NA_real_,
          n_disagreements = 0,
          n_00 = 0,
          n_01 = 0,
          n_10 = 0,
          n_11 = 0
        )
      } else {
        n_00 <- sum(x == 0 & y == 0)
        n_01 <- sum(x == 0 & y == 1)
        n_10 <- sum(x == 1 & y == 0)
        n_11 <- sum(x == 1 & y == 1)
        p0 <- (n_00 + n_11) / n
        px1 <- mean(x == 1)
        py1 <- mean(y == 1)
        pe <- px1 * py1 + (1 - px1) * (1 - py1)
        kappa <- if (isTRUE(all.equal(pe, 1))) NA_real_ else (p0 - pe) / (1 - pe)
        out <- data.frame(
          n_double_coded = n,
          agreement = p0,
          kappa = kappa,
          coder1_yes_percent = 100 * mean(x == 1),
          coder2_yes_percent = 100 * mean(y == 1),
          coder1_no_percent = 100 * mean(x == 0),
          coder2_no_percent = 100 * mean(y == 0),
          n_disagreements = n_01 + n_10,
          n_00 = n_00,
          n_01 = n_01,
          n_10 = n_10,
          n_11 = n_11
        )

      }

      cbind(
        variable = v,
        label = agreement_spec$label[[i]],
        out,
        stringsAsFactors = FALSE
      )
  })
)

note_cols <- intersect(c("CD1_Notes", "CD2_Notes", "Combined_notes"), names(eligibility_data))
text_cols <- names(eligibility_data)[grepl("exclude|exclusion|reason", names(eligibility_data), ignore.case = TRUE)]
sem_regex <- paste0("\\b(", paste(settings$sem_keywords, collapse = "|"), ")\\b")

sem_hits_explicit <- if (!length(text_cols)) rep(FALSE, nrow(eligibility_data)) else apply(eligibility_data[, text_cols, drop = FALSE], 1, function(row) any(grepl(sem_regex, paste(row, collapse = " | "), ignore.case = TRUE)))
sem_hits_notes <- if (!length(note_cols)) rep(FALSE, nrow(eligibility_data)) else apply(eligibility_data[, note_cols, drop = FALSE], 1, function(row) any(grepl(sem_regex, paste(row, collapse = " | "), ignore.case = TRUE)))
sem_n <- if (any(sem_hits_explicit)) sum(sem_hits_explicit, na.rm = TRUE) else sum(sem_hits_notes, na.rm = TRUE)
sem_basis <- if (any(sem_hits_explicit)) "explicit exclusion field" else "note-based keyword search"

screening_flow_table <- data.frame(
  step = c(
    "records retrieved across journal sheets",
    "eligible empirical articles in final dataset",
    "eligible articles testing at least one interaction",
    "eligible articles not testing interactions",
    "note-based SEM / latent-variable-only exclusions"
  ),
  n = c(
    sum(apply(screening_data, 1, function(row) any(!is.na(row) & trimws(as.character(row)) != ""))),
    n_eligible,
    n_interactions,
    n_non_interactions,
    sem_n
  ),
  basis = c(
    "non-empty rows across all screening workbook sheets (one per journal)",
    if (!is.null(eligible_col)) "Eligible == 1 in final review dataset" else "final review dataset row count",
    if (!is.null(tests_interactions_col)) "Tests_interactions == 1 in eligible rows" else "review flags present in final dataset",
    "eligible minus interaction-testing",
    sem_basis
  ),
  stringsAsFactors = FALSE
)

write.csv(review_summary_table, "tables/review-summary.csv", row.names = FALSE)
write.csv(outcome_table, "tables/review-outcome-types.csv", row.names = FALSE)
write.csv(by_journal_table, "tables/review-by-journal.csv", row.names = FALSE)
write.csv(intercoder_agreement_table, "tables/review-intercoder-agreement.csv", row.names = FALSE)
write.csv(screening_flow_table, "tables/review-screening-flow.csv", row.names = FALSE)

summary_rds <- list(
  settings = settings,
  data_file = settings$data_file,
  data_file_md5 = unname(tools::md5sum(settings$data_file)[[1]]),
  n_rows_raw = sum(apply(screening_data, 1, function(row) any(!is.na(row) & trimws(as.character(row)) != ""))),
  n_rows_eligible = n_eligible,
  n_rows_interaction_testing = n_interactions,
  review_summary_table = review_summary_table,
  outcome_table = outcome_table,
  by_journal_table = by_journal_table,
  intercoder_agreement_table = intercoder_agreement_table,
  screening_flow_table = screening_flow_table,
  generated_at = Sys.time(),
  r_version = R.version.string
)

saveRDS(summary_rds, "outputs/review-summary.rds")
