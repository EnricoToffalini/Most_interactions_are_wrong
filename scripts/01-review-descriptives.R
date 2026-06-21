source("R/project-settings.R")

read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

read_xlsx1 <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required by scripts/01-review-descriptives.R")
  }
  as.data.frame(readxl::read_excel(path, sheet = 1), stringsAsFactors = FALSE)
}

pick_col <- function(data, candidates = character(), patterns = character(), required = TRUE) {
  nm <- names(data)
  hit <- nm[nm %in% candidates]
  if (!length(hit) && length(patterns)) {
    hit <- nm[vapply(
      nm,
      function(x) any(grepl(paste(patterns, collapse = "|"), x, ignore.case = TRUE)),
      logical(1)
    )]
  }
  if (!length(hit)) {
    if (required) stop("Column not found: ", paste(candidates, collapse = ", "))
    return(NULL)
  }
  hit[[1]]
}

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

safe_md5 <- function(path) {
  if ("md5sum" %in% getNamespaceExports("tools")) {
    return(unname(tools::md5sum(path)[[1]]))
  }
  NA_character_
}

cohen_kappa01 <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  x <- as01(x[keep])
  y <- as01(y[keep])
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  n <- length(x)
  if (!n) {
    return(data.frame(
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
    ))
  }
  n_00 <- sum(x == 0 & y == 0)
  n_01 <- sum(x == 0 & y == 1)
  n_10 <- sum(x == 1 & y == 0)
  n_11 <- sum(x == 1 & y == 1)
  p0 <- (n_00 + n_11) / n
  px1 <- mean(x == 1)
  py1 <- mean(y == 1)
  pe <- px1 * py1 + (1 - px1) * (1 - py1)
  kappa <- if (isTRUE(all.equal(pe, 1))) NA_real_ else (p0 - pe) / (1 - pe)
  data.frame(
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

normalize_journal <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Journal of experimental psychology. General"] <- "Journal of Experimental Psychology: General"
  x[x == "Journal of Experimental Psychology General"] <- "Journal of Experimental Psychology: General"
  x
}

count_nonempty_rows <- function(data) {
  keep <- apply(data, 1, function(row) any(!is.na(row) & trimws(as.character(row)) != ""))
  sum(keep)
}

sum01 <- function(x) sum(as01(x) == 1, na.rm = TRUE)

flag_row <- function(row_id, quantity, n, denominator_n = NA_real_) {
  ci <- if (!is.na(denominator_n) && denominator_n > 0) wilson_ci_percent(n, denominator_n) else c(NA_real_, NA_real_)
  data.frame(
    row_id = row_id,
    quantity = quantity,
    n = n,
    denominator_n = denominator_n,
    percent = pct(n, denominator_n),
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    stringsAsFactors = FALSE
  )
}

has_any_pattern <- function(x, patterns) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  if (!length(patterns)) return(rep(FALSE, length(x)))
  pattern <- paste(patterns, collapse = "|")
  grepl(pattern, x, perl = TRUE)
}

settings <- list(
  data_file = "Literature_review/final-dataset-review.csv",
  screening_file = "Literature_review/0_screeningDataset.xlsx",
  eligibility_file = "Literature_review/1_screeningEligibilityDataset_2coders.xlsx",
  output_files = c(
    "tables/table1-review-summary.csv",
    "tables/table2-review-outcome-types.csv",
    "tables/tableS1-review-by-journal.csv",
    "tables/tableS2-review-intercoder-agreement.csv",
    "tables/tableS3-review-screening-flow.csv",
    "outputs/review-summary.rds"
  ),
  sem_keywords = c(
    "sem", "structural equation", "latent factor", "latent variable",
    "latent variables", "latent trajectory", "latent growth"
  )
)

final_data <- read_csv(settings$data_file)
screening_data <- read_xlsx1(settings$screening_file)
eligibility_data <- read_xlsx1(settings$eligibility_file)

# Prefer journal-title fields; Source is only a fallback because it may store a database label such as Scopus.
journal_col <- pick_col(
  final_data,
  candidates = c("Source.title", "Publication.title", "Journal", "Journal.title", "journal", "Source"),
  patterns = c("^source\\.title$", "^publication\\.title$", "^journal$", "^journal\\.title$", "^source$"),
  required = FALSE
)
if (is.null(journal_col)) stop("Column not found: Source.title, Publication.title, Journal, Journal.title, journal, Source")

distinct_nonmissing <- function(x) {
  x <- trimws(as.character(x))
  unique(x[!is.na(x) & x != ""])
}

if (identical(journal_col, "Source")) {
  source_values <- distinct_nonmissing(final_data[[journal_col]])
  if (length(source_values) == 1 && identical(source_values[[1]], "Scopus") && "Source.title" %in% names(final_data)) {
    source_title_values <- distinct_nonmissing(final_data[["Source.title"]])
    if (length(source_title_values) > 1) {
      journal_col <- "Source.title"
    }
  }
}

required_main_vars <- c(
  "Uses_non_identity_link_function",
  "Explicit_link_function",
  "Incorrect_identity_link_function",
  "Finds_significant_interaction"
)

for (v in required_main_vars) {
  if (!v %in% names(final_data)) stop("Missing column in final dataset: ", v)
  final_data[[v]] <- as01(final_data[[v]])
}

eligible_col <- pick_col(final_data, candidates = "Eligible", patterns = "^Eligible$", required = FALSE)
if (!is.null(eligible_col)) {
  eligible_data <- final_data[as01(final_data[[eligible_col]]) == 1, , drop = FALSE]
} else {
  eligible_data <- final_data
}

tests_interactions_col <- pick_col(
  eligible_data,
  candidates = "Tests_interactions",
  patterns = c("^Tests_interactions$", "interaction"),
  required = FALSE
)

if (!is.null(tests_interactions_col)) {
  interaction_data <- eligible_data[as01(eligible_data[[tests_interactions_col]]) == 1, , drop = FALSE]
} else {
  interaction_testing <- Reduce(`|`, lapply(required_main_vars, function(v) !is.na(eligible_data[[v]])))
  interaction_data <- eligible_data[interaction_testing, , drop = FALSE]
}

eligible_data[[journal_col]] <- normalize_journal(eligible_data[[journal_col]])
interaction_data[[journal_col]] <- normalize_journal(interaction_data[[journal_col]])

n_eligible <- nrow(eligible_data)
n_interactions <- nrow(interaction_data)
n_non_interactions <- n_eligible - n_interactions
n_non_identity <- sum01(interaction_data$Uses_non_identity_link_function)
n_explicit <- sum01(interaction_data$Explicit_link_function)
n_incorrect_identity <- sum01(interaction_data$Incorrect_identity_link_function)
n_significant_incorrect_identity <- sum(
  interaction_data$Incorrect_identity_link_function == 1 &
    interaction_data$Finds_significant_interaction == 1,
  na.rm = TRUE
)

table1 <- rbind(
  flag_row("eligible_empirical", "Eligible empirical articles", n_eligible, n_eligible),
  flag_row("testing_interactions", "Eligible empirical articles testing at least one interaction", n_interactions, n_eligible),
  flag_row("non_identity_link", "Interaction-testing articles using at least one non-identity link", n_non_identity, n_interactions),
  flag_row("explicit_link", "Interaction-testing articles explicitly reporting the link function", n_explicit, n_interactions),
  flag_row("incorrect_identity", "Interaction-testing articles with formally incorrect identity-link analyses of constrained observed outcomes", n_incorrect_identity, n_interactions),
  flag_row("significant_incorrect_identity", "Formally incorrect identity-link cases with at least one significant interaction", n_significant_incorrect_identity, n_incorrect_identity),
  flag_row("eligible_not_testing_interactions", "Eligible empirical articles not testing interactions", n_non_interactions, n_eligible)
)

response_types_col <- pick_col(
  interaction_data,
  candidates = "Response_variable_types",
  patterns = c("^Response_variable_types$", "response.*type"),
  required = TRUE
)
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
    n_i <- sum(has_any_pattern(response_types, outcome_spec$patterns[[i]]), na.rm = TRUE)
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
      non_identity_link_n = sum01(interaction_j$Uses_non_identity_link_function),
      non_identity_link_percent_interaction = pct(sum01(interaction_j$Uses_non_identity_link_function), nrow(interaction_j)),
      explicit_link_n = sum01(interaction_j$Explicit_link_function),
      explicit_link_percent_interaction = pct(sum01(interaction_j$Explicit_link_function), nrow(interaction_j)),
      incorrect_identity_n = sum01(interaction_j$Incorrect_identity_link_function),
      incorrect_identity_percent_interaction = pct(sum01(interaction_j$Incorrect_identity_link_function), nrow(interaction_j)),
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
    "Formally incorrect identity-link analyses of constrained observed outcomes",
    "Finds significant interaction"
  ),
  stringsAsFactors = FALSE
)

intercoder_agreement_table <- do.call(
  rbind,
  lapply(seq_len(nrow(agreement_spec)), function(i) {
    v <- agreement_spec$variable[[i]]
    cd1 <- pick_col(
      interaction_data,
      candidates = paste0("CD1_", v),
      patterns = paste0("^CD1_.*", v, "$")
    )
    cd2 <- pick_col(
      interaction_data,
      candidates = paste0("CD2_", v),
      patterns = paste0("^CD2_.*", v, "$")
    )
    out <- cohen_kappa01(interaction_data[[cd1]], interaction_data[[cd2]])
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

search_sem_hits <- function(data, cols, pattern) {
  if (!length(cols)) return(rep(FALSE, nrow(data)))
  apply(
    data[, cols, drop = FALSE],
    1,
    function(row) any(grepl(pattern, paste(row, collapse = " | "), ignore.case = TRUE))
  )
}

sem_hits_explicit <- search_sem_hits(eligibility_data, text_cols, sem_regex)
sem_hits_notes <- search_sem_hits(eligibility_data, note_cols, sem_regex)
sem_n <- if (any(sem_hits_explicit)) sum(sem_hits_explicit, na.rm = TRUE) else sum(sem_hits_notes, na.rm = TRUE)
sem_basis <- if (any(sem_hits_explicit)) "explicit exclusion field" else "note-based keyword search"

screening_flow_table <- data.frame(
  step = c(
    "total screened records",
    "eligible empirical articles in final dataset",
    "eligible articles testing at least one interaction",
    "eligible articles not testing interactions",
    "note-based SEM / latent-variable-only exclusions"
  ),
  n = c(
    count_nonempty_rows(screening_data),
    n_eligible,
    n_interactions,
    n_non_interactions,
    sem_n
  ),
  basis = c(
    "screening workbook row count",
    if (!is.null(eligible_col)) "Eligible == 1 in final review dataset" else "final review dataset row count",
    if (!is.null(tests_interactions_col)) "Tests_interactions == 1 in eligible rows" else "review flags present in final dataset",
    "eligible minus interaction-testing",
    sem_basis
  ),
  stringsAsFactors = FALSE
)

write.csv(table1, "tables/table1-review-summary.csv", row.names = FALSE)
write.csv(outcome_table, "tables/table2-review-outcome-types.csv", row.names = FALSE)
write.csv(by_journal_table, "tables/tableS1-review-by-journal.csv", row.names = FALSE)
write.csv(intercoder_agreement_table, "tables/tableS2-review-intercoder-agreement.csv", row.names = FALSE)
write.csv(screening_flow_table, "tables/tableS3-review-screening-flow.csv", row.names = FALSE)

summary_rds <- list(
  settings = settings,
  data_file = settings$data_file,
  data_file_md5 = safe_md5(settings$data_file),
  n_rows_raw = count_nonempty_rows(screening_data),
  n_rows_eligible = n_eligible,
  n_rows_interaction_testing = n_interactions,
  table1 = table1,
  outcome_table = outcome_table,
  by_journal_table = by_journal_table,
  intercoder_agreement_table = intercoder_agreement_table,
  screening_flow_table = screening_flow_table,
  generated_at = Sys.time(),
  r_version = R.version.string
)

saveRDS(summary_rds, "outputs/review-summary.rds")
