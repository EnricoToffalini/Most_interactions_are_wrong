# scripts/01-review-descriptives.R
# Preregistered review descriptives for the Link Function Problem paper.

rm(list = ls())

# ------------------------------------------------------------
# Project setup
# ------------------------------------------------------------

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-summaries.R")

ensure_output_dirs()

report_header("Preregistered review descriptives")

# ------------------------------------------------------------
# Script-specific settings
# ------------------------------------------------------------

settings <- list(
  data_path = "Literature_review/final-dataset-review.csv",
  table1_path = "tables/table1-review-summary.csv",
  journal_table_path = "tables/tableS1-review-by-journal.csv",
  outcome_table_path = "tables/table2-review-outcome-types.csv",
  output_rds_path = "outputs/review-summary.rds",
  confidence_level = 0.95
)

print_compact(list_to_table(settings))

# ------------------------------------------------------------
# Input data
# ------------------------------------------------------------

if (!file.exists(settings$data_path)) {
  stop(
    "Could not find ", settings$data_path,
    ". Run this script from the root of the repository, or check the review data path.",
    call. = FALSE
  )
}

d <- read.csv(settings$data_path, stringsAsFactors = FALSE, check.names = TRUE)

required_columns <- c(
  "Eligible",
  "Tests_interactions",
  "Uses_non_identity_link_function",
  "Explicit_link_function",
  "Incorrect_identity_link_function",
  "Finds_significant_interaction",
  "Response_variable_types",
  "Source.title"
)

missing_columns <- setdiff(required_columns, names(d))
if (length(missing_columns) > 0) {
  stop(
    "The review dataset is missing these required columns: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Local helpers for this review table
# ------------------------------------------------------------

as_binary01 <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  
  if (is.numeric(x)) {
    out <- ifelse(is.na(x), NA_integer_, as.integer(x))
    return(out)
  }
  
  z <- trimws(tolower(as.character(x)))
  out <- rep(NA_integer_, length(z))
  out[z %in% c("1", "yes", "y", "true", "t", "si", "s")] <- 1L
  out[z %in% c("0", "no", "n", "false", "f")] <- 0L
  out
}

safe_pct <- function(x, n, digits = 1) {
  ifelse(
    is.na(n) | n <= 0,
    NA_real_,
    round(100 * x / n, digits)
  )
}

make_review_row <- function(label, x, n, denominator, note = "") {
  x <- ifelse(is.na(x), 0, x)
  ci <- wilson_ci(x, n, conf = settings$confidence_level)
  
  data.frame(
    quantity = label,
    n = x,
    denominator_n = n,
    percent = safe_pct(x, n),
    ci_low = round(100 * ci["lower"], 1),
    ci_high = round(100 * ci["upper"], 1),
    denominator = denominator,
    note = note,
    stringsAsFactors = FALSE
  )
}

normalise_journal_labels <- function(x) {
  x <- gsub(
    "Journal of experimental psychology. General",
    "Journal of Experimental Psychology: General",
    x,
    fixed = TRUE
  )
  x <- gsub(
    "Journal of Experimental Psychology General",
    "Journal of Experimental Psychology: General",
    x,
    fixed = TRUE
  )
  x
}

classify_outcome <- function(x) {
  x <- tolower(as.character(x))
  x[is.na(x)] <- ""
  
  data.frame(
    binary_accuracy_proportions = grepl(
      "binary|accuracy|proportion|percentage|percent|correct|dprime|d-prime|d prime",
      x
    ),
    sum_scores_composites = grepl(
      "sum score|sum scores|span|composite|questionnaire|scale score|total score|score",
      x
    ),
    ordinal_likert_ratings = grepl(
      "ordinal|likert|rating|slider",
      x
    ),
    response_times_durations = grepl(
      "\\brt\\b|logrt|reaction time|response time|times|latency|duration|time interval",
      x
    ),
    counts_error_counts = grepl(
      "count|error",
      x
    ),
    neural_physiological = grepl(
      "amplitude|heart|\\bhr\\b|eeg|erp|bold|fmri|fnirs|mri|ibi",
      x
    ),
    correlations_associations = grepl(
      "\\bcor\\b|correlation|zcor",
      x
    ),
    difference_distance_scores = grepl(
      "difference|deviation|distance|meters|angular",
      x
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Clean key fields
# ------------------------------------------------------------

binary_columns <- c(
  "Eligible",
  "Tests_interactions",
  "Uses_non_identity_link_function",
  "Explicit_link_function",
  "Incorrect_identity_link_function",
  "Finds_significant_interaction"
)

for (nm in binary_columns) {
  d[[nm]] <- as_binary01(d[[nm]])
}

d$Source.title <- normalise_journal_labels(d$Source.title)

eligible <- d[d$Eligible == 1, , drop = FALSE]
interaction_articles <- eligible[eligible$Tests_interactions == 1, , drop = FALSE]
problematic_identity <- interaction_articles[
  interaction_articles$Incorrect_identity_link_function == 1,
  ,
  drop = FALSE
]

n_eligible <- nrow(eligible)
n_interactions <- nrow(interaction_articles)
n_problematic_identity <- nrow(problematic_identity)

report_section("Sample sizes")
print_compact(data.frame(
  quantity = c(
    "Eligible articles",
    "Articles testing interactions",
    "Potentially problematic identity-link cases"
  ),
  n = c(n_eligible, n_interactions, n_problematic_identity),
  stringsAsFactors = FALSE
))

# ------------------------------------------------------------
# Main review summary table
# ------------------------------------------------------------

table1 <- rbind(
  make_review_row(
    label = "Eligible empirical articles reviewed",
    x = n_eligible,
    n = n_eligible,
    denominator = "Eligible articles",
    note = "Articles passing eligibility screening."
  ),
  make_review_row(
    label = "Articles testing at least one interaction",
    x = sum(eligible$Tests_interactions == 1, na.rm = TRUE),
    n = n_eligible,
    denominator = "Eligible articles",
    note = "Interaction tested via statistical model or ANOVA."
  ),
  make_review_row(
    label = "Interaction-testing articles using a non-identity link",
    x = sum(interaction_articles$Uses_non_identity_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "At least one tested interaction was analyzed with a non-identity link."
  ),
  make_review_row(
    label = "Interaction-testing articles explicitly reporting the link function",
    x = sum(interaction_articles$Explicit_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "The link function was explicitly declared or identifiable from the report."
  ),
  make_review_row(
    label = "Interaction-testing articles with a potentially problematic identity-link analysis",
    x = sum(interaction_articles$Incorrect_identity_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "Coded when at least one interaction used an identity-link analysis for an outcome where a non-identity link appeared more appropriate."
  ),
  make_review_row(
    label = "Potentially problematic identity-link cases reporting at least one significant interaction",
    x = sum(problematic_identity$Finds_significant_interaction == 1, na.rm = TRUE),
    n = n_problematic_identity,
    denominator = "Potentially problematic identity-link cases",
    note = "At least one such interaction was reported as statistically significant or interpreted analogously."
  )
)

write.csv(table1, file = settings$table1_path, row.names = FALSE)

# ------------------------------------------------------------
# Journal-level descriptive table
# ------------------------------------------------------------

journal_summary <- aggregate(
  cbind(
    eligible_n = Eligible,
    interaction_n = Tests_interactions
  ) ~ Source.title,
  data = eligible,
  FUN = sum,
  na.rm = TRUE
)

names(journal_summary)[1] <- "journal"

if (n_interactions > 0) {
  journal_interaction <- aggregate(
    cbind(
      non_identity_link_n = Uses_non_identity_link_function,
      explicit_link_n = Explicit_link_function,
      potentially_problematic_identity_n = Incorrect_identity_link_function
    ) ~ Source.title,
    data = interaction_articles,
    FUN = sum,
    na.rm = TRUE
  )
  
  names(journal_interaction)[1] <- "journal"
  
  journal_table <- merge(
    journal_summary,
    journal_interaction,
    by = "journal",
    all.x = TRUE
  )
} else {
  journal_table <- journal_summary
  journal_table$non_identity_link_n <- NA_integer_
  journal_table$explicit_link_n <- NA_integer_
  journal_table$potentially_problematic_identity_n <- NA_integer_
}

count_columns <- c(
  "non_identity_link_n",
  "explicit_link_n",
  "potentially_problematic_identity_n"
)

for (nm in count_columns) {
  journal_table[[nm]][is.na(journal_table[[nm]])] <- 0L
}

journal_table$interaction_percent <- safe_pct(
  journal_table$interaction_n,
  journal_table$eligible_n
)

journal_table$non_identity_link_percent <- safe_pct(
  journal_table$non_identity_link_n,
  journal_table$interaction_n
)

journal_table$explicit_link_percent <- safe_pct(
  journal_table$explicit_link_n,
  journal_table$interaction_n
)

journal_table$potentially_problematic_identity_percent <- safe_pct(
  journal_table$potentially_problematic_identity_n,
  journal_table$interaction_n
)

journal_table <- journal_table[order(journal_table$journal), ]

write.csv(journal_table, file = settings$journal_table_path, row.names = FALSE)

# ------------------------------------------------------------
# Outcome-type table
# Non-exclusive broad categories. One article can contribute to
# more than one category.
# ------------------------------------------------------------

outcome_flags <- classify_outcome(interaction_articles$Response_variable_types)

outcome_table <- data.frame(
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
  n = colSums(outcome_flags, na.rm = TRUE),
  denominator_n = n_interactions,
  stringsAsFactors = FALSE
)

outcome_table$percent <- safe_pct(outcome_table$n, outcome_table$denominator_n)
outcome_table <- outcome_table[order(outcome_table$n, decreasing = TRUE), ]

write.csv(outcome_table, file = settings$outcome_table_path, row.names = FALSE)

# ------------------------------------------------------------
# Save R object
# ------------------------------------------------------------

review_summary <- list(
  table1 = table1,
  journal_table = journal_table,
  outcome_table = outcome_table,
  n_eligible = n_eligible,
  n_interactions = n_interactions,
  n_problematic_identity = n_problematic_identity,
  data_path = settings$data_path,
  settings = settings
)

saveRDS(review_summary, file = settings$output_rds_path)

report_section("Files written")
print_compact(data.frame(
  file = c(
    settings$table1_path,
    settings$journal_table_path,
    settings$outcome_table_path,
    settings$output_rds_path
  ),
  stringsAsFactors = FALSE
))
