# scripts/01-review-descriptives.R

# ============================================================
# Preregistered review descriptives
# The Link Function Problem paper
# ============================================================

rm(list = ls())

dir.create("outputs", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("paper/figs", recursive = TRUE, showWarnings = FALSE)

data_path <- "Literature_review/final-dataset-review.csv"

if (!file.exists(data_path)) {
  stop(
    "Could not find ", data_path,
    ". Run this script from the root of the repo.",
    call. = FALSE
  )
}

d <- read.csv(data_path, stringsAsFactors = FALSE)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

as01 <- function(x) {
  as.numeric(x)
}

wilson_ci <- function(x, n, conf = .95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- x / n
  
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  
  c(lower = center - half, upper = center + half)
}

fmt_pct <- function(x) {
  round(100 * x, 1)
}

make_row <- function(label, x, n, denominator, note = "") {
  ci <- wilson_ci(x, n)
  
  data.frame(
    quantity = label,
    n = x,
    denominator_n = n,
    percent = fmt_pct(x / n),
    ci_low = fmt_pct(ci["lower"]),
    ci_high = fmt_pct(ci["upper"]),
    denominator = denominator,
    note = note,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Clean key fields
# ------------------------------------------------------------

d$Eligible <- as01(d$Eligible)
d$Tests_interactions <- as01(d$Tests_interactions)
d$Uses_non_identity_link_function <- as01(d$Uses_non_identity_link_function)
d$Explicit_link_function <- as01(d$Explicit_link_function)
d$Incorrect_identity_link_function <- as01(d$Incorrect_identity_link_function)
d$Finds_significant_interaction <- as01(d$Finds_significant_interaction)

# Normalize one duplicate journal label if present
d$Source.title <- gsub(
  "Journal of experimental psychology. General",
  "Journal of Experimental Psychology: General",
  d$Source.title,
  fixed = TRUE
)

eligible <- d[d$Eligible == 1, ]
interaction_articles <- eligible[eligible$Tests_interactions == 1, ]
problematic_identity <- interaction_articles[
  interaction_articles$Incorrect_identity_link_function == 1,
]

# ------------------------------------------------------------
# Main review summary table
# ------------------------------------------------------------

n_eligible <- nrow(eligible)
n_interactions <- nrow(interaction_articles)
n_problematic_identity <- nrow(problematic_identity)

table1 <- rbind(
  make_row(
    label = "Eligible empirical articles reviewed",
    x = n_eligible,
    n = n_eligible,
    denominator = "Eligible articles",
    note = "Articles passing eligibility screening."
  ),
  make_row(
    label = "Articles testing at least one interaction",
    x = sum(eligible$Tests_interactions == 1, na.rm = TRUE),
    n = n_eligible,
    denominator = "Eligible articles",
    note = "Interaction tested via statistical model or ANOVA."
  ),
  make_row(
    label = "Interaction-testing articles using a non-identity link",
    x = sum(interaction_articles$Uses_non_identity_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "At least one tested interaction was analyzed with a non-identity link."
  ),
  make_row(
    label = "Interaction-testing articles explicitly reporting the link function",
    x = sum(interaction_articles$Explicit_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "The link itself was explicitly declared or identifiable from the report."
  ),
  make_row(
    label = "Interaction-testing articles with a potentially problematic identity-link analysis",
    x = sum(interaction_articles$Incorrect_identity_link_function == 1, na.rm = TRUE),
    n = n_interactions,
    denominator = "Articles testing interactions",
    note = "Coded when at least one interaction used an identity-link analysis for an outcome where a non-identity link appeared more appropriate."
  ),
  make_row(
    label = "Potentially problematic identity-link cases reporting at least one significant interaction",
    x = sum(problematic_identity$Finds_significant_interaction == 1, na.rm = TRUE),
    n = n_problematic_identity,
    denominator = "Potentially problematic identity-link cases",
    note = "At least one such interaction was reported as statistically significant or interpreted analogously."
  )
)

write.csv(
  table1,
  file = "tables/table1-review-summary.csv",
  row.names = FALSE
)

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

journal_table$interaction_percent <- fmt_pct(
  journal_table$interaction_n / journal_table$eligible_n
)

journal_table$non_identity_link_percent <- fmt_pct(
  journal_table$non_identity_link_n / journal_table$interaction_n
)

journal_table$explicit_link_percent <- fmt_pct(
  journal_table$explicit_link_n / journal_table$interaction_n
)

journal_table$potentially_problematic_identity_percent <- fmt_pct(
  journal_table$potentially_problematic_identity_n / journal_table$interaction_n
)

write.csv(
  journal_table,
  file = "tables/tableS1-review-by-journal.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# Outcome-type table
# Non-exclusive broad categories.
# One article can contribute to more than one category.
# ------------------------------------------------------------

classify_outcome <- function(x) {
  x <- tolower(x)
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

outcome_table$percent <- fmt_pct(outcome_table$n / outcome_table$denominator_n)

outcome_table <- outcome_table[order(outcome_table$n, decreasing = TRUE), ]

write.csv(
  outcome_table,
  file = "tables/table2-review-outcome-types.csv",
  row.names = FALSE
)

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
  data_path = data_path
)

saveRDS(
  review_summary,
  file = "outputs/review-summary.rds"
)

