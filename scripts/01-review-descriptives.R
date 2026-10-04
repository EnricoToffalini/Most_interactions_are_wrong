# Review descriptives. Run from the repository root.
# Article-level decisions (Eligible, Tests_interactions, Tests_observed_outcome_interactions)
# come from the screening/eligibility workbook, which has one sheet per journal
# (DS, JEPG, JPSP, PM, PS): all sheets must be read. The final review dataset holds
# the detailed-coding sample (eligible articles with observed-outcome interactions).

rm(list = ls())
library(readxl)
dir.create("tables", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# 0/1 flags are stored inconsistently (1/0, TRUE/FALSE, Yes/No, blanks): recode to 1/0/NA
as01 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA, length(x))
  out[x %in% c("1", "TRUE", "True", "true", "Yes", "YES", "yes", "Y", "y")] <- 1
  out[x %in% c("0", "FALSE", "False", "false", "No", "NO", "no", "N", "n")] <- 0
  num <- suppressWarnings(as.numeric(x)) # e.g. "1.0"
  out[is.na(out) & num %in% c(0, 1)] <- num[is.na(out) & num %in% c(0, 1)]
  out
}
pct <- function(num, den) round(100 * num / ifelse(den > 0, den, NA), 1)
normalize_journal <- function(x) {
  x <- trimws(x)
  x[x %in% c("Journal of experimental psychology. General", "Journal of Experimental Psychology General")] <- "Journal of Experimental Psychology: General"
  x
}

####################################################
# Data
####################################################

screening_file <- "Literature_review/0_screeningDataset.xlsx"
eligibility_file <- "Literature_review/1_screeningEligibilityDataset_2coders.xlsx"
final_data <- read.csv("Literature_review/final-dataset-review.csv", check.names = FALSE)

# records retrieved: all rows of all journal sheets of the screening workbook
n_records <- 0
for (sheet in excel_sheets(screening_file)) n_records <- n_records + nrow(read_excel(screening_file, sheet = sheet))

# article-level decisions from all journal sheets of the eligibility workbook
eligibility_data <- NULL
for (sheet in excel_sheets(eligibility_file)) {
  df <- read_excel(eligibility_file, sheet = sheet)
  eligibility_data <- rbind(eligibility_data, data.frame(source_sheet = sheet, journal = as.character(df$Source.title),
    Eligible = as01(df$Eligible), Tests_interactions = as01(df$Tests_interactions),
    Tests_observed_outcome_interactions = as01(df$Tests_observed_outcome_interactions)))
}
n_screened <- sum(!is.na(eligibility_data$Eligible))
eligible_data <- eligibility_data[eligibility_data$Eligible %in% 1, ]
eligible_data$journal <- normalize_journal(eligible_data$journal)

n_eligible <- nrow(eligible_data)
n_interactions <- sum(eligible_data$Tests_interactions == 1)
n_non_interactions <- n_eligible - n_interactions
n_observed <- sum(eligible_data$Tests_interactions == 1 & eligible_data$Tests_observed_outcome_interactions == 1)
n_latent_only <- sum(eligible_data$Tests_interactions == 1 & eligible_data$Tests_observed_outcome_interactions == 0)

# detailed-coding sample: it must coincide with the observed-outcome articles of the screening
main_vars <- c("Uses_non_identity_link_function", "Explicit_link_function", "Incorrect_identity_link_function", "Finds_significant_interaction")
for (v in main_vars) final_data[[v]] <- as01(final_data[[v]])
coded <- final_data[as01(final_data$Eligible) %in% 1 & as01(final_data$Tests_observed_outcome_interactions) %in% 1, ]
coded$journal <- normalize_journal(coded$Source.title)
stopifnot(nrow(coded) == n_observed, nrow(coded) == nrow(final_data))

n_non_identity <- sum(coded$Uses_non_identity_link_function == 1, na.rm = TRUE)
n_explicit <- sum(coded$Explicit_link_function == 1, na.rm = TRUE)
n_incorrect_identity <- sum(coded$Incorrect_identity_link_function == 1, na.rm = TRUE)
n_significant_incorrect_identity <- sum(coded$Incorrect_identity_link_function == 1 & coded$Finds_significant_interaction == 1, na.rm = TRUE)

####################################################
# Summary table with Wilson 95% CIs
####################################################

review_summary <- data.frame(
  row_id = c("eligible_empirical", "testing_interactions", "observed_outcome_interactions", "latent_only_interactions",
             "non_identity_link", "explicit_link", "incorrect_identity", "significant_incorrect_identity",
             "eligible_not_testing_interactions"),
  quantity = c("Screened articles satisfying the eligibility criteria",
               "Eligible empirical articles testing at least one interaction",
               "Interaction-testing articles with at least one observed-outcome interaction (coded sample)",
               "Interaction-testing articles with interactions on latent outcomes only",
               "Coded articles using at least one non-identity link",
               "Coded articles with clearly identifiable link function",
               "Coded articles with Gaussian-identity analyses on constrained observed outcomes",
               "Gaussian-identity cases with at least one significant interaction",
               "Eligible empirical articles not testing interactions"),
  n = c(n_eligible, n_interactions, n_observed, n_latent_only, n_non_identity, n_explicit,
        n_incorrect_identity, n_significant_incorrect_identity, n_non_interactions),
  denominator_n = c(n_screened, n_eligible, n_interactions, n_interactions, n_observed, n_observed,
                    n_observed, n_incorrect_identity, n_eligible))
review_summary$percent <- pct(review_summary$n, review_summary$denominator_n)
review_summary$ci_low <- NA
review_summary$ci_high <- NA
z <- qnorm(0.975)
for (i in 1:nrow(review_summary)) {
  x <- review_summary$n[i]
  n <- review_summary$denominator_n[i]
  if (n <= 0) next
  p <- x / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) / n) + (z^2 / (4 * n^2))) / (1 + z^2 / n)
  review_summary$ci_low[i] <- round(100 * max(0, center - half), 1)
  review_summary$ci_high[i] <- round(100 * min(1, center + half), 1)
}
print(review_summary[, c("row_id", "n", "denominator_n", "percent", "ci_low", "ci_high")])

####################################################
# Outcome types (keyword matching; categories are not exclusive)
####################################################

response_types <- tolower(trimws(coded$Response_variable_types))
response_types[is.na(response_types)] <- ""
outcome_patterns <- list(
  binary_accuracy_proportion = c("\\bbinary\\b", "\\baccuracy\\b", "\\baccuracies\\b", "\\bproportion", "\\bproportions\\b", "\\bpercent\\b", "\\bpercentage\\b"),
  sum_scores_composites = c("\\bsum score", "\\bsum scores", "\\bcomposite", "\\bcomposites", "\\bindex\\b", "\\bindices\\b", "\\bscale score", "\\btotal score"),
  ordinal_likert_ratings = c("\\bordinal\\b", "\\blikert\\b", "\\brating\\b", "\\bratings\\b", "\\branked\\b", "\\brankings\\b"),
  response_times_durations = c("\\bresponse time", "\\bresponse times", "\\brt\\b", "\\blatency\\b", "\\blatencies\\b", "\\bduration\\b", "\\bdurations\\b", "\\btime\\b", "\\b1/rt\\b", "\\blog\\(rt\\)\\b", "\\blogrt\\b", "\\bzrt\\b", "\\btimes\\b"),
  counts_error_counts = c("\\bcount\\b", "\\bcounts\\b", "\\berror count", "\\berror counts", "\\bfrequency\\b", "\\bfrequencies\\b"),
  neural_physiological = c("\\bneural\\b", "\\bphysiological\\b", "\\beeg\\b", "\\bfmri\\b", "\\beri?p\\b", "\\bheart rate\\b", "\\bscr\\b", "\\bemg\\b", "\\bpupil", "\\bamplitude\\b", "\\blogamplitude\\b", "\\bhr\\b", "\\bblood pressure\\b"),
  correlations_associations = c("\\bcorrelation\\b", "\\bcorrelations\\b", "\\bassociation\\b", "\\bassociations\\b", "\\bcovariance\\b", "\\bcor\\b", "\\bzcor\\b"),
  difference_distance_scores = c("\\bdifference score", "\\bdifference scores", "\\bdistance\\b", "\\bdistances\\b", "\\bdiscrepancy\\b", "\\bdiscrepancies\\b", "\\bdifferences\\b", "\\bdifference-score\\b", "\\bangular\\b"))
outcome_table <- data.frame(row_id = names(outcome_patterns),
  outcome_type = c("Binary / accuracy / proportions", "Sum scores / composites", "Ordinal / Likert / ratings",
                   "Response times / durations", "Counts / error counts", "Neural / physiological measures",
                   "Correlations / associations", "Difference / distance scores"),
  n = NA, denominator_n = n_observed)
for (i in 1:nrow(outcome_table)) outcome_table$n[i] <- sum(grepl(paste(outcome_patterns[[i]], collapse = "|"), response_types, perl = TRUE))
outcome_table$percent <- pct(outcome_table$n, n_observed)
print(outcome_table)

####################################################
# By journal
####################################################

by_journal <- NULL
for (j in sort(unique(eligible_data$journal))) {
  el <- eligible_data[eligible_data$journal == j, ]
  cd <- coded[coded$journal == j, ]
  incorrect <- cd$Incorrect_identity_link_function == 1
  significant_incorrect <- incorrect & cd$Finds_significant_interaction == 1
  by_journal <- rbind(by_journal, data.frame(journal = j,
    eligible_n = nrow(el),
    interaction_testing_n = sum(el$Tests_interactions == 1),
    interaction_testing_percent_of_eligible = pct(sum(el$Tests_interactions == 1), nrow(el)),
    observed_outcome_interaction_n = nrow(cd),
    non_identity_link_n = sum(cd$Uses_non_identity_link_function == 1, na.rm = TRUE),
    non_identity_link_percent_interaction = pct(sum(cd$Uses_non_identity_link_function == 1, na.rm = TRUE), nrow(cd)),
    explicit_link_n = sum(cd$Explicit_link_function == 1, na.rm = TRUE),
    explicit_link_percent_interaction = pct(sum(cd$Explicit_link_function == 1, na.rm = TRUE), nrow(cd)),
    incorrect_identity_n = sum(incorrect, na.rm = TRUE),
    incorrect_identity_percent_interaction = pct(sum(incorrect, na.rm = TRUE), nrow(cd)),
    significant_incorrect_identity_n = sum(significant_incorrect, na.rm = TRUE),
    significant_incorrect_identity_percent_incorrect = pct(sum(significant_incorrect, na.rm = TRUE), sum(incorrect, na.rm = TRUE))))
}
print(by_journal[, 1:5])

####################################################
# Intercoder agreement (double-coded articles, coders CD1 and CD2)
####################################################

agreement <- NULL
labels <- c("Uses non-identity link function", "Explicit link function",
            "Gaussian-identity analyses on constrained observed outcomes", "Finds significant interaction")
for (i in 1:length(main_vars)) {
  x <- as01(coded[[paste0("CD1_", main_vars[i])]])
  y <- as01(coded[[paste0("CD2_", main_vars[i])]])
  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]
  n <- length(x)
  n_00 <- sum(x == 0 & y == 0)
  n_01 <- sum(x == 0 & y == 1)
  n_10 <- sum(x == 1 & y == 0)
  n_11 <- sum(x == 1 & y == 1)
  p0 <- (n_00 + n_11) / n # observed agreement
  pe <- mean(x == 1) * mean(y == 1) + (1 - mean(x == 1)) * (1 - mean(y == 1)) # chance agreement
  agreement <- rbind(agreement, data.frame(variable = main_vars[i], label = labels[i], n_double_coded = n,
    agreement = if (n > 0) p0 else NA,
    kappa = if (n > 0 && !isTRUE(all.equal(pe, 1))) (p0 - pe) / (1 - pe) else NA,
    coder1_yes_percent = if (n > 0) 100 * mean(x == 1) else NA, coder2_yes_percent = if (n > 0) 100 * mean(y == 1) else NA,
    coder1_no_percent = if (n > 0) 100 * mean(x == 0) else NA, coder2_no_percent = if (n > 0) 100 * mean(y == 0) else NA,
    n_disagreements = n_01 + n_10, n_00 = n_00, n_01 = n_01, n_10 = n_10, n_11 = n_11))
}
print(agreement[, c("variable", "n_double_coded", "agreement", "kappa")])

####################################################
# Screening flow
####################################################

screening_flow <- data.frame(
  step = c("records retrieved across journal sheets", "articles screened for eligibility", "eligible empirical articles",
           "eligible articles testing at least one interaction", "eligible articles not testing interactions",
           "interaction-testing articles with at least one observed-outcome interaction (coded)",
           "interaction-testing articles with interactions on latent outcomes only"),
  n = c(n_records, n_screened, n_eligible, n_interactions, n_non_interactions, n_observed, n_latent_only),
  basis = c("non-empty rows across all screening workbook sheets (one per journal)",
            "articles screened for eligibility across all five journals",
            "Eligible == 1 in screening workbook", "Tests_interactions == 1 in eligible rows",
            "eligible minus interaction-testing",
            "Tests_observed_outcome_interactions == 1 in interaction-testing rows",
            "Tests_observed_outcome_interactions == 0 in interaction-testing rows"))
print(screening_flow[, 1:2])

write.csv(review_summary, "tables/review-summary.csv", row.names = FALSE)
write.csv(outcome_table, "tables/review-outcome-types.csv", row.names = FALSE)
write.csv(by_journal, "tables/review-by-journal.csv", row.names = FALSE)
write.csv(agreement, "tables/review-intercoder-agreement.csv", row.names = FALSE)
write.csv(screening_flow, "tables/review-screening-flow.csv", row.names = FALSE)
saveRDS(list(review_summary = review_summary, outcome_table = outcome_table, by_journal = by_journal,
             agreement = agreement, screening_flow = screening_flow,
             data_file_md5 = unname(tools::md5sum("Literature_review/final-dataset-review.csv")), r_version = R.version.string),
        "outputs/review-summary.rds")
