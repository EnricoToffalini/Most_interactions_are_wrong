# R/simulation-settings.R
# Shared settings for the link-function simulation scripts.

# Defaults can be overridden from the shell, for example:
#   N_SIM=1000 Rscript scripts/03-simulation-forced-choice.R
default_B <- as.integer(Sys.getenv("N_SIM", "300"))
default_alpha <- 0.05
default_dpi <- 300
figure_width <- 7.2
figure_height <- 7.0
age_values_for_summary <- c(6, 8, 10)
x_values_for_summary <- c(-1, 0, 1)

ensure_output_dirs <- function() {
  dir.create("tables", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
  dir.create("outputs/inspection", showWarnings = FALSE, recursive = TRUE)
  dir.create("paper/figs", showWarnings = FALSE, recursive = TRUE)
}

report_header <- function(x) {
  cat("\n", paste(rep("=", nchar(x) + 4), collapse = ""), "\n", sep = "")
  cat("  ", x, "\n", sep = "")
  cat(paste(rep("=", nchar(x) + 4), collapse = ""), "\n", sep = "")
}

report_section <- function(x) {
  cat("\n", x, "\n", paste(rep("-", nchar(x)), collapse = ""), "\n", sep = "")
}

list_to_table <- function(x) {
  data.frame(
    setting = names(x),
    value = vapply(x, function(v) paste(v, collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  )
}

print_compact <- function(x, digits = 3) {
  old <- options(digits = digits)
  on.exit(options(old), add = TRUE)
  print(x, row.names = FALSE)
}

progress_tick <- function(i, n, label = "") {
  if (n <= 20 || i %% max(1, floor(n / 10)) == 0 || i == n) {
    cat(label, i, "/", n, "\n", sep = "")
  }
}

report_sign_convention <- function(low_label, high_label) {
  cat("\nSign convention: group gaps are Group 1 minus Group 0.\n")
  cat("Change in group gap is gap at ", high_label, " minus gap at ", low_label, ".\n", sep = "")
}
