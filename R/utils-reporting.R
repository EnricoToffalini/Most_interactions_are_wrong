# R/utils-reporting.R
# Console and output-directory helpers.

ensure_output_dirs <- function(dirs = NULL) {
  if (is.null(dirs)) {
    dirs <- get0(
      "project_output_dirs",
      ifnotfound = c("tables", "outputs", "outputs/inspection", "paper/figs")
    )
  }

  invisible(lapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
}

report_header <- function(x) {
  stopifnot(length(x) == 1)
  line <- paste(rep("=", nchar(x) + 4), collapse = "")
  cat("\n", line, "\n", sep = "")
  cat("  ", x, "\n", sep = "")
  cat(line, "\n", sep = "")
  invisible(x)
}

report_section <- function(x) {
  stopifnot(length(x) == 1)
  line <- paste(rep("-", nchar(x)), collapse = "")
  cat("\n", x, "\n", line, "\n", sep = "")
  invisible(x)
}

list_to_table <- function(x) {
  data.frame(
    setting = names(x),
    value = vapply(
      x,
      function(v) paste(v, collapse = ", "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

print_compact <- function(x, digits = 3) {
  old <- options(digits = digits)
  on.exit(options(old), add = TRUE)
  print(x, row.names = FALSE)
  invisible(x)
}

progress_tick <- function(i, n, label = "") {
  if (n <= 20 || i %% max(1, floor(n / 10)) == 0 || i == n) {
    cat(label, i, "/", n, "\n", sep = "")
  }
  invisible(i)
}

report_sign_convention <- function(low_label, high_label) {
  cat("\nSign convention: group gaps are Group 1 minus Group 0.\n")
  cat(
    "Change in group gap is gap at ", high_label,
    " minus gap at ", low_label, ".\n",
    sep = ""
  )
  invisible(NULL)
}
