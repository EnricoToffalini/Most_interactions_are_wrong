# Lightweight smoke test for the companion Shiny app.
# Parses app.R and exercises the helper functions. No Shiny session is
# started and no simulation is run. Run from the repository root:
#   Rscript shiny-app/smoke-test.R

app_dir <- if (file.exists("shiny-app/app.R")) "shiny-app" else "."

# 1. app.R must parse.
invisible(parse(file.path(app_dir, "app.R")))
cat("app.R parsed OK\n")

# 2. Helpers must load and behave.
source(file.path(app_dir, "R", "app_helpers.R"))

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
stopifnot(abs(dd$observed_scale) > 1e-6)       # nonzero on the observed scale

cells2 <- four_cell_values(0.2, 0.4, -0.3, 0.7, "identity")
dd2 <- diff_in_diff(cells2)
stopifnot(abs(dd2$link_scale - 0.7) < 1e-12)
stopifnot(abs(dd2$observed_scale - 0.7) < 1e-12)  # identity: same on both scales

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
cat("smoke test passed\n")
