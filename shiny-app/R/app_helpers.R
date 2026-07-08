# Helper functions for the link-function companion Shiny app.
# All computations here are deterministic and cheap: no simulation, no fitting.

# Links available anywhere in the app.
LINKS_ALL <- c(
  "identity", "logit", "probit", "cloglog", "log-log",
  "cauchit", "log", "inverse", "chance-corrected logit"
)

# Links offered in the four-cell interaction calculator.
LINKS_FOUR_CELL <- c(
  "identity", "logit", "probit", "cloglog", "log-log",
  "chance-corrected logit"
)

# Inverse link: map linear predictor eta to expected response mu.
# For "chance-corrected logit", mu = chance + (1 - chance) * plogis(eta),
# with chance required to be in [0, 0.95].
inv_link <- function(eta, link, chance = 0) {
  if (identical(link, "chance-corrected logit")) {
    if (!is.numeric(chance) || length(chance) != 1 ||
        is.na(chance) || chance < 0 || chance > 0.95) {
      stop("chance must be a single number in [0, 0.95]")
    }
  }
  switch(link,
    "identity" = eta,
    "logit"    = stats::plogis(eta),
    "probit"   = stats::pnorm(eta),
    "cloglog"  = 1 - exp(-exp(eta)),
    "log-log"  = exp(-exp(-eta)),
    "cauchit"  = stats::pcauchy(eta),
    "log"      = exp(eta),
    "inverse"  = 1 / eta,
    "chance-corrected logit" = chance + (1 - chance) * stats::plogis(eta),
    stop("Unknown link: ", link)
  )
}

# Forward link: map expected response mu to the linear predictor eta.
# Inverse of inv_link for the links used in the four-cell calculators.
# Values outside the link's domain (e.g. mu <= chance for the
# chance-corrected logit) yield NaN rather than an error.
link_fun <- function(mu, link, chance = 0) {
  if (identical(link, "chance-corrected logit")) {
    if (!is.numeric(chance) || length(chance) != 1 ||
        is.na(chance) || chance < 0 || chance > 0.95) {
      stop("chance must be a single number in [0, 0.95]")
    }
  }
  switch(link,
    "identity" = mu,
    "logit"    = stats::qlogis(mu),
    "probit"   = stats::qnorm(mu),
    "cloglog"  = log(-log(1 - mu)),
    "log-log"  = -log(-log(mu)),
    "chance-corrected logit" = stats::qlogis((mu - chance) / (1 - chance)),
    stop("Unknown link: ", link)
  )
}

# Coefficients implied by four cell means under a given link, from the
# saturated 2x2 parameterization eta = beta_0 + beta_x X + beta_z Z +
# beta_xz X Z:
#   beta_0  = g(mu_00)
#   beta_x  = g(mu_10) - g(mu_00)
#   beta_z  = g(mu_01) - g(mu_00)
#   beta_xz = g(mu_11) - g(mu_10) - g(mu_01) + g(mu_00)
four_cell_coefs <- function(mu00, mu10, mu01, mu11, link, chance = 0) {
  g <- link_fun(c(mu00, mu10, mu01, mu11), link, chance)
  c(beta_0  = g[1],
    beta_x  = g[2] - g[1],
    beta_z  = g[3] - g[1],
    beta_xz = g[4] - g[3] - g[2] + g[1])
}

# Clamp to [0, 1]. Used only where a probability-specific plot needs
# display limits; computations elsewhere are never silently clamped.
clamp_probability <- function(p) {
  pmin(pmax(p, 0), 1)
}

# Four cells of a 2x2 design: eta and mu for X in {0, 1}, Z in {0, 1}.
four_cell_values <- function(beta_0, beta_x, beta_z, beta_xz,
                             link, chance = 0) {
  X <- c(0, 1, 0, 1)
  Z <- c(0, 0, 1, 1)
  eta <- beta_0 + beta_x * X + beta_z * Z + beta_xz * X * Z
  mu <- inv_link(eta, link, chance)
  data.frame(X = X, Z = Z, eta = eta, mu = mu)
}

# Difference-in-differences from a four-cell data.frame:
# [cell(1,1) - cell(0,1)] - [cell(1,0) - cell(0,0)],
# computed on the link scale (eta) and the observed scale (mu).
# On the link scale this equals beta_xz exactly.
diff_in_diff <- function(values) {
  cell <- function(x, z, col) {
    values[[col]][values$X == x & values$Z == z][1]
  }
  did <- function(col) {
    (cell(1, 1, col) - cell(0, 1, col)) - (cell(1, 0, col) - cell(0, 0, col))
  }
  list(link_scale = did("eta"), observed_scale = did("mu"))
}
