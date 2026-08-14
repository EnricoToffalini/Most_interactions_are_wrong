# scripts/04-diagnostic-worked-example.R
# Diagnostic simulation: link-induced pseudo-interactions and model checks.
#
# Purpose:
#   This script asks whether diagnostics flag the same link problems that can
#   generate pseudo-interactions. The scenarios are tied to examples
#   already used in the paper:
#     1. Poisson count example: true log link, fitted identity link.
#     2. Forced-choice accuracy: true chance-corrected logit, fitted standard logit.
#     3. Binary repeated trials, 2 x 2 hard case: true probit GLMM,
#        fitted logit GLMM.
#     4. Binary repeated trials, matched continuous-predictor counterpart:
#        same coefficients and total trials as scenario 3, but condition is
#        replaced by a continuous predictor on [0, 1].
#     5. Gamma positive outcome: true log link, fitted inverse link.

rm(list = ls())

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------

source("R/project-settings.R")
source("R/utils-reporting.R")
source("R/utils-link-functions.R")
source("R/utils-summaries.R")
source("R/utils-plots.R")

ensure_output_dirs()

report_header("Diagnostic simulation")

# ---------------------------------------------------------------------
# 1. User-tunable settings and scenarios
# ---------------------------------------------------------------------

settings <- list(
  B = default_B,
  n_cores = max(
    1L,
    min(
      as.integer(Sys.getenv("N_CORES", "12")),
      max(1L, parallel::detectCores(logical = TRUE) - 1L)
    )
  ),
  seed = 20260608,
  alpha = default_alpha,
  dharma_n_sim = as.integer(Sys.getenv("DHARMA_N_SIM", "250")),
  min_unique_for_quantile = 8,
  age_plot_n = 200,
  output_scenario_table = "tables/scenario-table-diagnostic-worked-example.csv",
  output_long_summary = "tables/simulation-summary-diagnostic-worked-example.csv",
  output_scenario_summary = "tables/simulation-summary-diagnostic-scenarios.csv",
  output_scenario_summary_paper = "tables/diagnostic-scenarios.csv",
  output_diagnostic_calibration = "tables/diagnostic-calibration.csv",
  output_replications = "outputs/diagnostic-simulation-replications.csv",
  output_dharma_example = "outputs/inspection/dharma-diagnostic-example.pdf",
  output_rds = "outputs/diagnostic-worked-example.rds"
)

# Scenario order is the intended table order.
diag_scenarios <- list(
  count_poisson_log_identity = list(
    scenario = "Count errors",
    short_label = "Count ",
    paper_anchor = "Simple count example figure",
    outcome_family = "Poisson count",
    true_link_function = "log",
    fitted_link_function = "identity",
    true_model_label = "Poisson log",
    fitted_model_label = "Poisson identity",
    N = 180,
    age_range = c(6, 10),
    age_origin = 6,
    beta_intercept = 2.20,
    beta_age = -0.55,
    beta_group = 0.45,
    beta_age_group = 0.00,
    focal_dharma = "continuous_age",
    variability_summary = "Poisson sampling; variance equals mean."
  ),
  chance_floor = list(
    scenario = "Chance-floor accuracy",
    short_label = "Chance ",
    paper_anchor = "Simulation 1, lower-performance forced-choice scenario",
    outcome_family = "Binomial accuracy",
    true_link_function = "chance-corrected logit",
    fitted_link_function = "standard logit",
    true_model_label = "chance-corrected binomial logit",
    fitted_model_label = "standard binomial logit",
    N = 250,
    k_trials = 20,
    chance = 0.50,
    age_range = c(6, 10),
    age_center = 8,
    beta_intercept = -0.80,
    beta_age = 0.60,
    beta_group = -0.90,
    beta_age_group = 0.00,
    focal_dharma = "continuous_age",
    variability_summary = "Binomial sampling with 20 trials per participant."
  ),
  probit_dgp_logit_fit = list(
    scenario = "Binary repeated trials",
    short_label = "Binary ",
    paper_anchor = "Simulation 2, probit-generated logit-fitted case",
    outcome_family = "Binary repeated trials",
    true_link_function = "probit",
    fitted_link_function = "logit",
    true_model_label = "binomial probit GLMM",
    fitted_model_label = "binomial logit GLMM",
    n_subjects = 300,
    k_trials = 15,
    target_icc = 0.30,
    beta_intercept = 1.50,
    beta_group = -1.00,
    beta_condition = -1.00,
    beta_group_condition = 0.00,
    focal_dharma = "categorical_design",
    variability_summary = "Subject random intercept with latent ICC = .30 and 15 binary trials per cell."
  ),
  probit_continuous_logit_fit = list(
    scenario = "Binary continuous predictor",
    short_label = "Binary continuous ",
    paper_anchor = "Matched continuous-predictor counterpart to Simulation 2",
    outcome_family = "Binary repeated trials",
    true_link_function = "probit",
    fitted_link_function = "logit",
    true_model_label = "binomial probit GLMM",
    fitted_model_label = "binomial logit GLMM",
    n_subjects = 300,
    trials_per_subject = 30,
    target_icc = 0.30,
    x_range = c(0, 1),
    beta_intercept = 1.50,
    beta_group = -1.00,
    beta_x = -1.00,
    beta_group_x = 0.00,
    focal_dharma = "continuous_binary_x",
    variability_summary = "Matched to the 2 x 2 binary scenario: same fixed effects, same latent ICC = .30, and 30 binary trials per subject; condition is replaced by x ~ Uniform(0, 1)."
  ),
  gamma_log_inverse = list(
    scenario = "Gamma mean response time",
    short_label = "Gamma ",
    paper_anchor = "Gamma response-time example from the introduction (log versus inverse link)",
    outcome_family = "Gamma positive continuous",
    true_link_function = "log",
    fitted_link_function = "inverse",
    true_model_label = "Gamma log",
    fitted_model_label = "Gamma inverse",
    N = 220,
    shape = 8,
    x_range = c(-2, 2),
    beta_intercept = log(400),
    beta_x = 0.25,
    beta_group = 0.25,
    beta_x_group = 0.00,
    focal_dharma = "continuous_x",
    variability_summary = "Gamma sampling with shape = 8; mean around 400 ms at x = 0, group = 0."
  )
)

dir.create(dirname(settings$output_dharma_example), recursive = TRUE, showWarnings = FALSE)
set.seed(settings$seed)

report_section("Scenario parameters you can tune")
print_compact(list_to_table(settings))
cat("\nAIC is used as a same-formula comparison between the target interaction model and the misspecified interaction model.\n")
cat("DHARMa checks are scenario-aware: quantile checks for continuous predictors, categorical checks for categorical design cells.\n")
cat("DHARMa and Pregibon-style checks measure detection under the wrong link and provide baseline calibration rates under the correct link.\n")

# ---------------------------------------------------------------------
# 2. Small utilities
# ---------------------------------------------------------------------

chance_logit_inv_local <- function(eta, chance) {
  chance + (1 - chance) * stats::plogis(eta)
}

latent_residual_variance <- function(link) {
  if (link == "logit") return(pi^2 / 3)
  if (link == "probit") return(1)
  stop("Unknown binary link.", call. = FALSE)
}

random_intercept_sd <- function(target_icc, link) {
  sqrt(target_icc * latent_residual_variance(link) / (1 - target_icc))
}

wilson_ci_local <- function(x, n, conf = 0.95) {
  if (!is.finite(x) || !is.finite(n) || n <= 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom
  c(max(0, centre - half), min(1, centre + half))
}

summarise_p_values <- function(values, alpha = settings$alpha) {
  values <- values[!is.na(values)]
  n <- length(values)
  x <- sum(values < alpha)
  ci <- wilson_ci_local(x, n)
  data.frame(
    n_successful_fits = n,
    n_significant = x,
    rate = if (n > 0) x / n else NA_real_,
    ci_low = ci[1],
    ci_high = ci[2],
    stringsAsFactors = FALSE
  )
}

summarise_logical <- function(values) {
  values <- values[!is.na(values)]
  n <- length(values)
  x <- sum(values)
  ci <- wilson_ci_local(x, n)
  data.frame(
    n_successful_fits = n,
    n_significant = x,
    rate = if (n > 0) x / n else NA_real_,
    ci_low = ci[1],
    ci_high = ci[2],
    stringsAsFactors = FALSE
  )
}

safe_glm <- function(expr) {
  fit <- try(suppressWarnings(expr), silent = TRUE)
  if (inherits(fit, "try-error")) return(fit)
  if (!is.null(fit$converged) && !isTRUE(fit$converged)) {
    return(structure("glm did not converge", class = "try-error"))
  }
  fit
}

safe_glmmTMB <- function(expr) {
  fit <- try(suppressWarnings(expr), silent = TRUE)
  if (inherits(fit, "try-error")) return(fit)
  fit
}

safe_aic <- function(fit) {
  if (inherits(fit, "try-error") || is.null(fit)) return(NA_real_)
  if (is.list(fit) && !is.null(fit$aic)) return(unname(fit$aic))
  out <- try(stats::AIC(fit), silent = TRUE)
  if (inherits(out, "try-error") || length(out) != 1 || !is.finite(out)) return(NA_real_)
  unname(out)
}

extract_interaction_p <- function(fit) {
  if (inherits(fit, "try-error") || is.null(fit)) return(NA_real_)
  if (inherits(fit, "glmmTMB")) {
    sm <- try(summary(fit)$coefficients$cond, silent = TRUE)
  } else {
    sm <- try(stats::coef(summary(fit)), silent = TRUE)
  }
  if (inherits(sm, "try-error")) return(NA_real_)
  rn <- rownames(sm)
  term <- rn[grepl(":", rn)][1]
  if (is.na(term)) return(NA_real_)
  p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(p_col)) return(NA_real_)
  unname(sm[term, p_col])
}

extract_interaction_coef <- function(fit) {
  if (inherits(fit, "try-error") || is.null(fit)) return(NA_real_)
  if (inherits(fit, "glmmTMB")) {
    sm <- try(summary(fit)$coefficients$cond, silent = TRUE)
    if (inherits(sm, "try-error")) return(NA_real_)
    rn <- rownames(sm)
    term <- rn[grepl(":", rn)][1]
    if (is.na(term)) return(NA_real_)
    return(unname(sm[term, "Estimate"]))
  }
  cf <- try(stats::coef(fit), silent = TRUE)
  if (inherits(cf, "try-error")) return(NA_real_)
  term <- names(cf)[grepl(":", names(cf))][1]
  if (is.na(term)) return(NA_real_)
  unname(cf[term])
}

extract_p_value <- function(x) {
  if (inherits(x, "try-error")) return(NA_real_)
  if (!is.null(x$p.value) && length(x$p.value) == 1) return(unname(x$p.value))
  NA_real_
}

quiet_try <- function(expr) {
  try(suppressMessages(suppressWarnings(expr)), silent = TRUE)
}

safe_dharma_test <- function(expr) {
  extract_p_value(quiet_try(expr))
}

unique_finite_n <- function(x) {
  x <- x[is.finite(x)]
  length(unique(round(x, 10)))
}

can_use_quantile_check <- function(x) {
  unique_finite_n(x) >= settings$min_unique_for_quantile
}

safe_predict_numeric <- function(fit, type = "response") {
  out <- try(stats::predict(fit, type = type), silent = TRUE)
  if (inherits(out, "try-error")) return(rep(NA_real_, 0))
  as.numeric(out)
}

make_empty_dharma <- function() {
  data.frame(
    dharma_uniformity_p = NA_real_,
    dharma_dispersion_p = NA_real_,
    dharma_quantile_fitted_p = NA_real_,
    dharma_quantile_predictor_p = NA_real_,
    dharma_categorical_design_p = NA_real_,
    dharma_valid_tests = 0L,
    dharma_tests_used = "",
    stringsAsFactors = FALSE
  )
}

# Scenario-aware DHARMa checks. The important change is that quantile
# regression is not attempted for categorical predictors.
dharma_diagnostics <- function(name, fit, data, scn) {
  empty <- make_empty_dharma()
  if (inherits(fit, "try-error") || is.null(fit)) return(empty)
  
  sim <- quiet_try(
    DHARMa::simulateResiduals(
      fittedModel = fit,
      n = settings$dharma_n_sim,
      plot = FALSE,
      seed = NULL
    )
  )
  
  if (inherits(sim, "try-error")) return(empty)
  
  p_uniformity <- safe_dharma_test(DHARMa::testUniformity(sim, plot = FALSE))
  p_dispersion <- safe_dharma_test(DHARMa::testDispersion(sim, plot = FALSE))
  
  fitted_response <- safe_predict_numeric(fit, type = "response")
  p_quantile_fitted <- NA_real_
  if (length(fitted_response) == nrow(data) && can_use_quantile_check(fitted_response)) {
    p_quantile_fitted <- safe_dharma_test(DHARMa::testQuantiles(sim, plot = FALSE))
  }
  
  p_quantile_predictor <- NA_real_
  p_categorical_design <- NA_real_
  
  if (identical(scn$focal_dharma, "continuous_age")) {
    x <- if ("age_c" %in% names(data)) data$age_c else data$age_offset
    if (can_use_quantile_check(x)) {
      p_quantile_predictor <- safe_dharma_test(
        DHARMa::testQuantiles(sim, predictor = x, plot = FALSE)
      )
    }
  }
  
  if (identical(scn$focal_dharma, "continuous_x")) {
    if (can_use_quantile_check(data$x)) {
      p_quantile_predictor <- safe_dharma_test(
        DHARMa::testQuantiles(sim, predictor = data$x, plot = FALSE)
      )
    }
  }
  
  if (identical(scn$focal_dharma, "continuous_binary_x")) {
    if (can_use_quantile_check(data$x)) {
      p_quantile_predictor <- safe_dharma_test(
        DHARMa::testQuantiles(sim, predictor = data$x, plot = FALSE)
      )
    }
  }
  
  if (identical(scn$focal_dharma, "categorical_design")) {
    design_cell <- interaction(data$group, data$condition, drop = TRUE)
    res <- sim$scaledResiduals
    # DHARMa::testCategorical() returns no scalar $p.value, so it never flowed
    # through extract_p_value() and this check silently produced NA in every
    # replication. A Kruskal-Wallis test of the scaled residuals across design
    # cells is a scalar, version-independent check of cell-specific misfit and
    # passes cleanly through safe_dharma_test().
    if (nlevels(design_cell) >= 2 && length(res) == length(design_cell)) {
      p_categorical_design <- safe_dharma_test(
        stats::kruskal.test(res ~ design_cell)
      )
    }
  }
  
  pvals <- c(
    uniformity = p_uniformity,
    dispersion = p_dispersion,
    quantile_fitted = p_quantile_fitted,
    quantile_predictor = p_quantile_predictor,
    categorical_design = p_categorical_design
  )
  
  used <- names(pvals)[!is.na(pvals)]
  
  data.frame(
    dharma_uniformity_p = unname(p_uniformity),
    dharma_dispersion_p = unname(p_dispersion),
    dharma_quantile_fitted_p = unname(p_quantile_fitted),
    dharma_quantile_predictor_p = unname(p_quantile_predictor),
    dharma_categorical_design_p = unname(p_categorical_design),
    dharma_valid_tests = length(used),
    dharma_tests_used = paste(used, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

# Audit fields for fits produced by fit_chance_binomial_logit(). They are NA
# for glm and glmmTMB fits, which carry no gradient_max element.
custom_fit_numeric <- function(fit, field) {
  if (inherits(fit, "try-error") || !is.list(fit)) return(NA_real_)
  value <- fit[[field]]
  if (is.null(value) || length(value) != 1 || !is.numeric(value)) return(NA_real_)
  as.numeric(value)
}

custom_fit_converged <- function(fit) {
  if (inherits(fit, "try-error") || !is.list(fit)) return(NA)
  if (is.null(fit$gradient_max)) return(NA)
  isTRUE(fit$converged)
}

aic_winner <- function(aic_true, aic_fitted) {
  if (!is.finite(aic_true) || !is.finite(aic_fitted)) {
    return(data.frame(
      favors_true_link = NA,
      favors_fitted_link = NA,
      difference_fitted_minus_true = NA_real_
    ))
  }
  data.frame(
    favors_true_link = aic_true <= aic_fitted,
    favors_fitted_link = aic_fitted < aic_true,
    difference_fitted_minus_true = aic_fitted - aic_true
  )
}

# ---------------------------------------------------------------------
# 3. Model-fitting helpers
# ---------------------------------------------------------------------

fit_chance_binomial_logit <- function(
    data,
    include_interaction = TRUE,
    chance = 0.50,
    add_eta_sq = FALSE
) {
  rhs_text <- if (include_interaction) "age_c * group" else "age_c + group"
  if (add_eta_sq) rhs_text <- paste(rhs_text, "+ eta_hat_sq")
  rhs <- stats::as.formula(paste("~", rhs_text))
  X <- stats::model.matrix(rhs, data = data)
  y <- data$y
  k <- data$k
  
  neg_loglik <- function(par) {
    eta <- as.vector(X %*% par)
    p <- chance_logit_inv_local(eta, chance = chance)
    p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }
  
  # Analytic score. With s = logistic(eta) and p = chance + (1 - chance) * s,
  # dp/deta = (1 - chance) * s * (1 - s), and the binomial contribution in p is
  # (y - k * p) / (p * (1 - p)). Supplying the gradient keeps BFGS on the
  # likelihood surface instead of relying on finite differences, which vanish
  # once the fitted probabilities approach the chance floor.
  neg_score <- function(par) {
    eta <- as.vector(X %*% par)
    s <- stats::plogis(eta)
    p <- chance + (1 - chance) * s
    p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
    w <- ((y - k * p) / (p * (1 - p))) * (1 - chance) * s * (1 - s)
    -as.vector(crossprod(X, w))
  }
  
  start_form <- stats::as.formula(paste("cbind(y, k - y) ~", rhs_text))
  
  start_glm <- safe_glm(
    stats::glm(
      start_form,
      family = stats::binomial("logit"),
      data = data
    )
  )
  
  zero <- rep(0, ncol(X))
  names(zero) <- colnames(X)
  
  from_glm <- zero
  if (!inherits(start_glm, "try-error")) {
    cf <- stats::coef(start_glm)
    common <- intersect(names(cf), names(from_glm))
    from_glm[common] <- cf[common]
  }
  
  # Standard-logit coefficients live on a different scale from the
  # chance-corrected ones, so they are only one candidate start among several.
  # This one reads the observed proportions on the above-chance scale the model
  # actually works on.
  above <- (y / k - chance) / (1 - chance)
  above <- pmin(pmax(above, 0.02), 0.98)
  from_above <- zero
  lin <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
  if (!inherits(lin, "try-error") && all(is.finite(stats::coef(lin)))) {
    from_above <- stats::setNames(stats::coef(lin), colnames(X))
  }
  
  starts <- list(zero, from_glm, from_glm / 2, from_glm * 1.5, from_above)
  
  best <- NULL
  for (s in starts) {
    for (m in c("BFGS", "Nelder-Mead")) {
      opt <- try(
        stats::optim(
          par = s,
          fn = neg_loglik,
          gr = if (identical(m, "BFGS")) neg_score else NULL,
          method = m,
          control = list(maxit = 5000)
        ),
        silent = TRUE
      )
      if (inherits(opt, "try-error")) next
      if (!is.finite(opt$value) || any(!is.finite(opt$par))) next
      if (is.null(best) || opt$value < best$value) best <- opt
    }
  }
  
  if (is.null(best)) {
    return(list(converged = FALSE, aic = NA_real_, logLik = NA_real_))
  }
  
  polished <- try(
    stats::optim(
      par = best$par,
      fn = neg_loglik,
      gr = neg_score,
      method = "BFGS",
      control = list(maxit = 5000, reltol = 1e-14)
    ),
    silent = TRUE
  )
  if (!inherits(polished, "try-error") && is.finite(polished$value) &&
      polished$value <= best$value) {
    best <- polished
  }
  
  names(best$par) <- colnames(X)
  
  # optim()'s convergence code only reports that a stopping rule was met, and on
  # the flat region near the chance floor that rule is met far from the maximum.
  # The retained solution is therefore checked against the score and the
  # curvature, and the same Hessian supplies the variance-covariance matrix.
  gradient_max <- max(abs(neg_score(best$par)))
  H <- try(stats::optimHess(best$par, neg_loglik, neg_score), silent = TRUE)
  
  na_vcov <- matrix(
    NA_real_,
    nrow = length(best$par),
    ncol = length(best$par),
    dimnames = list(names(best$par), names(best$par))
  )
  
  if (inherits(H, "try-error")) {
    hessian_min_eigen <- NA_real_
    vcov_fit <- na_vcov
  } else {
    hessian_min_eigen <- min(eigen(H, symmetric = TRUE, only.values = TRUE)$values)
    vcov_fit <- try(solve(H), silent = TRUE)
    if (inherits(vcov_fit, "try-error")) {
      vcov_fit <- na_vcov
    } else {
      dimnames(vcov_fit) <- list(names(best$par), names(best$par))
    }
  }
  
  converged <- all(is.finite(best$par)) && is.finite(best$value) &&
    isTRUE(hessian_min_eigen > 1e-6)
  
  loglik <- -best$value
  eta <- as.vector(X %*% best$par)
  fitted_probability <- chance_logit_inv_local(eta, chance = chance)
  model_frame <- stats::model.frame(start_form, data = data)
  
  structure(list(
    converged = converged,
    coefficients = best$par,
    vcov = vcov_fit,
    logLik = loglik,
    aic = if (converged) -2 * loglik + 2 * length(best$par) else NA_real_,
    gradient_max = gradient_max,
    hessian_min_eigen = hessian_min_eigen,
    include_interaction = include_interaction,
    add_eta_sq = add_eta_sq,
    chance = chance,
    formula = start_form,
    data = data,
    model_frame = model_frame,
    model_matrix = X,
    linear_predictors = eta,
    fitted_probability = fitted_probability,
    observed = y,
    trials = k
  ), class = "chance_binomial_logit")
}
coef.chance_binomial_logit <- function(object, ...) {
  object$coefficients
}

vcov.chance_binomial_logit <- function(object, ...) {
  object$vcov
}

family.chance_binomial_logit <- function(object, ...) {
  stats::binomial(link = "logit")
}

nobs.chance_binomial_logit <- function(object, ...) {
  length(object$observed)
}

formula.chance_binomial_logit <- function(x, ...) {
  x$formula
}

model.frame.chance_binomial_logit <- function(formula, ...) {
  formula$model_frame
}

predict.chance_binomial_logit <- function(
    object,
    newdata = NULL,
    type = c("link", "response"),
    ...
) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    eta <- object$linear_predictors
  } else {
    X <- stats::model.matrix(stats::delete.response(stats::terms(object$formula)), newdata)
    eta <- as.vector(X %*% object$coefficients)
  }
  if (type == "link") return(as.numeric(eta))
  chance_logit_inv_local(eta, chance = object$chance)
}

simulate.chance_binomial_logit <- function(
    object,
    nsim = 1,
    seed = NULL,
    ...
) {
  if (!is.null(seed)) set.seed(seed)
  out <- replicate(
    nsim,
    stats::rbinom(
      n = length(object$observed),
      size = object$trials,
      prob = object$fitted_probability
    ),
    simplify = FALSE
  )
  names(out) <- paste0("sim_", seq_len(nsim))
  as.data.frame(out, optional = TRUE)
}

residuals.chance_binomial_logit <- function(
    object,
    type = c("response", "pearson"),
    ...
) {
  type <- match.arg(type)
  raw <- object$observed - object$trials * object$fitted_probability
  if (type == "response") return(raw)
  variance <- object$trials * object$fitted_probability * (1 - object$fitted_probability)
  raw / sqrt(variance)
}

fit_binomial_standard <- function(data, link, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "age_c * group" else "age_c + group"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("cbind(y, k - y) ~", rhs))
  safe_glm(
    stats::glm(
      form,
      family = stats::binomial(link = link),
      data = data
    )
  )
}

fit_poisson_log <- function(data, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "age_offset * group" else "age_offset + group"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs))
  safe_glm(stats::glm(form, family = stats::poisson(link = "log"), data = data))
}

fit_poisson_identity <- function(data, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "age_offset * group" else "age_offset + group"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs))
  
  start_fit <- try(stats::lm(form, data = data), silent = TRUE)
  start <- NULL
  if (!inherits(start_fit, "try-error")) {
    start <- stats::coef(start_fit)
    X <- stats::model.matrix(form, data = data)
    pred <- as.vector(X %*% start)
    if (any(!is.finite(pred))) start <- NULL
    if (!is.null(start) && min(pred, na.rm = TRUE) <= 0) {
      start[1] <- start[1] + abs(min(pred, na.rm = TRUE)) + 1
    }
  }
  
  if (is.null(start) || any(!is.finite(start))) {
    safe_glm(stats::glm(form, family = stats::poisson(link = "identity"), data = data))
  } else {
    safe_glm(stats::glm(form, family = stats::poisson(link = "identity"), data = data, start = start))
  }
}

fit_gamma_log <- function(data, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "x * group" else "x + group"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs))
  safe_glm(stats::glm(form, family = stats::Gamma(link = "log"), data = data))
}

fit_gamma_inverse <- function(data, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "x * group" else "x + group"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs))
  
  start_fit <- try(stats::lm(stats::as.formula(paste("I(1 / y) ~", rhs)), data = data), silent = TRUE)
  start <- NULL
  if (!inherits(start_fit, "try-error")) {
    start <- stats::coef(start_fit)
    X <- stats::model.matrix(form, data = data)
    pred <- as.vector(X %*% start)
    if (any(!is.finite(pred))) start <- NULL
    if (!is.null(start) && min(pred, na.rm = TRUE) <= 0) {
      start[1] <- start[1] + abs(min(pred, na.rm = TRUE)) + 1e-4
    }
  }
  
  if (is.null(start) || any(!is.finite(start))) {
    safe_glm(stats::glm(form, family = stats::Gamma(link = "inverse"), data = data))
  } else {
    safe_glm(stats::glm(form, family = stats::Gamma(link = "inverse"), data = data, start = start))
  }
}

fit_binary_mixed <- function(data, link, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "group * condition" else "group + condition"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs, "+ (1 | id)"))
  safe_glmmTMB(
    glmmTMB::glmmTMB(
      form,
      data = data,
      family = stats::binomial(link = link)
    )
  )
}

fit_binary_mixed_continuous <- function(data, link, include_interaction = TRUE, add_eta_sq = FALSE) {
  rhs <- if (include_interaction) "group * x" else "group + x"
  if (add_eta_sq) rhs <- paste(rhs, "+ eta_hat_sq")
  form <- stats::as.formula(paste("y ~", rhs, "+ (1 | id)"))
  safe_glmmTMB(
    glmmTMB::glmmTMB(
      form,
      data = data,
      family = stats::binomial(link = link)
    )
  )
}

pregibon_added_term_p <- function(fit, data) {
  if (inherits(fit, "try-error") || is.null(fit)) return(NA_real_)
  
  eta_hat <- try(stats::predict(fit, type = "link"), silent = TRUE)
  if (inherits(eta_hat, "try-error") || length(eta_hat) != nrow(data)) return(NA_real_)
  
  data$eta_hat_sq <- as.numeric(eta_hat)^2
  
  if (inherits(fit, "chance_binomial_logit")) {
    fit2 <- fit_chance_binomial_logit(
      data,
      include_interaction = fit$include_interaction,
      chance = fit$chance,
      add_eta_sq = TRUE
    )
  } else {
    augmented_formula <- stats::update.formula(
      stats::formula(fit),
      . ~ . + eta_hat_sq
    )
    fit_family <- stats::family(fit)
    if (inherits(fit, "glmmTMB")) {
      fit2 <- safe_glmmTMB(
        glmmTMB::glmmTMB(
          augmented_formula,
          data = data,
          family = fit_family
        )
      )
    } else {
      start <- c(stats::coef(fit), eta_hat_sq = 0)
      fit2 <- safe_glm(
        stats::glm(
          augmented_formula,
          data = data,
          family = fit_family,
          start = start
        )
      )
    }
  }
  
  if (inherits(fit2, "try-error") || is.null(fit2)) return(NA_real_)
  
  if (inherits(fit2, "chance_binomial_logit")) {
    if (!isTRUE(fit2$converged) || !"eta_hat_sq" %in% names(fit2$coefficients)) {
      return(NA_real_)
    }
    variance <- fit2$vcov["eta_hat_sq", "eta_hat_sq"]
    if (!is.finite(variance) || variance <= 0) return(NA_real_)
    z <- fit2$coefficients["eta_hat_sq"] / sqrt(variance)
    return(unname(2 * stats::pnorm(-abs(z))))
  } else if (inherits(fit2, "glmmTMB")) {
    sm <- try(summary(fit2)$coefficients$cond, silent = TRUE)
  } else {
    sm <- try(stats::coef(summary(fit2)), silent = TRUE)
  }
  
  if (inherits(sm, "try-error")) return(NA_real_)
  if (!"eta_hat_sq" %in% rownames(sm)) return(NA_real_)
  p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(p_col)) return(NA_real_)
  unname(sm["eta_hat_sq", p_col])
}

# ---------------------------------------------------------------------
# 4. Scenario-specific simulators and model dispatch
# ---------------------------------------------------------------------

simulate_count_poisson <- function(scn) {
  group_num <- stats::rbinom(scn$N, 1, 0.5)
  age <- stats::runif(scn$N, scn$age_range[1], scn$age_range[2])
  age_offset <- age - scn$age_origin
  eta <- scn$beta_intercept + scn$beta_age * age_offset +
    scn$beta_group * group_num + scn$beta_age_group * age_offset * group_num
  mu <- exp(eta)
  y <- stats::rpois(scn$N, lambda = mu)
  data.frame(
    age = age,
    age_offset = age_offset,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    eta_true = eta,
    mu_true = mu,
    y = y,
    stringsAsFactors = FALSE
  )
}

simulate_chance_floor <- function(scn) {
  group_num <- stats::rbinom(scn$N, 1, 0.5)
  age <- stats::runif(scn$N, scn$age_range[1], scn$age_range[2])
  age_c <- age - scn$age_center
  eta <- scn$beta_intercept + scn$beta_age * age_c +
    scn$beta_group * group_num + scn$beta_age_group * age_c * group_num
  p <- chance_logit_inv_local(eta, chance = scn$chance)
  y <- stats::rbinom(scn$N, size = scn$k_trials, prob = p)
  data.frame(
    age = age,
    age_c = age_c,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    eta_true = eta,
    p_true = p,
    y = y,
    k = scn$k_trials,
    accuracy = y / scn$k_trials,
    stringsAsFactors = FALSE
  )
}

simulate_probit_mixed <- function(scn) {
  n_per_group <- scn$n_subjects / 2
  id <- rep(seq_len(scn$n_subjects), each = 2 * scn$k_trials)
  group_by_subject <- rep(c(0, 1), each = n_per_group)
  d <- data.frame(
    id = factor(id),
    group_num = rep(group_by_subject, each = 2 * scn$k_trials),
    condition_num = rep(rep(c(0, 1), each = scn$k_trials), times = scn$n_subjects),
    stringsAsFactors = FALSE
  )
  u <- stats::rnorm(
    scn$n_subjects,
    mean = 0,
    sd = random_intercept_sd(scn$target_icc, link = "probit")
  )
  eta <- scn$beta_intercept + scn$beta_group * d$group_num +
    scn$beta_condition * d$condition_num +
    scn$beta_group_condition * d$group_num * d$condition_num +
    u[as.integer(d$id)]
  p <- stats::pnorm(eta)
  d$y <- stats::rbinom(nrow(d), size = 1, prob = p)
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
  d$eta_true <- eta
  d$p_true <- p
  d
}

simulate_probit_continuous_mixed <- function(scn) {
  n_per_group <- scn$n_subjects / 2
  id <- rep(seq_len(scn$n_subjects), each = scn$trials_per_subject)
  group_by_subject <- rep(c(0, 1), each = n_per_group)
  d <- data.frame(
    id = factor(id),
    group_num = rep(group_by_subject, each = scn$trials_per_subject),
    x = stats::runif(scn$n_subjects * scn$trials_per_subject, scn$x_range[1], scn$x_range[2]),
    stringsAsFactors = FALSE
  )
  u <- stats::rnorm(
    scn$n_subjects,
    mean = 0,
    sd = random_intercept_sd(scn$target_icc, link = "probit")
  )
  eta <- scn$beta_intercept + scn$beta_group * d$group_num +
    scn$beta_x * d$x + scn$beta_group_x * d$group_num * d$x +
    u[as.integer(d$id)]
  p <- stats::pnorm(eta)
  d$y <- stats::rbinom(nrow(d), size = 1, prob = p)
  d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
  d$eta_true <- eta
  d$p_true <- p
  d
}

simulate_gamma_log <- function(scn) {
  group_num <- stats::rbinom(scn$N, 1, 0.5)
  x <- stats::runif(scn$N, scn$x_range[1], scn$x_range[2])
  eta <- scn$beta_intercept + scn$beta_x * x +
    scn$beta_group * group_num + scn$beta_x_group * x * group_num
  mu <- exp(eta)
  y <- stats::rgamma(scn$N, shape = scn$shape, scale = mu / scn$shape)
  data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    eta_true = eta,
    mu_true = mu,
    y = y,
    stringsAsFactors = FALSE
  )
}

simulate_scenario <- function(name, scn) {
  switch(
    name,
    count_poisson_log_identity = simulate_count_poisson(scn),
    chance_floor = simulate_chance_floor(scn),
    probit_dgp_logit_fit = simulate_probit_mixed(scn),
    probit_continuous_logit_fit = simulate_probit_continuous_mixed(scn),
    gamma_log_inverse = simulate_gamma_log(scn),
    stop("Unknown scenario: ", name, call. = FALSE)
  )
}

fit_true_link_model <- function(name, data, scn, include_interaction = TRUE) {
  switch(
    name,
    count_poisson_log_identity = fit_poisson_log(data, include_interaction = include_interaction),
    chance_floor = fit_chance_binomial_logit(data, include_interaction = include_interaction, chance = scn$chance),
    probit_dgp_logit_fit = fit_binary_mixed(data, link = "probit", include_interaction = include_interaction),
    probit_continuous_logit_fit = fit_binary_mixed_continuous(data, link = "probit", include_interaction = include_interaction),
    gamma_log_inverse = fit_gamma_log(data, include_interaction = include_interaction),
    stop("Unknown scenario: ", name, call. = FALSE)
  )
}

fit_wrong_link_model <- function(name, data, scn, include_interaction = TRUE) {
  switch(
    name,
    count_poisson_log_identity = fit_poisson_identity(data, include_interaction = include_interaction),
    chance_floor = fit_binomial_standard(data, link = "logit", include_interaction = include_interaction),
    probit_dgp_logit_fit = fit_binary_mixed(data, link = "logit", include_interaction = include_interaction),
    probit_continuous_logit_fit = fit_binary_mixed_continuous(data, link = "logit", include_interaction = include_interaction),
    gamma_log_inverse = fit_gamma_inverse(data, include_interaction = include_interaction),
    stop("Unknown scenario: ", name, call. = FALSE)
  )
}

# ---------------------------------------------------------------------
# 5. Deterministic scenario tables
# ---------------------------------------------------------------------

scenario_description <- function(name, scn) {
  if (name == "count_poisson_log_identity") {
    return(paste0(
      "Simple count example figure coefficients: log(E[y]) = ", scn$beta_intercept,
      " + ", scn$beta_age, " * (age - ", scn$age_origin, ") + ",
      scn$beta_group, " * group; no age-by-group product term."
    ))
  }
  if (name == "chance_floor") {
    return(paste0(
      "Simulation 1 lower-performance coefficients: eta = ", scn$beta_intercept,
      " + ", scn$beta_age, " * (age - ", scn$age_center, ") + ",
      scn$beta_group, " * group; p = ", scn$chance,
      " + (1 - ", scn$chance, ") * logistic(eta); no age-by-group product term."
    ))
  }
  if (name == "probit_dgp_logit_fit") {
    return(paste0(
      "Simulation 2 probit reference coefficients: eta = ", scn$beta_intercept,
      " + ", scn$beta_group, " * group + ", scn$beta_condition,
      " * condition + subject random intercept; latent ICC = ", scn$target_icc,
      "; no group-by-condition product term."
    ))
  }
  if (name == "probit_continuous_logit_fit") {
    return(paste0(
      "Matched continuous counterpart to the 2 x 2 binary scenario: eta = ",
      scn$beta_intercept, " + ", scn$beta_group, " * group + ",
      scn$beta_x, " * x + subject random intercept; x ~ Uniform(",
      scn$x_range[1], ", ", scn$x_range[2], "). Thus the x = 0 to x = 1 contrast has the same probit-scale size as the condition 0 to 1 contrast; latent ICC = ", scn$target_icc, "; no group-by-x product term."
    ))
  }
  if (name == "gamma_log_inverse") {
    return(paste0(
      "Gamma response-time example, log-link scale: log(E[y]) = log(400) + ", scn$beta_x,
      " * x + ", scn$beta_group,
      " * group; Gamma shape = ", scn$shape,
      "; no x-by-group product term."
    ))
  }
  NA_character_
}

make_scenario_values <- function(name, scn) {
  if (name == "count_poisson_log_identity") {
    g <- expand.grid(age = c(6, 8, 10), group_num = c(0, 1))
    g$age_offset <- g$age - scn$age_origin
    eta <- scn$beta_intercept + scn$beta_age * g$age_offset + scn$beta_group * g$group_num
    expected <- exp(eta)
    predictor_name <- "age"
    predictor_value <- g$age
    outcome_label <- "expected_count"
  } else if (name == "chance_floor") {
    g <- expand.grid(age = c(6, 8, 10), group_num = c(0, 1))
    g$age_c <- g$age - scn$age_center
    eta <- scn$beta_intercept + scn$beta_age * g$age_c + scn$beta_group * g$group_num
    expected <- chance_logit_inv_local(eta, scn$chance)
    predictor_name <- "age"
    predictor_value <- g$age
    outcome_label <- "expected_accuracy"
  } else if (name == "probit_dgp_logit_fit") {
    g <- expand.grid(condition_num = c(0, 1), group_num = c(0, 1))
    eta <- scn$beta_intercept + scn$beta_group * g$group_num + scn$beta_condition * g$condition_num
    expected <- stats::pnorm(eta)
    predictor_name <- "condition"
    predictor_value <- g$condition_num
    outcome_label <- "expected_probability_at_random_intercept_0"
  } else if (name == "probit_continuous_logit_fit") {
    # Low, mid, high on the predictor's own range. Hard-coding 0 as the middle
    # value collapsed onto the lower bound whenever x_range starts at 0.
    g <- expand.grid(
      x = c(scn$x_range[1], mean(scn$x_range), scn$x_range[2]),
      group_num = c(0, 1)
    )
    eta <- scn$beta_intercept + scn$beta_group * g$group_num + scn$beta_x * g$x
    expected <- stats::pnorm(eta)
    predictor_name <- "x"
    predictor_value <- g$x
    outcome_label <- "expected_probability_at_random_intercept_0"
  } else if (name == "gamma_log_inverse") {
    g <- expand.grid(x = c(-2, 0, 2), group_num = c(0, 1))
    eta <- scn$beta_intercept + scn$beta_x * g$x + scn$beta_group * g$group_num
    expected <- exp(eta)
    predictor_name <- "x"
    predictor_value <- g$x
    outcome_label <- "expected_mean"
  } else {
    stop("Unknown scenario: ", name, call. = FALSE)
  }
  
  data.frame(
    scenario = scn$scenario,
    paper_anchor = scn$paper_anchor,
    outcome_family = scn$outcome_family,
    true_link_function = scn$true_link_function,
    fitted_link_function = scn$fitted_link_function,
    true_model_label = scn$true_model_label,
    fitted_model_label = scn$fitted_model_label,
    predictor_name = predictor_name,
    predictor_value = predictor_value,
    group = factor(g$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    linear_predictor = eta,
    expected_value = expected,
    expected_value_label = outcome_label,
    quantitative_description = scenario_description(name, scn),
    variability_summary = scn$variability_summary,
    stringsAsFactors = FALSE
  )
}

scenario_table <- do.call(
  rbind,
  Map(make_scenario_values, names(diag_scenarios), diag_scenarios)
)

utils::write.csv(scenario_table, settings$output_scenario_table, row.names = FALSE)

report_section("Implied scenario values")
print_compact(scenario_table)

# ---------------------------------------------------------------------
# 6. Repeated simulation
# ---------------------------------------------------------------------

run_replication <- function(name, scn, rep_id) {
  d <- simulate_scenario(name, scn)
  
  fit_wrong_interaction <- fit_wrong_link_model(name, d, scn, include_interaction = TRUE)
  fit_true_interaction <- fit_true_link_model(name, d, scn, include_interaction = TRUE)
  
  dh_wrong <- dharma_diagnostics(name, fit_wrong_interaction, d, scn)
  dh_correct <- dharma_diagnostics(name, fit_true_interaction, d, scn)
  preg_wrong <- pregibon_added_term_p(fit_wrong_interaction, d)
  preg_correct <- pregibon_added_term_p(fit_true_interaction, d)
  
  aic_result <- aic_winner(
    aic_true = safe_aic(fit_true_interaction),
    aic_fitted = safe_aic(fit_wrong_interaction)
  )
  
  data.frame(
    scenario = scn$scenario,
    true_link_function = scn$true_link_function,
    fitted_link_function = scn$fitted_link_function,
    interaction_p = extract_interaction_p(fit_wrong_interaction),
    interaction_coef = extract_interaction_coef(fit_wrong_interaction),
    dharma_uniformity_p = dh_wrong$dharma_uniformity_p,
    dharma_dispersion_p = dh_wrong$dharma_dispersion_p,
    dharma_quantile_fitted_p = dh_wrong$dharma_quantile_fitted_p,
    dharma_quantile_predictor_p = dh_wrong$dharma_quantile_predictor_p,
    dharma_categorical_design_p = dh_wrong$dharma_categorical_design_p,
    dharma_valid_tests = dh_wrong$dharma_valid_tests,
    dharma_tests_used = dh_wrong$dharma_tests_used,
    pregibon_link_test_p = preg_wrong,
    dharma_wrong_uniformity_p = dh_wrong$dharma_uniformity_p,
    dharma_wrong_dispersion_p = dh_wrong$dharma_dispersion_p,
    dharma_wrong_quantile_fitted_p = dh_wrong$dharma_quantile_fitted_p,
    dharma_wrong_quantile_predictor_p = dh_wrong$dharma_quantile_predictor_p,
    dharma_wrong_categorical_design_p = dh_wrong$dharma_categorical_design_p,
    dharma_wrong_valid_tests = dh_wrong$dharma_valid_tests,
    dharma_wrong_tests_used = dh_wrong$dharma_tests_used,
    dharma_correct_uniformity_p = dh_correct$dharma_uniformity_p,
    dharma_correct_dispersion_p = dh_correct$dharma_dispersion_p,
    dharma_correct_quantile_fitted_p = dh_correct$dharma_quantile_fitted_p,
    dharma_correct_quantile_predictor_p = dh_correct$dharma_quantile_predictor_p,
    dharma_correct_categorical_design_p = dh_correct$dharma_categorical_design_p,
    dharma_correct_valid_tests = dh_correct$dharma_valid_tests,
    dharma_correct_tests_used = dh_correct$dharma_tests_used,
    pregibon_wrong_p = preg_wrong,
    pregibon_correct_p = preg_correct,
    aic_true_link_interaction = safe_aic(fit_true_interaction),
    aic_fitted_link_interaction = safe_aic(fit_wrong_interaction),
    aic_favors_true_link = aic_result$favors_true_link,
    aic_favors_fitted_link = aic_result$favors_fitted_link,
    aic_fitted_minus_true = aic_result$difference_fitted_minus_true,
    target_fit_converged = custom_fit_converged(fit_true_interaction),
    target_fit_gradient_max = custom_fit_numeric(fit_true_interaction, "gradient_max"),
    target_fit_hessian_min_eigen = custom_fit_numeric(fit_true_interaction, "hessian_min_eigen"),
    stringsAsFactors = FALSE
  )
}

make_cluster <- function() {
  if (settings$n_cores <= 1L) return(NULL)
  cl <- parallel::makeCluster(settings$n_cores)
  parallel::clusterSetRNGStream(cl, iseed = settings$seed)
  parallel::clusterEvalQ(cl, {
    library(DHARMa)
    library(glmmTMB)
    NULL
  })
  parallel::clusterExport(
    cl,
    varlist = setdiff(ls(envir = .GlobalEnv), "cl"),
    envir = .GlobalEnv
  )
  cl
}

run_scenario <- function(name, scn, scenario_index, n_scenarios, cl = NULL) {
  started <- proc.time()[["elapsed"]]
  cat(
    "Starting scenario ", scenario_index, "/", n_scenarios, ": ",
    scn$scenario, "\n",
    sep = ""
  )
  
  if (is.null(cl)) {
    out <- lapply(seq_len(settings$B), function(b) {
      run_replication(name, scn, b)
    })
  } else {
    parallel::clusterExport(cl, varlist = c("name", "scn"), envir = environment())
    out <- parallel::parLapplyLB(cl, seq_len(settings$B), function(b) {
      run_replication(name, scn, b)
    })
  }
  
  elapsed_minutes <- (proc.time()[["elapsed"]] - started) / 60
  cat(
    "Completed scenario ", scenario_index, "/", n_scenarios, " in ",
    formatC(elapsed_minutes, format = "f", digits = 1), " minutes\n",
    sep = ""
  )
  do.call(rbind, out)
}

cl <- make_cluster()
on.exit(if (!is.null(cl)) parallel::stopCluster(cl), add = TRUE)

cat(
  "\nStarting diagnostic simulation with ",
  if (is.null(cl)) 1L else settings$n_cores,
  if (is.null(cl) || settings$n_cores == 1L) " worker\n" else " workers\n",
  sep = ""
)

scenario_names <- names(diag_scenarios)
simulation_results <- do.call(
  rbind,
  Map(
    function(name, scn, scenario_index) {
      run_scenario(
        name,
        scn,
        scenario_index = scenario_index,
        n_scenarios = length(diag_scenarios),
        cl = cl
      )
    },
    scenario_names,
    diag_scenarios,
    seq_along(diag_scenarios)
  )
)

# ---------------------------------------------------------------------
# 7. Summaries
# ---------------------------------------------------------------------

make_long_summary_one <- function(name, dat, scn) {
  summaries <- list(
    interaction = summarise_p_values(dat$interaction_p),
    dharma_uniformity = summarise_p_values(dat$dharma_uniformity_p),
    dharma_dispersion = summarise_p_values(dat$dharma_dispersion_p),
    dharma_quantile_fitted = summarise_p_values(dat$dharma_quantile_fitted_p),
    dharma_quantile_predictor = summarise_p_values(dat$dharma_quantile_predictor_p),
    dharma_categorical_design = summarise_p_values(dat$dharma_categorical_design_p),
    pregibon = summarise_p_values(dat$pregibon_link_test_p),
    aic_true = summarise_logical(dat$aic_favors_true_link),
    aic_fitted = summarise_logical(dat$aic_favors_fitted_link)
  )
  
  keys <- names(summaries)
  quantities <- c(
    interaction = "Pseudo-interaction detection",
    dharma_uniformity = "DHARMa uniformity",
    dharma_dispersion = "DHARMa dispersion",
    dharma_quantile_fitted = "DHARMa residual quantiles over fitted values",
    dharma_quantile_predictor = "DHARMa residual quantiles over focal predictor",
    dharma_categorical_design = "DHARMa residual distribution across design cells",
    pregibon = "Pregibon-style added-term link check, secondary",
    aic_true = "AIC same interaction formula favors true link",
    aic_fitted = "AIC same interaction formula favors fitted link"
  )
  families <- c(
    interaction = "Interaction test",
    dharma_uniformity = "DHARMa",
    dharma_dispersion = "DHARMa",
    dharma_quantile_fitted = "DHARMa",
    dharma_quantile_predictor = "DHARMa",
    dharma_categorical_design = "DHARMa",
    pregibon = "Pregibon-style link check",
    aic_true = "AIC link comparison",
    aic_fitted = "AIC link comparison"
  )
  
  do.call(
    rbind,
    lapply(keys, function(key) {
      cbind(
        data.frame(
          scenario = scn$scenario,
          paper_anchor = scn$paper_anchor,
          outcome_family = scn$outcome_family,
          true_link_function = scn$true_link_function,
          fitted_link_function = scn$fitted_link_function,
          true_model_label = scn$true_model_label,
          fitted_model_label = scn$fitted_model_label,
          quantitative_description = scenario_description(name, scn),
          variability_summary = scn$variability_summary,
          quantity = unname(quantities[key]),
          diagnostic_family = unname(families[key]),
          stringsAsFactors = FALSE
        ),
        summaries[[key]]
      )
    })
  )
}

long_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
    scn <- diag_scenarios[[name]]
    dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
    make_long_summary_one(name, dat, scn)
  })
)

valid_dharma_rate_names <- c(
  "dharma_uniformity_p",
  "dharma_dispersion_p",
  "dharma_quantile_fitted_p",
  "dharma_quantile_predictor_p",
  "dharma_categorical_design_p"
)

calibration_diagnostics <- data.frame(
  diagnostic = c(
    "DHARMa uniformity",
    "DHARMa dispersion",
    "DHARMa residual quantiles over fitted values",
    "DHARMa residual quantiles over focal predictor",
    "DHARMa residual distribution across design cells",
    "Pregibon-style added-term link check"
  ),
  suffix = c(
    "uniformity_p",
    "dispersion_p",
    "quantile_fitted_p",
    "quantile_predictor_p",
    "categorical_design_p",
    NA_character_
  ),
  stringsAsFactors = FALSE
)

make_calibration_summary_one <- function(dat, scn) {
  applicable <- rep(TRUE, nrow(calibration_diagnostics))
  if (identical(scn$focal_dharma, "categorical_design")) {
    applicable[calibration_diagnostics$suffix == "quantile_predictor_p"] <- FALSE
  } else {
    applicable[calibration_diagnostics$suffix == "categorical_design_p"] <- FALSE
  }
  diagnostics <- calibration_diagnostics[applicable, , drop = FALSE]
  
  do.call(
    rbind,
    lapply(c("correct", "wrong"), function(specification) {
      do.call(
        rbind,
        lapply(seq_len(nrow(diagnostics)), function(i) {
          suffix <- diagnostics$suffix[i]
          column <- if (is.na(suffix)) {
            paste0("pregibon_", specification, "_p")
          } else {
            paste0("dharma_", specification, "_", suffix)
          }
          summary <- summarise_p_values(dat[[column]])
          data.frame(
            scenario = scn$scenario,
            diagnostic = diagnostics$diagnostic[i],
            model_specification = if (specification == "correct") {
              "Correct link"
            } else {
              "Wrong link"
            },
            n_attempted = nrow(dat),
            n_successful = summary$n_successful_fits,
            flagging_rate = summary$rate,
            ci_low = summary$ci_low,
            ci_high = summary$ci_high,
            stringsAsFactors = FALSE
          )
        })
      )
    })
  )
}

calibration_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
    scn <- diag_scenarios[[name]]
    dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
    make_calibration_summary_one(dat, scn)
  })
)

make_scenario_summary_one <- function(name, dat, scn) {
  interaction <- summarise_p_values(dat$interaction_p)
  pregibon <- summarise_p_values(dat$pregibon_link_test_p)
  aic_true <- summarise_logical(dat$aic_favors_true_link)
  aic_fitted <- summarise_logical(dat$aic_favors_fitted_link)
  
  dharma_summaries <- lapply(valid_dharma_rate_names, function(col) summarise_p_values(dat[[col]]))
  names(dharma_summaries) <- valid_dharma_rate_names
  dharma_rates <- vapply(dharma_summaries, function(x) x$rate, numeric(1))
  dharma_ns <- vapply(dharma_summaries, function(x) x$n_successful_fits, integer(1))
  valid <- dharma_ns > 0 & !is.na(dharma_rates)
  
  if (any(valid)) {
    dharma_min <- min(dharma_rates[valid], na.rm = TRUE)
    dharma_max <- max(dharma_rates[valid], na.rm = TRUE)
    dharma_used_names <- names(dharma_rates)[valid]
  } else {
    dharma_min <- NA_real_
    dharma_max <- NA_real_
    dharma_used_names <- character(0)
  }
  
  data.frame(
    scenario = scn$scenario,
    paper_anchor = scn$paper_anchor,
    outcome_family = scn$outcome_family,
    true_link_function = scn$true_link_function,
    fitted_link_function = scn$fitted_link_function,
    true_model_label = scn$true_model_label,
    fitted_model_label = scn$fitted_model_label,
    quantitative_description = scenario_description(name, scn),
    variability_summary = scn$variability_summary,
    n_replications = interaction$n_successful_fits,
    pseudo_interaction_detection_rate = interaction$rate,
    pseudo_interaction_detection_ci_low = interaction$ci_low,
    pseudo_interaction_detection_ci_high = interaction$ci_high,
    dharma_detection_rate_min = dharma_min,
    dharma_detection_rate_max = dharma_max,
    dharma_checks_used = paste(dharma_used_names, collapse = "; "),
    dharma_n_checks_used = length(dharma_used_names),
    pregibon_detection_rate = pregibon$rate,
    pregibon_detection_ci_low = pregibon$ci_low,
    pregibon_detection_ci_high = pregibon$ci_high,
    aic_favors_target = aic_true$rate,
    aic_favors_true_link = aic_true$rate,
    aic_favors_fitted_link = aic_fitted$rate,
    n_aic_pairs = sum(is.finite(dat$aic_fitted_minus_true)),
    median_aic_fitted_minus_true = stats::median(dat$aic_fitted_minus_true, na.rm = TRUE),
    q10_aic_fitted_minus_true = unname(stats::quantile(dat$aic_fitted_minus_true, 0.10, na.rm = TRUE)),
    q25_aic_fitted_minus_true = unname(stats::quantile(dat$aic_fitted_minus_true, 0.25, na.rm = TRUE)),
    q75_aic_fitted_minus_true = unname(stats::quantile(dat$aic_fitted_minus_true, 0.75, na.rm = TRUE)),
    q90_aic_fitted_minus_true = unname(stats::quantile(dat$aic_fitted_minus_true, 0.90, na.rm = TRUE)),
    aic_difference_within_2 = mean(abs(dat$aic_fitted_minus_true) < 2, na.rm = TRUE),
    target_fit_converged_rate = mean(dat$target_fit_converged, na.rm = TRUE),
    target_fit_gradient_max = if (any(is.finite(dat$target_fit_gradient_max))) max(dat$target_fit_gradient_max, na.rm = TRUE) else NA_real_,
    # Compatibility aliases. The manuscript reads these column names, so both
    # spellings are written until paper.qmd is updated to the new ones.
    false_positive_interaction = interaction$rate,
    false_positive_interaction_ci_low = interaction$ci_low,
    false_positive_interaction_ci_high = interaction$ci_high,
    dharma_detection_min = dharma_min,
    dharma_detection_max = dharma_max,
    pregibon_detection = pregibon$rate,
    stringsAsFactors = FALSE
  )
}

scenario_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
    scn <- diag_scenarios[[name]]
    dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
    make_scenario_summary_one(name, dat, scn)
  })
)

utils::write.csv(simulation_results, settings$output_replications, row.names = FALSE)
utils::write.csv(long_summary, settings$output_long_summary, row.names = FALSE)
utils::write.csv(scenario_summary, settings$output_scenario_summary, row.names = FALSE)
utils::write.csv(scenario_summary, settings$output_scenario_summary_paper, row.names = FALSE)
utils::write.csv(calibration_summary, settings$output_diagnostic_calibration, row.names = FALSE)

report_section("Long diagnostic summary")
print_compact(long_summary)
report_section("Scenario-level diagnostic summary")
print_compact(scenario_summary)
report_section("Diagnostic calibration summary")
print_compact(calibration_summary)

cat("\nInterpretation aid:\n")
cat("- The pseudo-interaction detection rate uses the fitted wrong-link interaction model.\n")
cat("- DHARMa and Pregibon checks on the wrong-link interaction model measure detection of misspecification.\n")
cat("- The same checks on the correct-link interaction model provide baseline calibration rates.\n")
cat("- DHARMa quantile checks are used only when the predictor has enough unique values.\n")
cat("- For the 2 x 2 binary repeated-trials scenario, DHARMa uses a categorical design-cell check instead.\n")
cat("- The continuous binary scenario matches the 2 x 2 binary scenario in coefficients, ICC, and trials, but replaces condition with x in [0, 1].\n")
cat("- AIC compares the target interaction model with the misspecified interaction model, with the same formula.\n")
cat("- AIC always favors one of the two candidate models when both AIC values are available.\n")
cat("- aic_fitted_minus_true is positive when AIC favors the target link; the scenario summary reports its median, quartiles, and deciles.\n")
cat("- target_fit_gradient_max and target_fit_hessian_min_eigen audit the custom chance-corrected fit and are NA for glm and glmmTMB fits.\n")
cat("- false_positive_interaction, dharma_detection_min/max, and pregibon_detection are compatibility aliases of the new column names, kept for the manuscript.\n")

chance <- diag_scenarios$chance_floor
plot_grid <- expand.grid(
  age = seq(chance$age_range[1], chance$age_range[2], length.out = settings$age_plot_n),
  group_num = c(0, 1)
)
plot_grid$age_c <- plot_grid$age - chance$age_center
plot_grid$group <- factor(
  plot_grid$group_num,
  levels = c(0, 1),
  labels = c("Group 0", "Group 1")
)
plot_grid$eta <- chance$beta_intercept + chance$beta_age * plot_grid$age_c +
  chance$beta_group * plot_grid$group_num +
  chance$beta_age_group * plot_grid$age_c * plot_grid$group_num
plot_grid$expected_accuracy <- chance_logit_inv_local(plot_grid$eta, chance = chance$chance)

# Save one representative DHARMa plot for inspection. This is not a main-text figure.
example_data <- simulate_chance_floor(chance)
example_fit <- fit_wrong_link_model("chance_floor", example_data, chance, include_interaction = TRUE)
example_sim <- DHARMa::simulateResiduals(
  fittedModel = example_fit,
  n = settings$dharma_n_sim,
  plot = FALSE,
  seed = 123
)

grDevices::pdf(settings$output_dharma_example, width = 7.2, height = 6.5)
old_par <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
DHARMa::plotQQunif(example_sim, main = "Overall uniformity")
DHARMa::plotResiduals(example_sim, quantreg = TRUE, main = "Residuals over fitted values")
DHARMa::plotResiduals(
  example_sim,
  form = example_data$age_c,
  quantreg = TRUE,
  main = "Residuals over age"
)
DHARMa::plotResiduals(
  example_sim,
  form = example_data$group,
  main = "Residuals by group"
)
graphics::par(old_par)
grDevices::dev.off()

saveRDS(
  list(
    settings = settings,
    diag_scenarios = diag_scenarios,
    scenario_table = scenario_table,
    chance_plot_data = plot_grid,
    simulation_results = simulation_results,
    long_summary = long_summary,
    scenario_summary = scenario_summary,
    calibration_summary = calibration_summary,
    example_data = example_data
  ),
  file = settings$output_rds
)

report_section("Saved files")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_replications, "\n", sep = "")
cat("- ", settings$output_long_summary, "\n", sep = "")
cat("- ", settings$output_scenario_summary, "\n", sep = "")
cat("- ", settings$output_scenario_summary_paper, "\n", sep = "")
cat("- ", settings$output_diagnostic_calibration, "\n", sep = "")
cat("- ", settings$output_dharma_example, "\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
