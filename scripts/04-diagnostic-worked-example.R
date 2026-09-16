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

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
rm(list = ls())
library(glmmTMB)
library(lme4)
library(psyphy)
library(DHARMa)

# ---------------------------------------------------------------------
# 0. Project setup
# ---------------------------------------------------------------------

# Run from the repository root. Publication runs use 3000 replications.
B <- as.integer(Sys.getenv("N_SIM", "3000"))
default_alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))

for (path in c("tables", "figs", "outputs", "outputs/inspection")) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

cat("\n", "Diagnostic simulation", "\n")

# ---------------------------------------------------------------------
# 1. User-tunable settings and scenarios
# ---------------------------------------------------------------------

settings <- list(
  B = B,
  n_cores = as.integer(Sys.getenv(
      "N_CORES",
      Sys.getenv("SLURM_CPUS_PER_TASK", max(1L, parallel::detectCores(logical = TRUE) - 1L))
  )),
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
    target_icc = 0.30,
    focal_dharma = "continuous_age",
    variability_summary = "Trial-level Bernoulli sampling with 20 trials per participant and a subject random intercept."
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

cat("\n", "Scenario parameters you can tune", "\n")
print(settings)
cat("\nAIC is used as a same-formula comparison between the target interaction model and the misspecified interaction model.\n")
cat("DHARMa checks are scenario-aware: quantile checks for continuous predictors, categorical checks for categorical design cells.\n")
cat("DHARMa and Pregibon-style checks measure detection under the wrong link and provide baseline calibration rates under the correct link.\n")

# ---------------------------------------------------------------------
# 2. Small utilities
# ---------------------------------------------------------------------

chance_logit_inv_local <- function(eta, chance) {
  chance + (1 - chance) * stats::plogis(eta)
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

# Scenario-aware DHARMa checks. The important change is that quantile
# regression is not attempted for categorical predictors.

# ---------------------------------------------------------------------
# 4. DGPs and initial model fits are written inside run_replication below
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# 5. Deterministic scenario tables
# ---------------------------------------------------------------------

scenario_descriptions <- setNames(rep(NA_character_, length(diag_scenarios)), names(diag_scenarios))
for (name in names(diag_scenarios)) {
  scn <- diag_scenarios[[name]]

  if (name == "count_poisson_log_identity") {
    scenario_descriptions[name] <- paste0(
      "Simple count example figure coefficients: log(E[y]) = ", scn$beta_intercept,
      " + ", scn$beta_age, " * (age - ", scn$age_origin, ") + ",
      scn$beta_group, " * group; no age-by-group product term."
    )
  }
  if (name == "chance_floor") {
    scenario_descriptions[name] <- paste0(
      "Simulation 1 lower-performance coefficients: eta = ", scn$beta_intercept,
      " + ", scn$beta_age, " * (age - ", scn$age_center, ") + ",
      scn$beta_group, " * group + subject random intercept; p = ", scn$chance,
      " + (1 - ", scn$chance, ") * logistic(eta); latent ICC = ", scn$target_icc,
      "; no age-by-group product term on the conditional chance-corrected-logit scale."
    )
  }
  if (name == "probit_dgp_logit_fit") {
    scenario_descriptions[name] <- paste0(
      "Simulation 2 probit reference coefficients: eta = ", scn$beta_intercept,
      " + ", scn$beta_group, " * group + ", scn$beta_condition,
      " * condition + subject random intercept; latent ICC = ", scn$target_icc,
      "; no group-by-condition product term."
    )
  }
  if (name == "probit_continuous_logit_fit") {
    scenario_descriptions[name] <- paste0(
      "Matched continuous counterpart to the 2 x 2 binary scenario: eta = ",
      scn$beta_intercept, " + ", scn$beta_group, " * group + ",
      scn$beta_x, " * x + subject random intercept; x ~ Uniform(",
      scn$x_range[1], ", ", scn$x_range[2], "). Thus the x = 0 to x = 1 contrast has the same probit-scale size as the condition 0 to 1 contrast; latent ICC = ", scn$target_icc, "; no group-by-x product term."
    )
  }
  if (name == "gamma_log_inverse") {
    scenario_descriptions[name] <- paste0(
      "Gamma response-time example, log-link scale: log(E[y]) = log(400) + ", scn$beta_x,
      " * x + ", scn$beta_group,
      " * group; Gamma shape = ", scn$shape,
      "; no x-by-group product term."
    )
  }

}

scenario_table <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
      scn <- diag_scenarios[[name]]

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
        outcome_label <- "expected_accuracy_at_random_intercept_0"
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
        quantitative_description = scenario_descriptions[[name]],
        variability_summary = scn$variability_summary,
        stringsAsFactors = FALSE
      )

  })
)

utils::write.csv(scenario_table, settings$output_scenario_table, row.names = FALSE)

cat("\n", "Implied scenario values", "\n")
print(scenario_table)

# ---------------------------------------------------------------------
# 6. Repeated simulation
# ---------------------------------------------------------------------

run_replication <- function(rep_id, name, scn) {
  if (name == "count_poisson_log_identity") {
    group_num <- stats::rbinom(scn$N, 1, 0.5)
    age <- stats::runif(scn$N, scn$age_range[1], scn$age_range[2])
    age_offset <- age - scn$age_origin
    eta <- scn$beta_intercept + scn$beta_age * age_offset +
      scn$beta_group * group_num + scn$beta_age_group * age_offset * group_num
    mu <- exp(eta)
    y <- stats::rpois(scn$N, lambda = mu)
    d <- data.frame(
      age = age,
      age_offset = age_offset,
      group_num = group_num,
      group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
      eta_true = eta,
      mu_true = mu,
      y = y,
      stringsAsFactors = FALSE
    )

    form <- y ~ age_offset * group
    start_fit <- try(stats::lm(form, data = d), silent = TRUE)
    start <- NULL
    if (!inherits(start_fit, "try-error")) {
      start <- stats::coef(start_fit)
      X <- stats::model.matrix(form, data = d)
      pred <- as.vector(X %*% start)
      if (any(!is.finite(pred))) start <- NULL
      if (!is.null(start) && min(pred, na.rm = TRUE) <= 0) {
        start[1] <- start[1] + abs(min(pred, na.rm = TRUE)) + 1
      }
    }

    fit_wrong_interaction <- if (is.null(start) || any(!is.finite(start))) {
      {
        glm_fit <- try(stats::glm(form, family = stats::poisson(link = "identity"), data = d), silent = TRUE)
        if (!inherits(glm_fit, "try-error") && !isTRUE(glm_fit$converged)) glm_fit <- NULL
        glm_fit
      }
    } else {
      {
        glm_fit <- try(stats::glm(form, family = stats::poisson(link = "identity"), data = d, start = start), silent = TRUE)
        if (!inherits(glm_fit, "try-error") && !isTRUE(glm_fit$converged)) glm_fit <- NULL
        glm_fit
      }
    }

    fit_true_interaction <- try(stats::glm(y ~ age_offset * group, data = d, family = stats::poisson("log")), silent = TRUE)
    if (!inherits(fit_true_interaction, "try-error") && !isTRUE(fit_true_interaction$converged)) fit_true_interaction <- NULL
  } else if (name == "chance_floor") {
    group_num <- stats::rbinom(scn$N, 1, 0.5)
    age <- stats::runif(scn$N, scn$age_range[1], scn$age_range[2])
    age_c <- age - scn$age_center
    u <- stats::rnorm(
      scn$N, mean = 0,
      sd = sqrt(scn$target_icc * (pi^2 / 3) / (1 - scn$target_icc))
    )
    eta <- scn$beta_intercept + scn$beta_age * age_c +
      scn$beta_group * group_num + scn$beta_age_group * age_c * group_num + u
    d <- data.frame(
      id = factor(rep(seq_len(scn$N), each = scn$k_trials)),
      age = rep(age, each = scn$k_trials),
      age_c = rep(age_c, each = scn$k_trials),
      group_num = rep(group_num, each = scn$k_trials),
      group = factor(rep(group_num, each = scn$k_trials),
        levels = c(0, 1), labels = c("Group 0", "Group 1")),
      stringsAsFactors = FALSE
    )
    d$eta_true <- rep(eta, each = scn$k_trials)
    d$p_true <- chance_logit_inv_local(d$eta_true, chance = scn$chance)
    d$correct <- stats::rbinom(nrow(d), size = 1, prob = d$p_true)

    fit_wrong_interaction <- try(lme4::glmer(
      correct ~ age_c * group + (1 | id),
      data = d, family = stats::binomial("logit")
    ), silent = TRUE)
    fit_true_interaction <- try(lme4::glmer(
      correct ~ age_c * group + (1 | id),
      data = d, family = stats::binomial(psyphy::mafc.logit(2))
    ), silent = TRUE)
  } else if (name == "probit_dgp_logit_fit") {
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
      sd = sqrt((scn$target_icc) / (1 - (scn$target_icc)))
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

    fit_wrong_interaction <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id), data = d, family = stats::binomial("logit")), silent = TRUE)

    fit_true_interaction <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id), data = d, family = stats::binomial("probit")), silent = TRUE)
  } else if (name == "probit_continuous_logit_fit") {
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
      sd = sqrt((scn$target_icc) / (1 - (scn$target_icc)))
    )
    eta <- scn$beta_intercept + scn$beta_group * d$group_num +
      scn$beta_x * d$x + scn$beta_group_x * d$group_num * d$x +
      u[as.integer(d$id)]
    p <- stats::pnorm(eta)
    d$y <- stats::rbinom(nrow(d), size = 1, prob = p)
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    d$eta_true <- eta
    d$p_true <- p

    fit_wrong_interaction <- try(glmmTMB::glmmTMB(y ~ group * x + (1 | id), data = d, family = stats::binomial("logit")), silent = TRUE)

    fit_true_interaction <- try(glmmTMB::glmmTMB(y ~ group * x + (1 | id), data = d, family = stats::binomial("probit")), silent = TRUE)
  } else if (name == "gamma_log_inverse") {
    group_num <- stats::rbinom(scn$N, 1, 0.5)
    x <- stats::runif(scn$N, scn$x_range[1], scn$x_range[2])
    eta <- scn$beta_intercept + scn$beta_x * x +
      scn$beta_group * group_num + scn$beta_x_group * x * group_num
    mu <- exp(eta)
    y <- stats::rgamma(scn$N, shape = scn$shape, scale = mu / scn$shape)
    d <- data.frame(
      x = x,
      group_num = group_num,
      group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
      eta_true = eta,
      mu_true = mu,
      y = y,
      stringsAsFactors = FALSE
    )

    form <- y ~ x * group
    start_fit <- try(stats::lm(I(1 / y) ~ x * group, data = d), silent = TRUE)
    start <- NULL
    if (!inherits(start_fit, "try-error")) {
      start <- stats::coef(start_fit)
      X <- stats::model.matrix(form, data = d)
      pred <- as.vector(X %*% start)
      if (any(!is.finite(pred))) start <- NULL
      if (!is.null(start) && min(pred, na.rm = TRUE) <= 0) {
        start[1] <- start[1] + abs(min(pred, na.rm = TRUE)) + 1e-4
      }
    }

    fit_wrong_interaction <- if (is.null(start) || any(!is.finite(start))) {
      {
        glm_fit <- try(stats::glm(form, family = stats::Gamma(link = "inverse"), data = d), silent = TRUE)
        if (!inherits(glm_fit, "try-error") && !isTRUE(glm_fit$converged)) glm_fit <- NULL
        glm_fit
      }
    } else {
      {
        glm_fit <- try(stats::glm(form, family = stats::Gamma(link = "inverse"), data = d, start = start), silent = TRUE)
        if (!inherits(glm_fit, "try-error") && !isTRUE(glm_fit$converged)) glm_fit <- NULL
        glm_fit
      }
    }

    fit_true_interaction <- try(stats::glm(y ~ x * group, data = d, family = stats::Gamma("log")), silent = TRUE)
    if (!inherits(fit_true_interaction, "try-error") && !isTRUE(fit_true_interaction$converged)) fit_true_interaction <- NULL
  }

  interaction_p <- interaction_coef <- NA_real_
  if (!inherits(fit_wrong_interaction, "try-error") && !is.null(fit_wrong_interaction)) {
    if (name %in% c("probit_dgp_logit_fit", "probit_continuous_logit_fit")) {
      sm <- summary(fit_wrong_interaction)$coefficients$cond
    } else {
      sm <- summary(fit_wrong_interaction)$coefficients
    }
    term <- if (name == "count_poisson_log_identity") "age_offset:groupGroup 1" else if (name == "chance_floor") "age_c:groupGroup 1" else if (name == "probit_dgp_logit_fit") "groupGroup 1:conditionCondition 1" else if (name == "probit_continuous_logit_fit") "groupGroup 1:x" else "x:groupGroup 1"
    if (term %in% rownames(sm)) {
      interaction_p <- unname(sm[term, 4])
      interaction_coef <- unname(sm[term, 1])
    }
  }
  # Preserve diagnostic order: DHARMa wrong/correct, then Pregibon wrong/correct.
  dharma_rows <- list()
  for (specification in c("wrong", "correct")) {
    fit <- if (specification == "wrong") fit_wrong_interaction else fit_true_interaction
    data <- d
    empty <- data.frame(
      dharma_uniformity_p = NA_real_,
      dharma_dispersion_p = NA_real_,
      dharma_quantile_fitted_p = NA_real_,
      dharma_quantile_predictor_p = NA_real_,
      dharma_categorical_design_p = NA_real_,
      dharma_valid_tests = 0L,
      dharma_tests_used = "",
      stringsAsFactors = FALSE
    )
    dharma_rows[[specification]] <- empty
    if (inherits(fit, "try-error") || is.null(fit)) next

    sim <- try(DHARMa::simulateResiduals(
        fittedModel = fit,
        n = settings$dharma_n_sim,
        plot = FALSE,
        seed = NULL
      ), silent = TRUE)

    if (inherits(sim, "try-error")) next

    p_uniformity <- {
      check <- try(DHARMa::testUniformity(sim, plot = FALSE), silent = TRUE)
      if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
    }
    p_dispersion <- {
      check <- try(DHARMa::testDispersion(sim, plot = FALSE), silent = TRUE)
      if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
    }

    fitted_response <- as.numeric(stats::predict(fit, type = "response"))
    p_quantile_fitted <- NA_real_
    if (length(fitted_response) == nrow(data) && length(unique(round((fitted_response)[is.finite(fitted_response)], 10))) >= settings$min_unique_for_quantile) {
      p_quantile_fitted <- {
        check <- try(DHARMa::testQuantiles(sim, plot = FALSE), silent = TRUE)
        if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
      }
    }

    p_quantile_predictor <- NA_real_
    p_categorical_design <- NA_real_

    if (identical(scn$focal_dharma, "continuous_age")) {
      x <- if ("age_c" %in% names(data)) data$age_c else data$age_offset
      if (length(unique(round((x)[is.finite(x)], 10))) >= settings$min_unique_for_quantile) {
        p_quantile_predictor <- {
          check <- try(DHARMa::testQuantiles(sim, predictor = x, plot = FALSE), silent = TRUE)
          if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
        }
      }
    }

    if (identical(scn$focal_dharma, "continuous_x")) {
      if (length(unique(round((data$x)[is.finite(data$x)], 10))) >= settings$min_unique_for_quantile) {
        p_quantile_predictor <- {
          check <- try(DHARMa::testQuantiles(sim, predictor = data$x, plot = FALSE), silent = TRUE)
          if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
        }
      }
    }

    if (identical(scn$focal_dharma, "continuous_binary_x")) {
      if (length(unique(round((data$x)[is.finite(data$x)], 10))) >= settings$min_unique_for_quantile) {
        p_quantile_predictor <- {
          check <- try(DHARMa::testQuantiles(sim, predictor = data$x, plot = FALSE), silent = TRUE)
          if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
        }
      }
    }

    if (identical(scn$focal_dharma, "categorical_design")) {
      design_cell <- interaction(data$group, data$condition, drop = TRUE)
      res <- sim$scaledResiduals
      # Preserve the existing Kruskal-Wallis test of scaled residuals by design
      # cell; DHARMa::testCategorical does not supply the scalar p-value used here.
      if (nlevels(design_cell) >= 2 && length(res) == length(design_cell)) {
        p_categorical_design <- {
          check <- try(stats::kruskal.test(res ~ design_cell), silent = TRUE)
          if (inherits(check, "try-error") || length(check$p.value) != 1L) NA_real_ else unname(check$p.value)
        }
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

    dharma_rows[[specification]] <- data.frame(
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
  dh_wrong <- dharma_rows$wrong
  dh_correct <- dharma_rows$correct
  pregibon_values <- c(wrong = NA_real_, correct = NA_real_)
  for (specification in c("wrong", "correct")) {
    fit <- if (specification == "wrong") fit_wrong_interaction else fit_true_interaction
    data <- d
    if (inherits(fit, "try-error") || is.null(fit)) next

    # For mixed models, construct the added term from the fixed-effects linear
    # predictor only. The default glmmTMB prediction includes estimated random
    # effects; squaring those empirical Bayes estimates and feeding them back as
    # an ordinary covariate is badly anti-conservative under the correct link.
    eta_hat <- try(
      if (inherits(fit, "glmmTMB") || inherits(fit, "merMod")) {
        stats::predict(fit, type = "link", re.form = NA)
      } else {
        stats::predict(fit, type = "link")
      },
      silent = TRUE
    )
    if (inherits(eta_hat, "try-error") || length(eta_hat) != nrow(data)) next

    data$eta_hat_sq <- as.numeric(eta_hat)^2

    # A saturated fixed-effects design can already span eta_hat_sq. This occurs
    # in the 2 x 2 interaction model, whose four cell indicators span any
    # function of the four fitted cell predictors. The added coefficient is then
    # not identifiable, so the check is not applicable.
    if (inherits(fit, "glmmTMB") || inherits(fit, "merMod")) {
      fixed_formula <- try(
        if (inherits(fit, "glmmTMB")) stats::formula(fit, fixed.only = TRUE) else lme4::nobars(stats::formula(fit)),
        silent = TRUE
      )
      if (inherits(fixed_formula, "try-error")) next
      augmented_fixed_formula <- stats::update.formula(
        fixed_formula,
        . ~ . + eta_hat_sq
      )
      X_base <- try(stats::model.matrix(fixed_formula, data = data), silent = TRUE)
      X_augmented <- try(
        stats::model.matrix(augmented_fixed_formula, data = data),
        silent = TRUE
      )
      if (inherits(X_base, "try-error") || inherits(X_augmented, "try-error")) {
        next
      }
      if (qr(X_augmented)$rank <= qr(X_base)$rank) next
    }

    augmented_formula <- stats::update.formula(
      stats::formula(fit),
      . ~ . + eta_hat_sq
    )
    fit_family <- stats::family(fit)
    if (inherits(fit, "glmmTMB")) {
      fit2 <- try(glmmTMB::glmmTMB(
          augmented_formula,
          data = data,
          family = fit_family
        ), silent = TRUE)
    } else if (inherits(fit, "merMod")) {
      fit2 <- try(lme4::glmer(
          augmented_formula,
          data = data,
          family = fit_family
        ), silent = TRUE)
    } else {
        # glm() reads `start` by position, and model.matrix() orders the added
        # main effect eta_hat_sq before any interaction term. Appending the zero
        # to coef(fit) therefore handed the interaction estimate to eta_hat_sq and
        # zero to the interaction. Under an identity link that start implies
        # negative fitted means, so the refit aborted with "cannot find valid
        # starting values" and the check returned NA in nearly every count-scenario
        # replication. Aligning the start by name keeps it at the current fit.
        X_start <- try(stats::model.matrix(augmented_formula, data = data), silent = TRUE)
        if (inherits(X_start, "try-error")) next
        start <- stats::setNames(rep(0, ncol(X_start)), colnames(X_start))
        current <- stats::coef(fit)
        shared <- intersect(names(current), names(start))
        start[shared] <- current[shared]
        fit2 <- {
          glm_fit <- try(stats::glm(
              augmented_formula,
              data = data,
              family = fit_family,
              start = start
            ), silent = TRUE)
          if (!inherits(glm_fit, "try-error") && !isTRUE(glm_fit$converged)) glm_fit <- NULL
          glm_fit
        }
    }

    if (inherits(fit2, "try-error") || is.null(fit2)) next

    if (inherits(fit2, "glmmTMB")) {
      sm <- try(summary(fit2)$coefficients$cond, silent = TRUE)
    } else {
      sm <- try(stats::coef(summary(fit2)), silent = TRUE)
    }

    if (inherits(sm, "try-error")) next
    if (!"eta_hat_sq" %in% rownames(sm)) next
    p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
    if (is.na(p_col)) next
    pregibon_values[specification] <- unname(sm["eta_hat_sq", p_col])
  }
  preg_wrong <- unname(pregibon_values["wrong"])
  preg_correct <- unname(pregibon_values["correct"])

  aic_true <- {
    if (inherits(fit_true_interaction, "try-error") || is.null(fit_true_interaction)) NA_real_ else {
      aic_value <- try(stats::AIC(fit_true_interaction), silent = TRUE)
      if (inherits(aic_value, "try-error") || !is.finite(aic_value)) NA_real_ else unname(aic_value)
    }
  }
  aic_fitted <- {
    if (inherits(fit_wrong_interaction, "try-error") || is.null(fit_wrong_interaction)) NA_real_ else {
      aic_value <- try(stats::AIC(fit_wrong_interaction), silent = TRUE)
      if (inherits(aic_value, "try-error") || !is.finite(aic_value)) NA_real_ else unname(aic_value)
    }
  }

  data.frame(
    scenario = scn$scenario,
    true_link_function = scn$true_link_function,
    fitted_link_function = scn$fitted_link_function,
    interaction_p = interaction_p,
    interaction_coef = interaction_coef,
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
    aic_true_link_interaction = aic_true,
    aic_fitted_link_interaction = aic_fitted,
    aic_favors_true_link = if (is.finite(aic_true) && is.finite(aic_fitted)) aic_true <= aic_fitted else NA,
    aic_favors_fitted_link = if (is.finite(aic_true) && is.finite(aic_fitted)) aic_fitted < aic_true else NA,
    aic_fitted_minus_true = if (is.finite(aic_true) && is.finite(aic_fitted)) aic_fitted - aic_true else NA_real_,
    target_fit_converged = if (inherits(fit_true_interaction, "merMod")) {
      length(fit_true_interaction@optinfo$conv$lme4$messages) == 0 &&
        !lme4::isSingular(fit_true_interaction, tol = 1e-4)
    } else {
      NA
    },
    target_fit_gradient_max = NA_real_,
    target_fit_hessian_min_eigen = NA_real_,
    stringsAsFactors = FALSE
  )
}

cat(
  "\nStarting diagnostic simulation with ",
  settings$n_cores,
  if (settings$n_cores == 1L) " worker\n" else " workers\n",
  sep = ""
)

cluster <- NULL
if (settings$n_cores > 1L && .Platform$OS.type != "unix") {
  cluster <- parallel::makeCluster(settings$n_cores)
  parallel::clusterSetRNGStream(cluster, iseed = settings$seed)
  parallel::clusterExport(cluster, c(
      "settings", "chance_logit_inv_local", "diag_scenarios", "run_replication"))
}
scenario_results <- list()
for (name in names(diag_scenarios)) {
  scn <- diag_scenarios[[name]]
  cat("Scenario:", scn$scenario, "B =", settings$B, "cores =", settings$n_cores, "\n")
  if (settings$n_cores > 1L && .Platform$OS.type == "unix") {
    out <- parallel::mclapply(seq_len(settings$B), run_replication,
      name = name, scn = scn, mc.cores = settings$n_cores, mc.set.seed = TRUE)
  } else if (!is.null(cluster)) {
    out <- parallel::parLapply(cluster, seq_len(settings$B), run_replication, name = name, scn = scn)
  } else {
    out <- lapply(seq_len(settings$B), run_replication, name = name, scn = scn)
  }
  scenario_results[[length(scenario_results) + 1L]] <- do.call(rbind, out)
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
simulation_results <- do.call(rbind, scenario_results)

# ---------------------------------------------------------------------
# 7. Summaries
# ---------------------------------------------------------------------

long_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
      scn <- diag_scenarios[[name]]
      dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
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
                quantitative_description = scenario_descriptions[[name]],
                variability_summary = scn$variability_summary,
                quantity = unname(quantities[key]),
                diagnostic_family = unname(families[key]),
                stringsAsFactors = FALSE
              ),
              summaries[[key]]
            )
        })
      )
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

calibration_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
      scn <- diag_scenarios[[name]]
      dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
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
  })
)

scenario_summary <- do.call(
  rbind,
  lapply(names(diag_scenarios), function(name) {
      scn <- diag_scenarios[[name]]
      dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
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
        quantitative_description = scenario_descriptions[[name]],
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
  })
)

utils::write.csv(simulation_results, settings$output_replications, row.names = FALSE)
utils::write.csv(long_summary, settings$output_long_summary, row.names = FALSE)
utils::write.csv(scenario_summary, settings$output_scenario_summary, row.names = FALSE)
utils::write.csv(scenario_summary, settings$output_scenario_summary_paper, row.names = FALSE)
utils::write.csv(calibration_summary, settings$output_diagnostic_calibration, row.names = FALSE)

cat("\n", "Long diagnostic summary", "\n")
print(long_summary)
cat("\n", "Scenario-level diagnostic summary", "\n")
print(scenario_summary)
cat("\n", "Diagnostic calibration summary", "\n")
print(calibration_summary)

cat("\nInterpretation aid:\n")
cat("- The pseudo-interaction detection rate uses the fitted wrong-link interaction model.\n")
cat("- DHARMa and Pregibon checks on the wrong-link interaction model measure detection of misspecification.\n")
cat("- The same checks on the correct-link interaction model provide baseline calibration rates.\n")
cat("- DHARMa quantile checks are used only when the predictor has enough unique values.\n")
cat("- For the 2 x 2 binary repeated-trials scenario, DHARMa uses a categorical design-cell check instead.\n")
cat("- The Pregibon-style check uses the fixed-effects linear predictor for GLMMs; it is not applicable when its square does not increase the fixed-effects design rank, as in the saturated 2 x 2 interaction model.\n")
cat("- The continuous binary scenario matches the 2 x 2 binary scenario in coefficients, ICC, and trials, but replaces condition with x in [0, 1].\n")
cat("- AIC compares the target interaction model with the misspecified interaction model, with the same formula.\n")
cat("- AIC always favors one of the two candidate models when both AIC values are available.\n")
cat("- aic_fitted_minus_true is positive when AIC favors the target link; the scenario summary reports its median, quartiles, and deciles.\n")
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

group_num <- stats::rbinom(chance$N, 1, 0.5)
age <- stats::runif(chance$N, chance$age_range[1], chance$age_range[2])
age_c <- age - chance$age_center
u <- stats::rnorm(
  chance$N, mean = 0,
  sd = sqrt(chance$target_icc * (pi^2 / 3) / (1 - chance$target_icc))
)
eta <- chance$beta_intercept + chance$beta_age * age_c +
  chance$beta_group * group_num + chance$beta_age_group * age_c * group_num + u
example_data <- data.frame(
  id = factor(rep(seq_len(chance$N), each = chance$k_trials)),
  age = rep(age, each = chance$k_trials),
  age_c = rep(age_c, each = chance$k_trials),
  group = factor(rep(group_num, each = chance$k_trials),
    levels = c(0, 1), labels = c("Group 0", "Group 1")),
  stringsAsFactors = FALSE
)
example_data$eta_true <- rep(eta, each = chance$k_trials)
example_data$p_true <- chance_logit_inv_local(example_data$eta_true, chance = chance$chance)
example_data$correct <- stats::rbinom(nrow(example_data), size = 1, prob = example_data$p_true)

example_fit <- lme4::glmer(
  correct ~ age_c * group + (1 | id),
  data = example_data, family = stats::binomial("logit")
)
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

cat("\n", "Saved files", "\n")
cat("- ", settings$output_scenario_table, "\n", sep = "")
cat("- ", settings$output_replications, "\n", sep = "")
cat("- ", settings$output_long_summary, "\n", sep = "")
cat("- ", settings$output_scenario_summary, "\n", sep = "")
cat("- ", settings$output_scenario_summary_paper, "\n", sep = "")
cat("- ", settings$output_diagnostic_calibration, "\n", sep = "")
cat("- ", settings$output_dharma_example, "\n", sep = "")
cat("- ", settings$output_rds, "\n", sep = "")
cat("\nDone.\n")
