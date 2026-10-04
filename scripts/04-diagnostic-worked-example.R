# Diagnostic simulation: do model checks flag the same wrong links that generate
# pseudo-interactions? Scenarios are tied to examples already used in the paper:
#   1. Poisson count: true log link, fitted identity link.
#   2. Forced-choice accuracy: true chance-corrected logit, fitted standard logit.
#   3. Binary repeated trials, 2 x 2: true probit GLMM, fitted logit GLMM.
#   4. Binary repeated trials, continuous counterpart of 3: same coefficients and
#      total trials, but condition replaced by x ~ Uniform(0, 1).
#   5. Gamma positive outcome: true log link, fitted inverse link.
# No scenario has a product term on the true scale. Checks on the wrong-link model
# measure detection; the same checks on the correct-link model give calibration.
# AIC compares the two models with the same interaction formula.
# Run from the repository root. N_SIM, N_CORES, ALPHA, DHARMA_N_SIM can be set as environment variables.

rm(list = ls())
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(glmmTMB)
library(lme4)
library(psyphy)
library(DHARMa)
library(parallel)

B <- as.integer(Sys.getenv("N_SIM", "3000")) # replications per scenario (3000 for the paper)
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
alpha <- as.numeric(Sys.getenv("ALPHA", "0.05"))
dharma_n_sim <- as.integer(Sys.getenv("DHARMA_N_SIM", "250")) # simulated datasets per DHARMa check
min_unique_for_quantile <- 8 # DHARMa quantile tests only with enough distinct predictor values
seed <- 20260608
set.seed(seed)
for (path in c("tables", "outputs/inspection")) dir.create(path, recursive = TRUE, showWarnings = FALSE)

# Scenario order is the table order. focal_dharma selects the scenario-specific DHARMa check.
diag_scenarios <- list(
  count_poisson_log_identity = list(scenario = "Count errors", paper_anchor = "Simple count example figure",
    outcome_family = "Poisson count", true_link_function = "log", fitted_link_function = "identity",
    true_model_label = "Poisson log", fitted_model_label = "Poisson identity",
    N = 180, age_range = c(6, 10), age_origin = 6,
    beta_intercept = 2.20, beta_age = -0.55, beta_group = 0.45, beta_age_group = 0,
    focal_dharma = "continuous_age", variability_summary = "Poisson sampling; variance equals mean."),
  chance_floor = list(scenario = "Chance-floor accuracy", paper_anchor = "Simulation 1, lower-performance forced-choice scenario",
    outcome_family = "Binomial accuracy", true_link_function = "chance-corrected logit", fitted_link_function = "standard logit",
    true_model_label = "chance-corrected binomial logit", fitted_model_label = "standard binomial logit",
    N = 250, k_trials = 20, chance = 0.50, age_range = c(6, 10), age_center = 8,
    beta_intercept = -0.80, beta_age = 0.60, beta_group = -0.90, beta_age_group = 0, target_icc = 0.30,
    focal_dharma = "continuous_age",
    variability_summary = "Trial-level Bernoulli sampling with 20 trials per participant and a subject random intercept."),
  probit_dgp_logit_fit = list(scenario = "Binary repeated trials", paper_anchor = "Simulation 2, probit-generated logit-fitted case",
    outcome_family = "Binary repeated trials", true_link_function = "probit", fitted_link_function = "logit",
    true_model_label = "binomial probit GLMM", fitted_model_label = "binomial logit GLMM",
    n_subjects = 300, k_trials = 15, target_icc = 0.30,
    beta_intercept = 1.50, beta_group = -1.00, beta_condition = -1.00, beta_group_condition = 0,
    focal_dharma = "categorical_design",
    variability_summary = "Subject random intercept with latent ICC = .30 and 15 binary trials per cell."),
  probit_continuous_logit_fit = list(scenario = "Binary continuous predictor", paper_anchor = "Matched continuous-predictor counterpart to Simulation 2",
    outcome_family = "Binary repeated trials", true_link_function = "probit", fitted_link_function = "logit",
    true_model_label = "binomial probit GLMM", fitted_model_label = "binomial logit GLMM",
    n_subjects = 300, trials_per_subject = 30, target_icc = 0.30, x_range = c(0, 1),
    beta_intercept = 1.50, beta_group = -1.00, beta_x = -1.00, beta_group_x = 0,
    focal_dharma = "continuous_binary_x",
    variability_summary = "Matched to the 2 x 2 binary scenario: same fixed effects, same latent ICC = .30, and 30 binary trials per subject; condition is replaced by x ~ Uniform(0, 1)."),
  gamma_log_inverse = list(scenario = "Gamma mean response time", paper_anchor = "Gamma response-time example from the introduction (log versus inverse link)",
    outcome_family = "Gamma positive continuous", true_link_function = "log", fitted_link_function = "inverse",
    true_model_label = "Gamma log", fitted_model_label = "Gamma inverse",
    N = 220, shape = 8, x_range = c(-2, 2), beta_intercept = log(400), beta_x = 0.25, beta_group = 0.25, beta_x_group = 0,
    focal_dharma = "continuous_x", variability_summary = "Gamma sampling with shape = 8; mean around 400 ms at x = 0, group = 0.")
)

s1 <- diag_scenarios$count_poisson_log_identity
s2 <- diag_scenarios$chance_floor
s3 <- diag_scenarios$probit_dgp_logit_fit
s4 <- diag_scenarios$probit_continuous_logit_fit
s5 <- diag_scenarios$gamma_log_inverse
scenario_descriptions <- c(
  count_poisson_log_identity = paste0("Simple count example figure coefficients: log(E[y]) = ", s1$beta_intercept,
    " + ", s1$beta_age, " * (age - ", s1$age_origin, ") + ", s1$beta_group, " * group; no age-by-group product term."),
  chance_floor = paste0("Simulation 1 lower-performance coefficients: eta = ", s2$beta_intercept,
    " + ", s2$beta_age, " * (age - ", s2$age_center, ") + ", s2$beta_group, " * group + subject random intercept; p = ",
    s2$chance, " + (1 - ", s2$chance, ") * logistic(eta); latent ICC = ", s2$target_icc,
    "; no age-by-group product term on the conditional chance-corrected-logit scale."),
  probit_dgp_logit_fit = paste0("Simulation 2 probit reference coefficients: eta = ", s3$beta_intercept,
    " + ", s3$beta_group, " * group + ", s3$beta_condition, " * condition + subject random intercept; latent ICC = ",
    s3$target_icc, "; no group-by-condition product term."),
  probit_continuous_logit_fit = paste0("Matched continuous counterpart to the 2 x 2 binary scenario: eta = ",
    s4$beta_intercept, " + ", s4$beta_group, " * group + ", s4$beta_x, " * x + subject random intercept; x ~ Uniform(",
    s4$x_range[1], ", ", s4$x_range[2], "). Thus the x = 0 to x = 1 contrast has the same probit-scale size as the condition 0 to 1 contrast; latent ICC = ",
    s4$target_icc, "; no group-by-x product term."),
  gamma_log_inverse = paste0("Gamma response-time example, log-link scale: log(E[y]) = log(400) + ", s5$beta_x,
    " * x + ", s5$beta_group, " * group; Gamma shape = ", s5$shape, "; no x-by-group product term.")
)

cat("\nDiagnostic simulation\n")
cat("B =", B, "| DHARMa simulations =", dharma_n_sim, "| alpha =", alpha, "\n")

####################################################
# Deterministic scenario table
####################################################

cells <- list(
  count_poisson_log_identity = expand.grid(predictor_value = c(6, 8, 10), group_num = c(0, 1)),
  chance_floor = expand.grid(predictor_value = c(6, 8, 10), group_num = c(0, 1)),
  probit_dgp_logit_fit = expand.grid(predictor_value = c(0, 1), group_num = c(0, 1)),
  probit_continuous_logit_fit = expand.grid(predictor_value = c(s4$x_range[1], mean(s4$x_range), s4$x_range[2]), group_num = c(0, 1)),
  gamma_log_inverse = expand.grid(predictor_value = c(-2, 0, 2), group_num = c(0, 1))
)
g <- cells$count_poisson_log_identity
cells$count_poisson_log_identity$eta <- s1$beta_intercept + s1$beta_age * (g$predictor_value - s1$age_origin) + s1$beta_group * g$group_num
cells$count_poisson_log_identity$expected <- exp(cells$count_poisson_log_identity$eta)
g <- cells$chance_floor
cells$chance_floor$eta <- s2$beta_intercept + s2$beta_age * (g$predictor_value - s2$age_center) + s2$beta_group * g$group_num
cells$chance_floor$expected <- s2$chance + (1 - s2$chance) * plogis(cells$chance_floor$eta)
g <- cells$probit_dgp_logit_fit
cells$probit_dgp_logit_fit$eta <- s3$beta_intercept + s3$beta_group * g$group_num + s3$beta_condition * g$predictor_value
cells$probit_dgp_logit_fit$expected <- pnorm(cells$probit_dgp_logit_fit$eta)
g <- cells$probit_continuous_logit_fit
cells$probit_continuous_logit_fit$eta <- s4$beta_intercept + s4$beta_group * g$group_num + s4$beta_x * g$predictor_value
cells$probit_continuous_logit_fit$expected <- pnorm(cells$probit_continuous_logit_fit$eta)
g <- cells$gamma_log_inverse
cells$gamma_log_inverse$eta <- s5$beta_intercept + s5$beta_x * g$predictor_value + s5$beta_group * g$group_num
cells$gamma_log_inverse$expected <- exp(cells$gamma_log_inverse$eta)

predictor_names <- c("age", "age", "condition", "x", "x")
expected_labels <- c("expected_count", "expected_accuracy_at_random_intercept_0", "expected_probability_at_random_intercept_0",
                     "expected_probability_at_random_intercept_0", "expected_mean")
scenario_table <- NULL
for (k in 1:5) {
  scn <- diag_scenarios[[k]]
  g <- cells[[k]]
  scenario_table <- rbind(scenario_table, data.frame(scenario = scn$scenario, paper_anchor = scn$paper_anchor,
    outcome_family = scn$outcome_family, true_link_function = scn$true_link_function,
    fitted_link_function = scn$fitted_link_function, true_model_label = scn$true_model_label,
    fitted_model_label = scn$fitted_model_label, predictor_name = predictor_names[k], predictor_value = g$predictor_value,
    group = paste("Group", g$group_num), linear_predictor = g$eta, expected_value = g$expected,
    expected_value_label = expected_labels[k], quantitative_description = scenario_descriptions[[k]],
    variability_summary = scn$variability_summary))
}
write.csv(scenario_table, "tables/scenario-table-diagnostic-worked-example.csv", row.names = FALSE)
print(scenario_table[, c("scenario", "predictor_name", "predictor_value", "group", "expected_value")])

####################################################
# Functions for one replication
####################################################

# failed fits become NULL, so that one failure does not stop the run
try_fit <- function(expr) tryCatch(expr, error = function(e) NULL)
pval <- function(test) if (inherits(test, "try-error") || length(test$p.value) != 1) NA else unname(test$p.value)

# Scenario-aware DHARMa checks on one fitted model: quantile tests for continuous
# predictors, Kruskal-Wallis of scaled residuals across cells for the 2 x 2 design
dharma_checks <- function(fit, d, focal) {
  p <- c(uniformity = NA, dispersion = NA, quantile_fitted = NA, quantile_predictor = NA, categorical_design = NA)
  sim <- if (is.null(fit)) NULL else try_fit(simulateResiduals(fittedModel = fit, n = dharma_n_sim, plot = FALSE, seed = NULL))
  if (!is.null(sim)) {
    p["uniformity"] <- pval(try(testUniformity(sim, plot = FALSE), silent = TRUE))
    p["dispersion"] <- pval(try(testDispersion(sim, plot = FALSE), silent = TRUE))
    fitted <- as.numeric(predict(fit, type = "response"))
    if (length(fitted) == nrow(d) && length(unique(round(fitted[is.finite(fitted)], 10))) >= min_unique_for_quantile) {
      p["quantile_fitted"] <- pval(try(testQuantiles(sim, plot = FALSE), silent = TRUE))
    }
    if (focal != "categorical_design") {
      x <- if (focal == "continuous_age") { if ("age_c" %in% names(d)) d$age_c else d$age_offset } else d$x
      if (length(unique(round(x[is.finite(x)], 10))) >= min_unique_for_quantile) {
        p["quantile_predictor"] <- pval(try(testQuantiles(sim, predictor = x, plot = FALSE), silent = TRUE))
      }
    } else {
      design_cell <- interaction(d$group, d$condition, drop = TRUE)
      res <- sim$scaledResiduals
      if (nlevels(design_cell) >= 2 && length(res) == length(design_cell)) {
        p["categorical_design"] <- pval(try(kruskal.test(res ~ design_cell), silent = TRUE))
      }
    }
  }
  used <- names(p)[!is.na(p)]
  data.frame(uniformity_p = p[["uniformity"]], dispersion_p = p[["dispersion"]], quantile_fitted_p = p[["quantile_fitted"]],
             quantile_predictor_p = p[["quantile_predictor"]], categorical_design_p = p[["categorical_design"]],
             valid_tests = length(used), tests_used = paste(used, collapse = "; "))
}

# Pregibon-style check: refit with the squared linear predictor added; returns its p-value.
# For mixed models eta_hat uses fixed effects only (squared empirical Bayes random
# effects as a covariate are badly anti-conservative under the correct link).
# Not applicable (NA) when eta_hat^2 does not increase the rank of the fixed-effects
# design, as in the saturated 2 x 2 interaction model.
pregibon_p <- function(fit, d) {
  if (is.null(fit)) return(NA)
  mixed <- inherits(fit, "glmmTMB") || inherits(fit, "merMod")
  eta_hat <- if (mixed) predict(fit, type = "link", re.form = NA) else predict(fit, type = "link")
  if (length(eta_hat) != nrow(d)) return(NA)
  d$eta_hat_sq <- as.numeric(eta_hat)^2
  if (mixed) {
    fixed <- if (inherits(fit, "glmmTMB")) formula(fit, fixed.only = TRUE) else nobars(formula(fit))
    if (qr(model.matrix(update(fixed, . ~ . + eta_hat_sq), data = d))$rank <= qr(model.matrix(fixed, data = d))$rank) return(NA)
  }
  form <- update(formula(fit), . ~ . + eta_hat_sq)
  if (inherits(fit, "glmmTMB")) {
    fit2 <- try_fit(glmmTMB(form, data = d, family = family(fit)))
  } else if (inherits(fit, "merMod")) {
    fit2 <- try_fit(glmer(form, data = d, family = family(fit)))
  } else {
    # glm() reads start by position and model.matrix() puts eta_hat_sq before the
    # interaction, so align the current estimates by name (0 for eta_hat_sq)
    X <- model.matrix(form, data = d)
    start <- setNames(rep(0, ncol(X)), colnames(X))
    shared <- intersect(names(coef(fit)), names(start))
    start[shared] <- coef(fit)[shared]
    fit2 <- try_fit(glm(form, data = d, family = family(fit), start = start))
    if (!isTRUE(fit2$converged)) fit2 <- NULL
  }
  if (is.null(fit2)) return(NA)
  tab <- if (inherits(fit2, "glmmTMB")) summary(fit2)$coefficients$cond else coef(summary(fit2))
  if (!"eta_hat_sq" %in% rownames(tab)) return(NA)
  unname(tab["eta_hat_sq", grep("Pr\\(", colnames(tab))[1]])
}

# One replication of scenario k: DGP, wrong-link and correct-link interaction models, checks
sim_one <- function(b, k) {
  set.seed(seed + 100000 * k + b)
  name <- names(diag_scenarios)[k]
  scn <- diag_scenarios[[k]]

  if (name == "count_poisson_log_identity") {
    group_num <- rbinom(scn$N, 1, 0.5)
    age <- runif(scn$N, scn$age_range[1], scn$age_range[2])
    age_offset <- age - scn$age_origin
    mu <- exp(scn$beta_intercept + scn$beta_age * age_offset + scn$beta_group * group_num + scn$beta_age_group * age_offset * group_num)
    d <- data.frame(age_offset = age_offset, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
                    y = rpois(scn$N, mu))
    # identity-link Poisson needs positive fitted means: start from lm, shifted up if needed
    start <- coef(lm(y ~ age_offset * group, data = d))
    pred <- as.vector(model.matrix(y ~ age_offset * group, data = d) %*% start)
    if (any(!is.finite(pred))) start <- NULL else if (min(pred) <= 0) start[1] <- start[1] + abs(min(pred)) + 1
    fit_wrong <- try_fit(glm(y ~ age_offset * group, family = poisson(link = "identity"), data = d, start = start))
    fit_correct <- try_fit(glm(y ~ age_offset * group, family = poisson("log"), data = d))
    term <- "age_offset:groupGroup 1"
  }
  if (name == "chance_floor") {
    group_num <- rbinom(scn$N, 1, 0.5)
    age <- runif(scn$N, scn$age_range[1], scn$age_range[2])
    age_c <- age - scn$age_center
    u <- rnorm(scn$N, 0, sqrt(scn$target_icc * (pi^2 / 3) / (1 - scn$target_icc)))
    eta <- scn$beta_intercept + scn$beta_age * age_c + scn$beta_group * group_num + scn$beta_age_group * age_c * group_num + u
    d <- data.frame(id = factor(rep(1:scn$N, each = scn$k_trials)), age_c = rep(age_c, each = scn$k_trials),
      group = factor(rep(group_num, each = scn$k_trials), levels = c(0, 1), labels = c("Group 0", "Group 1")))
    d$correct <- rbinom(nrow(d), 1, scn$chance + (1 - scn$chance) * plogis(rep(eta, each = scn$k_trials)))
    fit_wrong <- try_fit(glmer(correct ~ age_c * group + (1 | id), data = d, family = binomial("logit")))
    fit_correct <- try_fit(glmer(correct ~ age_c * group + (1 | id), data = d, family = binomial(mafc.logit(2))))
    term <- "age_c:groupGroup 1"
  }
  if (name == "probit_dgp_logit_fit") {
    d <- data.frame(id = factor(rep(1:scn$n_subjects, each = 2 * scn$k_trials)),
      group_num = rep(rep(c(0, 1), each = scn$n_subjects / 2), each = 2 * scn$k_trials),
      condition_num = rep(rep(c(0, 1), each = scn$k_trials), times = scn$n_subjects))
    u <- rnorm(scn$n_subjects, 0, sqrt(scn$target_icc / (1 - scn$target_icc)))
    eta <- scn$beta_intercept + scn$beta_group * d$group_num + scn$beta_condition * d$condition_num +
      scn$beta_group_condition * d$group_num * d$condition_num + u[as.integer(d$id)]
    d$y <- rbinom(nrow(d), 1, pnorm(eta))
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
    fit_wrong <- try_fit(glmmTMB(y ~ group * condition + (1 | id), data = d, family = binomial("logit")))
    fit_correct <- try_fit(glmmTMB(y ~ group * condition + (1 | id), data = d, family = binomial("probit")))
    term <- "groupGroup 1:conditionCondition 1"
  }
  if (name == "probit_continuous_logit_fit") {
    d <- data.frame(id = factor(rep(1:scn$n_subjects, each = scn$trials_per_subject)),
      group_num = rep(rep(c(0, 1), each = scn$n_subjects / 2), each = scn$trials_per_subject),
      x = runif(scn$n_subjects * scn$trials_per_subject, scn$x_range[1], scn$x_range[2]))
    u <- rnorm(scn$n_subjects, 0, sqrt(scn$target_icc / (1 - scn$target_icc)))
    eta <- scn$beta_intercept + scn$beta_group * d$group_num + scn$beta_x * d$x + scn$beta_group_x * d$group_num * d$x + u[as.integer(d$id)]
    d$y <- rbinom(nrow(d), 1, pnorm(eta))
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    fit_wrong <- try_fit(glmmTMB(y ~ group * x + (1 | id), data = d, family = binomial("logit")))
    fit_correct <- try_fit(glmmTMB(y ~ group * x + (1 | id), data = d, family = binomial("probit")))
    term <- "groupGroup 1:x"
  }
  if (name == "gamma_log_inverse") {
    group_num <- rbinom(scn$N, 1, 0.5)
    x <- runif(scn$N, scn$x_range[1], scn$x_range[2])
    mu <- exp(scn$beta_intercept + scn$beta_x * x + scn$beta_group * group_num + scn$beta_x_group * x * group_num)
    d <- data.frame(x = x, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
                    y = rgamma(scn$N, shape = scn$shape, scale = mu / scn$shape))
    # inverse-link Gamma needs positive fitted 1/means: start from lm on 1/y, shifted up if needed
    start <- coef(lm(I(1 / y) ~ x * group, data = d))
    pred <- as.vector(model.matrix(y ~ x * group, data = d) %*% start)
    if (any(!is.finite(pred))) start <- NULL else if (min(pred) <= 0) start[1] <- start[1] + abs(min(pred)) + 1e-4
    fit_wrong <- try_fit(glm(y ~ x * group, family = Gamma(link = "inverse"), data = d, start = start))
    fit_correct <- try_fit(glm(y ~ x * group, family = Gamma("log"), data = d))
    term <- "x:groupGroup 1"
  }
  # non-converged GLMs are treated as failed fits
  if (inherits(fit_wrong, "glm") && !fit_wrong$converged) fit_wrong <- NULL
  if (inherits(fit_correct, "glm") && !fit_correct$converged) fit_correct <- NULL

  interaction_p <- interaction_coef <- NA
  if (!is.null(fit_wrong)) {
    tab <- if (inherits(fit_wrong, "glmmTMB")) summary(fit_wrong)$coefficients$cond else summary(fit_wrong)$coefficients
    if (term %in% rownames(tab)) {
      interaction_p <- unname(tab[term, 4])
      interaction_coef <- unname(tab[term, 1])
    }
  }
  dh_wrong <- dharma_checks(fit_wrong, d, scn$focal_dharma)
  dh_correct <- dharma_checks(fit_correct, d, scn$focal_dharma)
  preg_wrong <- tryCatch(pregibon_p(fit_wrong, d), error = function(e) NA)
  preg_correct <- tryCatch(pregibon_p(fit_correct, d), error = function(e) NA)
  aic_true <- if (is.null(fit_correct)) NA else tryCatch(AIC(fit_correct), error = function(e) NA)
  aic_fitted <- if (is.null(fit_wrong)) NA else tryCatch(AIC(fit_wrong), error = function(e) NA)
  if (!is.finite(aic_true)) aic_true <- NA
  if (!is.finite(aic_fitted)) aic_fitted <- NA
  both_aic <- !is.na(aic_true) && !is.na(aic_fitted)

  # the unprefixed dharma_* and pregibon_link_test_p columns refer to the wrong-link model
  data.frame(scenario = scn$scenario, true_link_function = scn$true_link_function, fitted_link_function = scn$fitted_link_function,
    interaction_p = interaction_p, interaction_coef = interaction_coef,
    dharma_uniformity_p = dh_wrong$uniformity_p, dharma_dispersion_p = dh_wrong$dispersion_p,
    dharma_quantile_fitted_p = dh_wrong$quantile_fitted_p, dharma_quantile_predictor_p = dh_wrong$quantile_predictor_p,
    dharma_categorical_design_p = dh_wrong$categorical_design_p, dharma_valid_tests = dh_wrong$valid_tests,
    dharma_tests_used = dh_wrong$tests_used, pregibon_link_test_p = preg_wrong,
    dharma_wrong_uniformity_p = dh_wrong$uniformity_p, dharma_wrong_dispersion_p = dh_wrong$dispersion_p,
    dharma_wrong_quantile_fitted_p = dh_wrong$quantile_fitted_p, dharma_wrong_quantile_predictor_p = dh_wrong$quantile_predictor_p,
    dharma_wrong_categorical_design_p = dh_wrong$categorical_design_p, dharma_wrong_valid_tests = dh_wrong$valid_tests,
    dharma_wrong_tests_used = dh_wrong$tests_used,
    dharma_correct_uniformity_p = dh_correct$uniformity_p, dharma_correct_dispersion_p = dh_correct$dispersion_p,
    dharma_correct_quantile_fitted_p = dh_correct$quantile_fitted_p, dharma_correct_quantile_predictor_p = dh_correct$quantile_predictor_p,
    dharma_correct_categorical_design_p = dh_correct$categorical_design_p, dharma_correct_valid_tests = dh_correct$valid_tests,
    dharma_correct_tests_used = dh_correct$tests_used,
    pregibon_wrong_p = preg_wrong, pregibon_correct_p = preg_correct,
    aic_true_link_interaction = aic_true, aic_fitted_link_interaction = aic_fitted,
    aic_favors_true_link = if (both_aic) aic_true <= aic_fitted else NA,
    aic_favors_fitted_link = if (both_aic) aic_fitted < aic_true else NA,
    aic_fitted_minus_true = if (both_aic) aic_fitted - aic_true else NA_real_,
    target_fit_converged = if (inherits(fit_correct, "merMod")) length(fit_correct@optinfo$conv$lme4$messages) == 0 && !isSingular(fit_correct, tol = 1e-4) else NA,
    target_fit_gradient_max = NA_real_, target_fit_hessian_min_eigen = NA_real_)
}

####################################################
# Monte Carlo simulation
####################################################

cat("\nRunning B =", B, "replications per scenario on", n_cores, "cores\n")
cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, {library(glmmTMB); library(lme4); library(psyphy); library(DHARMa)}))
clusterExport(cl, c("try_fit", "pval", "dharma_checks", "pregibon_p", "seed", "diag_scenarios", "dharma_n_sim", "min_unique_for_quantile"))
results <- list()
for (k in 1:length(diag_scenarios)) {
  cat("Scenario:", diag_scenarios[[k]]$scenario, "\n")
  results[[k]] <- do.call(rbind, parLapply(cl, 1:B, sim_one, k = k))
}
stopCluster(cl)
simulation_results <- do.call(rbind, results)

####################################################
# Summaries
####################################################

# rate of flagged replications (p < alpha, or TRUE for logical AIC flags) with clipped Wilson 95% CI
rate_summary <- function(flag) {
  flag <- flag[!is.na(flag)]
  n <- length(flag)
  x <- sum(flag)
  z <- qnorm(0.975)
  p <- x / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  data.frame(n_successful_fits = n, n_significant = x, rate = if (n > 0) p else NA,
             ci_low = if (n > 0) max(0, center - half) else NA, ci_high = if (n > 0) min(1, center + half) else NA)
}

quantities <- data.frame(
  column = c("interaction_p", "dharma_uniformity_p", "dharma_dispersion_p", "dharma_quantile_fitted_p", "dharma_quantile_predictor_p",
             "dharma_categorical_design_p", "pregibon_link_test_p", "aic_favors_true_link", "aic_favors_fitted_link"),
  quantity = c("Pseudo-interaction detection", "DHARMa uniformity", "DHARMa dispersion", "DHARMa residual quantiles over fitted values",
               "DHARMa residual quantiles over focal predictor", "DHARMa residual distribution across design cells",
               "Pregibon-style added-term link check, secondary", "AIC same interaction formula favors true link",
               "AIC same interaction formula favors fitted link"),
  diagnostic_family = c("Interaction test", "DHARMa", "DHARMa", "DHARMa", "DHARMa", "DHARMa", "Pregibon-style link check",
                        "AIC link comparison", "AIC link comparison"))
calibration_diagnostics <- data.frame(
  diagnostic = c("DHARMa uniformity", "DHARMa dispersion", "DHARMa residual quantiles over fitted values",
                 "DHARMa residual quantiles over focal predictor", "DHARMa residual distribution across design cells",
                 "Pregibon-style added-term link check"),
  suffix = c("uniformity_p", "dispersion_p", "quantile_fitted_p", "quantile_predictor_p", "categorical_design_p", NA))
dharma_columns <- c("dharma_uniformity_p", "dharma_dispersion_p", "dharma_quantile_fitted_p", "dharma_quantile_predictor_p", "dharma_categorical_design_p")

long_summary <- calibration_summary <- scenario_summary <- NULL
for (k in 1:length(diag_scenarios)) {
  scn <- diag_scenarios[[k]]
  dat <- simulation_results[simulation_results$scenario == scn$scenario, ]
  info <- data.frame(scenario = scn$scenario, paper_anchor = scn$paper_anchor, outcome_family = scn$outcome_family,
    true_link_function = scn$true_link_function, fitted_link_function = scn$fitted_link_function,
    true_model_label = scn$true_model_label, fitted_model_label = scn$fitted_model_label,
    quantitative_description = scenario_descriptions[[k]], variability_summary = scn$variability_summary)

  # long summary: one row per quantity
  for (q in 1:nrow(quantities)) {
    flag <- dat[[quantities$column[q]]]
    if (is.numeric(flag)) flag <- flag < alpha
    long_summary <- rbind(long_summary, data.frame(info, quantity = quantities$quantity[q],
      diagnostic_family = quantities$diagnostic_family[q], rate_summary(flag)))
  }

  # calibration: each applicable check on the correct-link and on the wrong-link model
  diagnostics <- calibration_diagnostics[calibration_diagnostics$suffix %in% c(NA, "uniformity_p", "dispersion_p", "quantile_fitted_p",
    if (scn$focal_dharma == "categorical_design") "categorical_design_p" else "quantile_predictor_p"), ]
  for (spec in c("correct", "wrong")) {
    for (j in 1:nrow(diagnostics)) {
      column <- if (is.na(diagnostics$suffix[j])) paste0("pregibon_", spec, "_p") else paste0("dharma_", spec, "_", diagnostics$suffix[j])
      sm <- rate_summary(dat[[column]] < alpha)
      calibration_summary <- rbind(calibration_summary, data.frame(scenario = scn$scenario, diagnostic = diagnostics$diagnostic[j],
        model_specification = if (spec == "correct") "Correct link" else "Wrong link", n_attempted = nrow(dat),
        n_successful = sm$n_successful_fits, flagging_rate = sm$rate, ci_low = sm$ci_low, ci_high = sm$ci_high))
    }
  }

  # scenario summary: range of DHARMa detection rates over the checks actually computed
  interaction <- rate_summary(dat$interaction_p < alpha)
  pregibon <- rate_summary(dat$pregibon_link_test_p < alpha)
  aic_true <- rate_summary(dat$aic_favors_true_link)
  aic_fitted <- rate_summary(dat$aic_favors_fitted_link)
  dharma_rates <- sapply(dharma_columns, function(column) rate_summary(dat[[column]] < alpha)$rate)
  used <- dharma_columns[!is.na(dharma_rates)]
  aic_diff <- dat$aic_fitted_minus_true
  scenario_summary <- rbind(scenario_summary, data.frame(info,
    n_replications = interaction$n_successful_fits,
    pseudo_interaction_detection_rate = interaction$rate,
    pseudo_interaction_detection_ci_low = interaction$ci_low, pseudo_interaction_detection_ci_high = interaction$ci_high,
    dharma_detection_rate_min = if (length(used)) min(dharma_rates[used]) else NA,
    dharma_detection_rate_max = if (length(used)) max(dharma_rates[used]) else NA,
    dharma_checks_used = paste(used, collapse = "; "), dharma_n_checks_used = length(used),
    pregibon_detection_rate = pregibon$rate, pregibon_detection_ci_low = pregibon$ci_low, pregibon_detection_ci_high = pregibon$ci_high,
    aic_favors_target = aic_true$rate, aic_favors_true_link = aic_true$rate, aic_favors_fitted_link = aic_fitted$rate,
    n_aic_pairs = sum(is.finite(aic_diff)),
    median_aic_fitted_minus_true = median(aic_diff, na.rm = TRUE),
    q10_aic_fitted_minus_true = unname(quantile(aic_diff, 0.10, na.rm = TRUE)),
    q25_aic_fitted_minus_true = unname(quantile(aic_diff, 0.25, na.rm = TRUE)),
    q75_aic_fitted_minus_true = unname(quantile(aic_diff, 0.75, na.rm = TRUE)),
    q90_aic_fitted_minus_true = unname(quantile(aic_diff, 0.90, na.rm = TRUE)),
    aic_difference_within_2 = mean(abs(aic_diff) < 2, na.rm = TRUE),
    target_fit_converged_rate = mean(dat$target_fit_converged, na.rm = TRUE),
    target_fit_gradient_max = NA_real_,
    # aliases with the older column names, still read by the manuscript
    false_positive_interaction = interaction$rate,
    false_positive_interaction_ci_low = interaction$ci_low, false_positive_interaction_ci_high = interaction$ci_high,
    dharma_detection_min = if (length(used)) min(dharma_rates[used]) else NA,
    dharma_detection_max = if (length(used)) max(dharma_rates[used]) else NA,
    pregibon_detection = pregibon$rate))
}

write.csv(simulation_results, "outputs/diagnostic-simulation-replications.csv", row.names = FALSE)
write.csv(long_summary, "tables/simulation-summary-diagnostic-worked-example.csv", row.names = FALSE)
write.csv(scenario_summary, "tables/simulation-summary-diagnostic-scenarios.csv", row.names = FALSE)
write.csv(scenario_summary, "tables/diagnostic-scenarios.csv", row.names = FALSE)
write.csv(calibration_summary, "tables/diagnostic-calibration.csv", row.names = FALSE)

print(scenario_summary[, c("scenario", "pseudo_interaction_detection_rate", "dharma_detection_rate_min",
                           "dharma_detection_rate_max", "pregibon_detection_rate", "aic_favors_true_link")])
print(calibration_summary)

####################################################
# One representative DHARMa plot (inspection only, not a main-text figure)
####################################################

group_num <- rbinom(s2$N, 1, 0.5)
age <- runif(s2$N, s2$age_range[1], s2$age_range[2])
age_c <- age - s2$age_center
u <- rnorm(s2$N, 0, sqrt(s2$target_icc * (pi^2 / 3) / (1 - s2$target_icc)))
eta <- s2$beta_intercept + s2$beta_age * age_c + s2$beta_group * group_num + s2$beta_age_group * age_c * group_num + u
example_data <- data.frame(id = factor(rep(1:s2$N, each = s2$k_trials)), age_c = rep(age_c, each = s2$k_trials),
  group = factor(rep(group_num, each = s2$k_trials), levels = c(0, 1), labels = c("Group 0", "Group 1")))
example_data$correct <- rbinom(nrow(example_data), 1, s2$chance + (1 - s2$chance) * plogis(rep(eta, each = s2$k_trials)))
example_fit <- glmer(correct ~ age_c * group + (1 | id), data = example_data, family = binomial("logit"))
example_sim <- simulateResiduals(fittedModel = example_fit, n = dharma_n_sim, plot = FALSE, seed = 123)

pdf("outputs/inspection/dharma-diagnostic-example.pdf", width = 7.2, height = 6.5)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plotQQunif(example_sim, main = "Overall uniformity")
plotResiduals(example_sim, quantreg = TRUE, main = "Residuals over fitted values")
plotResiduals(example_sim, form = example_data$age_c, quantreg = TRUE, main = "Residuals over age")
plotResiduals(example_sim, form = example_data$group, main = "Residuals by group")
dev.off()

settings <- list(B = B, n_cores = n_cores, seed = seed, alpha = alpha, dharma_n_sim = dharma_n_sim,
                 min_unique_for_quantile = min_unique_for_quantile)
saveRDS(list(settings = settings, diag_scenarios = diag_scenarios, scenario_table = scenario_table,
             simulation_results = simulation_results, long_summary = long_summary, scenario_summary = scenario_summary,
             calibration_summary = calibration_summary, example_data = example_data),
        "outputs/diagnostic-worked-example.rds")
cat("\nDone.\n")
