# Atlas, diagnostics: AIC, DHARMa and Pregibon-style checks on the wrong-link model
# (detection) and on the correct-link model (calibration), over the rows of the
# diagnostic grid (forced choice, within family 2 x 2, Poisson count, binary
# continuous predictor and Gamma; sum-score diagnostics are not defined).
# Definitions follow scripts/04-diagnostic-worked-example.R.
# Run from the repository root after 01-build-scenario-grid.R.
# Environment variables: ATLAS_MODE (full/smoke), N_SIM, N_CORES, ATLAS_OVERWRITE,
# ATLAS_RUN_DHARMA (TRUE/FALSE), DHARMA_N_SIM.

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(parallel)
mode <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (mode == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
overwrite <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
run_dharma <- as.logical(Sys.getenv("ATLAS_RUN_DHARMA", "FALSE"))
dharma_n_sim <- if (run_dharma) as.integer(Sys.getenv("DHARMA_N_SIM", if (mode == "smoke") "25" else "250")) else NA_integer_
kind <- if (run_dharma) "diagnostic" else "diagnostic-nodharma" # raw file prefix
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)

grid <- read.csv("simulation-atlas/data/diagnostic-grid.csv")
scenarios <- grid[grid$family != "sum_scores", ]
if (mode == "smoke") scenarios <- scenarios[scenarios$diagnostic_paper_anchor, ]

pval <- function(test) if (inherits(test, "try-error") || length(test$p.value) != 1) NA else test$p.value
# DHARMa testCategorical with defaults; flagged if any within-cell uniformity test
# (adjusted by DHARMa) or the Levene test is significant. testCategorical calls
# mtext() even with plot = FALSE (DHARMa 0.4.7), so it needs an open plot.
pval_categorical <- function(sim, cell) {
  pdf(NULL); on.exit(dev.off())
  plot.new()
  test <- try(testCategorical(sim, catPred = cell, plot = FALSE), silent = TRUE)
  if (inherits(test, "try-error")) NA else min(test$uniformity$p.value.cor, test$homogeneity$`Pr(>F)`[1])
}

# DHARMa checks on one fitted model: quantile tests over the focal continuous
# predictor (column `focal` of d), or testCategorical across the four design
# cells for the 2 x 2 within-family design (focal = "design_cells")
dharma_checks <- function(fit, d, focal, dharma_n_sim) {
  dh <- c(uniformity = NA_real_, dispersion = NA_real_, quantile_fitted = NA_real_, quantile_predictor = NA_real_, categorical_design = NA_real_)
  if (inherits(fit, "try-error") || is.na(dharma_n_sim)) return(dh)
  sim <- try(simulateResiduals(fittedModel = fit, n = dharma_n_sim, plot = FALSE, seed = NULL), silent = TRUE)
  if (inherits(sim, "try-error")) return(dh)
  dh["uniformity"] <- pval(try(testUniformity(sim, plot = FALSE), silent = TRUE))
  dh["dispersion"] <- pval(try(testDispersion(sim, plot = FALSE), silent = TRUE))
  fitted <- as.numeric(predict(fit, type = "response"))
  if (length(unique(round(fitted[is.finite(fitted)], 10))) >= 8) dh["quantile_fitted"] <- pval(try(testQuantiles(sim, plot = FALSE), silent = TRUE))
  if (focal == "design_cells") {
    dh["categorical_design"] <- pval_categorical(sim, interaction(d$group, d$condition, drop = TRUE))
  } else {
    if (length(unique(round(d[[focal]], 10))) >= 8) dh["quantile_predictor"] <- pval(try(testQuantiles(sim, predictor = d[[focal]], plot = FALSE), silent = TRUE))
  }
  dh
}

# Pregibon-style check: refit the model being diagnosed (same family, link, random
# intercept and, for glmer, nAGQ) with the squared linear predictor added.
# For mixed models eta_hat uses fixed effects only (squared empirical Bayes random
# effects as a covariate are badly anti-conservative under the correct link).
# Not applicable (NA) when eta_hat^2 does not increase the rank of the fixed-effects
# design `fixed`, as in the saturated 2 x 2 interaction model.
pregibon_p <- function(fit, d, fixed) {
  if (inherits(fit, "try-error")) return(NA_real_)
  eta_hat <- if (inherits(fit, "glm")) predict(fit, type = "link") else predict(fit, type = "link", re.form = NA)
  d$eta_hat_sq <- as.numeric(eta_hat)^2
  if (qr(model.matrix(update(fixed, . ~ . + eta_hat_sq), data = d))$rank <= qr(model.matrix(fixed, data = d))$rank) return(NA_real_)
  form <- update(formula(fit), . ~ . + eta_hat_sq)
  if (inherits(fit, "merMod")) {
    added_fit <- try(glmer(form, data = d, family = family(fit), nAGQ = fit@devcomp$dims[["nAGQ"]]), silent = TRUE)
    if (inherits(added_fit, "try-error")) return(NA_real_)
    tab <- summary(added_fit)$coefficients
  } else if (inherits(fit, "glmmTMB")) {
    added_fit <- try(glmmTMB(form, data = d, family = family(fit)), silent = TRUE)
    if (inherits(added_fit, "try-error")) return(NA_real_)
    tab <- summary(added_fit)$coefficients$cond
  } else {
    # glm() reads start by position and model.matrix() puts eta_hat_sq before the
    # interaction, so align the current estimates by name (0 for eta_hat_sq)
    X <- model.matrix(form, data = d)
    start <- setNames(rep(0, ncol(X)), colnames(X))
    shared <- intersect(names(coef(fit)), names(start))
    start[shared] <- coef(fit)[shared]
    added_fit <- try(glm(form, data = d, family = family(fit), start = start), silent = TRUE)
    if (inherits(added_fit, "try-error") || !added_fit$converged) return(NA_real_)
    tab <- coef(summary(added_fit))
  }
  if ("eta_hat_sq" %in% rownames(tab)) tab["eta_hat_sq", 4] else NA_real_
}

sim_one <- function(replication, scenario, dharma_n_sim) {
  family_offset <- c(forced_choice = 1000000, within_family = 3000000, count = 5000000, binary_continuous = 6000000, gamma = 7000000)[[scenario$family]]
  replication_seed <- as.integer((20260807 + family_offset + 4000000 +
    as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  correct_problem <- TRUE
  aic_correct <- aic_wrong <- NA_real_

  if (scenario$family == "forced_choice") {
    # chance-corrected logit generated, standard logit fitted (as in the manuscript)
    N <- scenario$N
    k <- scenario$k_trials
    group_num <- rbinom(N, 1, 0.5)
    age <- runif(N, scenario$age_min, scenario$age_max)
    age_c <- age - scenario$age_center
    u <- rnorm(N, 0, sqrt(scenario$target_icc * (pi^2 / 3) / (1 - scenario$target_icc)))
    eta <- scenario$beta_intercept + scenario$beta_age * age_c + scenario$beta_group * group_num + scenario$beta_age_group * age_c * group_num + u
    d <- data.frame(id = factor(rep(1:N, each = k)), age_c = rep(age_c, each = k),
      group = factor(rep(group_num, each = k), levels = c(0, 1), labels = c("Group 0", "Group 1")))
    d$correct <- rbinom(nrow(d), 1, scenario$chance + (1 - scenario$chance) * plogis(rep(eta, each = k)))
    wrong_fit <- try(glmer(correct ~ age_c * group + (1 | id), data = d, family = binomial("logit")), silent = TRUE)
    # nAGQ = 10 for the chance-corrected model, as in the manuscript
    correct_fit <- try(glmer(correct ~ age_c * group + (1 | id), data = d, family = binomial(mafc.logit(as.integer(round(1 / scenario$chance)))), nAGQ = 10), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- AIC(correct_fit)
      correct_problem <- length(correct_fit@optinfo$conv$lme4$messages) > 0 || isSingular(correct_fit, tol = 1e-4)
    }
    term <- "age_c:groupGroup 1"
    focal <- "age_c"
    fixed <- correct ~ age_c * group
  }
  if (scenario$family == "within_family") {
    # probit generated, logit fitted; individual binary trials as in the manuscript diagnostic example
    n <- scenario$n_subjects
    k <- scenario$k_trials
    d <- data.frame(id = factor(rep(1:n, each = 2 * k)), group_num = rep(rep(c(0, 1), each = n / 2), each = 2 * k),
      condition_num = rep(rep(c(0, 1), each = k), times = n))
    residual_variance <- if (scenario$generating_link == "logit") pi^2 / 3 else 1
    u <- rnorm(n, sd = sqrt(scenario$target_icc * residual_variance / (1 - scenario$target_icc)))
    eta <- scenario$beta_intercept + scenario$beta_group * d$group_num + scenario$beta_condition * d$condition_num +
      scenario$beta_group_condition * d$group_num * d$condition_num + u[as.integer(d$id)]
    d$y <- rbinom(nrow(d), 1, if (scenario$generating_link == "logit") plogis(eta) else pnorm(eta))
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
    wrong_fit <- try(glmmTMB(y ~ group * condition + (1 | id), data = d, family = binomial("logit")), silent = TRUE)
    correct_fit <- try(glmmTMB(y ~ group * condition + (1 | id), data = d, family = binomial("probit")), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- AIC(correct_fit)
      correct_problem <- !isTRUE(correct_fit$sdr$pdHess) || correct_fit$fit$convergence != 0
    }
    term <- "groupGroup 1:conditionCondition 1"
    focal <- "design_cells"
    fixed <- y ~ group * condition
  }
  if (scenario$family == "count") {
    # Poisson log generated, identity link fitted (as in the manuscript)
    N <- scenario$N
    group_num <- rbinom(N, 1, 0.5)
    age <- runif(N, scenario$age_min, scenario$age_max)
    age_offset <- age - scenario$age_origin
    mu <- exp(scenario$beta_intercept + scenario$beta_age * age_offset + scenario$beta_group * group_num + scenario$beta_age_group * age_offset * group_num)
    d <- data.frame(age_offset = age_offset, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
                    y = rpois(N, mu))
    # identity-link Poisson needs positive fitted means: start from lm, shifted up if needed
    start <- coef(lm(y ~ age_offset * group, data = d))
    pred <- as.vector(model.matrix(y ~ age_offset * group, data = d) %*% start)
    if (any(!is.finite(pred))) start <- NULL else if (min(pred) <= 0) start[1] <- start[1] + abs(min(pred)) + 1
    wrong_fit <- try(glm(y ~ age_offset * group, family = poisson(link = "identity"), data = d, start = start), silent = TRUE)
    correct_fit <- try(glm(y ~ age_offset * group, family = poisson("log"), data = d), silent = TRUE)
    term <- "age_offset:groupGroup 1"
    focal <- "age_offset"
    fixed <- y ~ age_offset * group
  }
  if (scenario$family == "binary_continuous") {
    # probit generated, logit fitted; continuous counterpart of the 2 x 2 design (as in the manuscript)
    n <- scenario$n_subjects
    k <- scenario$trials_per_subject
    d <- data.frame(id = factor(rep(1:n, each = k)), group_num = rep(rep(c(0, 1), each = n / 2), each = k),
      x = runif(n * k, scenario$x_min, scenario$x_max))
    u <- rnorm(n, 0, sqrt(scenario$target_icc / (1 - scenario$target_icc)))
    eta <- scenario$beta_intercept + scenario$beta_group * d$group_num + scenario$beta_x * d$x + scenario$beta_x_group * d$group_num * d$x + u[as.integer(d$id)]
    d$y <- rbinom(nrow(d), 1, pnorm(eta))
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    wrong_fit <- try(glmmTMB(y ~ group * x + (1 | id), data = d, family = binomial("logit")), silent = TRUE)
    correct_fit <- try(glmmTMB(y ~ group * x + (1 | id), data = d, family = binomial("probit")), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- AIC(correct_fit)
      correct_problem <- !isTRUE(correct_fit$sdr$pdHess) || correct_fit$fit$convergence != 0
    }
    term <- "groupGroup 1:x"
    focal <- "x"
    fixed <- y ~ group * x
  }
  if (scenario$family == "gamma") {
    # Gamma log generated, inverse link fitted (as in the manuscript)
    N <- scenario$N
    group_num <- rbinom(N, 1, 0.5)
    x <- runif(N, scenario$x_min, scenario$x_max)
    mu <- exp(scenario$beta_intercept + scenario$beta_x * x + scenario$beta_group * group_num + scenario$beta_x_group * x * group_num)
    d <- data.frame(x = x, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
                    y = rgamma(N, shape = scenario$gamma_shape, scale = mu / scenario$gamma_shape))
    # inverse-link Gamma needs positive fitted 1/means: start from lm on 1/y, shifted up if needed
    start <- coef(lm(I(1 / y) ~ x * group, data = d))
    pred <- as.vector(model.matrix(y ~ x * group, data = d) %*% start)
    if (any(!is.finite(pred))) start <- NULL else if (min(pred) <= 0) start[1] <- start[1] + abs(min(pred)) + 1e-4
    wrong_fit <- try(glm(y ~ x * group, family = Gamma(link = "inverse"), data = d, start = start), silent = TRUE)
    correct_fit <- try(glm(y ~ x * group, family = Gamma("log"), data = d), silent = TRUE)
    term <- "x:groupGroup 1"
    focal <- "x"
    fixed <- y ~ x * group
  }
  # non-converged GLMs are treated as failed fits, as in the manuscript
  if (inherits(wrong_fit, "glm") && !wrong_fit$converged) wrong_fit <- try(stop("glm did not converge"), silent = TRUE)
  if (inherits(correct_fit, "glm") && !correct_fit$converged) correct_fit <- try(stop("glm did not converge"), silent = TRUE)
  if (inherits(correct_fit, "glm")) {
    aic_correct <- AIC(correct_fit)
    correct_problem <- FALSE
  }

  interaction_p <- interaction_coef <- NA_real_
  wrong_problem <- TRUE
  problem_message <- ""
  if (inherits(wrong_fit, "try-error")) {
    problem_message <- as.character(wrong_fit)
  } else {
    aic_wrong <- AIC(wrong_fit)
    if (inherits(wrong_fit, "merMod")) {
      tab <- summary(wrong_fit)$coefficients
      wrong_problem <- length(wrong_fit@optinfo$conv$lme4$messages) > 0 || isSingular(wrong_fit, tol = 1e-4)
    } else if (inherits(wrong_fit, "glmmTMB")) {
      tab <- summary(wrong_fit)$coefficients$cond
      wrong_problem <- !isTRUE(wrong_fit$sdr$pdHess) || wrong_fit$fit$convergence != 0
    } else {
      tab <- summary(wrong_fit)$coefficients
      wrong_problem <- FALSE # converged GLM
    }
    if (term %in% rownames(tab)) {
      interaction_p <- tab[term, 4]
      interaction_coef <- tab[term, 1]
    }
  }

  # the same checks on the wrong-link model (detection) and on the correct-link model (calibration)
  dh <- dharma_checks(wrong_fit, d, focal, dharma_n_sim)
  dh_correct <- dharma_checks(correct_fit, d, focal, dharma_n_sim)
  preg <- tryCatch(pregibon_p(wrong_fit, d, fixed), error = function(e) NA_real_)
  preg_correct <- tryCatch(pregibon_p(correct_fit, d, fixed), error = function(e) NA_real_)

  # the unprefixed dharma_* and pregibon_p columns refer to the wrong-link model
  data.frame(scenario_id = scenario$scenario_id, family = scenario$family, replication = replication, replication_seed = replication_seed,
    interaction_p = interaction_p, interaction_coef = interaction_coef,
    fit_success = is.finite(interaction_p) && !wrong_problem, convergence_problem = wrong_problem || correct_problem,
    problem_message = problem_message, aic_generating = aic_correct, aic_wrong = aic_wrong,
    aic_favors_generating = if (is.finite(aic_correct) && is.finite(aic_wrong)) aic_correct <= aic_wrong else NA,
    dharma_uniformity_p = dh[["uniformity"]], dharma_dispersion_p = dh[["dispersion"]], dharma_quantile_fitted_p = dh[["quantile_fitted"]],
    dharma_quantile_predictor_p = dh[["quantile_predictor"]], dharma_categorical_design_p = dh[["categorical_design"]],
    dharma_correct_uniformity_p = dh_correct[["uniformity"]], dharma_correct_dispersion_p = dh_correct[["dispersion"]],
    dharma_correct_quantile_fitted_p = dh_correct[["quantile_fitted"]], dharma_correct_quantile_predictor_p = dh_correct[["quantile_predictor"]],
    dharma_correct_categorical_design_p = dh_correct[["categorical_design"]],
    dharma_computed = !is.na(dharma_n_sim), pregibon_p = preg, pregibon_correct_p = preg_correct)
}

cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, {library(glmmTMB); library(lme4); library(psyphy)}))
if (run_dharma) invisible(clusterEvalQ(cl, library(DHARMa)))
clusterExport(cl, c("pval", "pval_categorical", "dharma_checks", "pregibon_p"))
for (i in 1:nrow(scenarios)) {
  scenario <- scenarios[i, ]
  output_file <- sprintf("simulation-atlas/raw/%s-%s-%s-B%d.rds", kind, scenario$scenario_id, mode, B)
  if (file.exists(output_file) && !overwrite) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")
  result <- do.call(rbind, parLapply(cl, 1:B, sim_one, scenario = scenario, dharma_n_sim = dharma_n_sim))
  result$B_requested <- B
  result$run_type <- mode
  result$dharma_n_sim <- dharma_n_sim
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
stopCluster(cl)
