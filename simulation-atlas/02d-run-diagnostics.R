# Atlas, diagnostics: AIC, DHARMa and Pregibon-style checks on the wrong-link model,
# over the rows of the diagnostic grid (forced choice and within family; sum-score
# diagnostics are not defined). Note: these definitions differ slightly from
# scripts/04-diagnostic-worked-example.R (e.g. Pregibon for the binary GLMM uses
# eta_hat including random effects); they are kept as they are.
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

sim_one <- function(replication, scenario, dharma_n_sim) {
  fc <- scenario$family == "forced_choice"
  replication_seed <- as.integer((20260807 + (if (fc) 1000000 else 3000000) + 4000000 +
    as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  correct_problem <- TRUE
  aic_correct <- aic_wrong <- NA_real_

  if (fc) {
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
    correct_fit <- try(glmer(correct ~ age_c * group + (1 | id), data = d, family = binomial(mafc.logit(2))), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- AIC(correct_fit)
      correct_problem <- length(correct_fit@optinfo$conv$lme4$messages) > 0 || isSingular(correct_fit, tol = 1e-4)
    }
  } else {
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
  }

  interaction_p <- interaction_coef <- NA_real_
  wrong_problem <- TRUE
  problem_message <- ""
  dh <- c(uniformity = NA_real_, dispersion = NA_real_, quantile_fitted = NA_real_, quantile_predictor = NA_real_, categorical_design = NA_real_)
  pregibon_p <- NA_real_
  if (inherits(wrong_fit, "try-error")) {
    problem_message <- as.character(wrong_fit)
  } else {
    aic_wrong <- AIC(wrong_fit)
    if (fc) {
      tab <- summary(wrong_fit)$coefficients
      term <- "age_c:groupGroup 1"
      wrong_problem <- length(wrong_fit@optinfo$conv$lme4$messages) > 0 || isSingular(wrong_fit, tol = 1e-4)
    } else {
      tab <- summary(wrong_fit)$coefficients$cond
      term <- "groupGroup 1:conditionCondition 1"
      wrong_problem <- !isTRUE(wrong_fit$sdr$pdHess) || wrong_fit$fit$convergence != 0
    }
    if (term %in% rownames(tab)) {
      interaction_p <- tab[term, 4]
      interaction_coef <- tab[term, 1]
    }

    if (!is.na(dharma_n_sim)) {
      sim <- try(simulateResiduals(fittedModel = wrong_fit, n = dharma_n_sim, plot = FALSE, seed = NULL), silent = TRUE)
      if (!inherits(sim, "try-error")) {
        dh["uniformity"] <- pval(try(testUniformity(sim, plot = FALSE), silent = TRUE))
        dh["dispersion"] <- pval(try(testDispersion(sim, plot = FALSE), silent = TRUE))
        if (fc) {
          fitted <- as.numeric(predict(wrong_fit, type = "response", re.form = NA))
          if (length(unique(round(fitted[is.finite(fitted)], 10))) >= 8) dh["quantile_fitted"] <- pval(try(testQuantiles(sim, plot = FALSE), silent = TRUE))
          if (length(unique(round(d$age_c, 10))) >= 8) dh["quantile_predictor"] <- pval(try(testQuantiles(sim, predictor = d$age_c, plot = FALSE), silent = TRUE))
        } else {
          # scaled residuals across the four design cells
          design_cell <- interaction(d$group, d$condition, drop = TRUE)
          residual <- sim$scaledResiduals
          dh["categorical_design"] <- pval(try(kruskal.test(residual ~ design_cell), silent = TRUE))
        }
      }
    }

    # Pregibon-style check: add the squared linear predictor to the original formula
    d$eta_hat_sq <- as.numeric(if (fc) predict(wrong_fit, type = "link", re.form = NA) else predict(wrong_fit, type = "link"))^2
    if (fc) {
      added_fit <- try(glmer(correct ~ age_c * group + eta_hat_sq + (1 | id), data = d, family = binomial("logit")), silent = TRUE)
      if (!inherits(added_fit, "try-error")) tab <- summary(added_fit)$coefficients
    } else {
      added_fit <- try(glmmTMB(y ~ group * condition + (1 | id) + eta_hat_sq, data = d, family = binomial("logit")), silent = TRUE)
      if (!inherits(added_fit, "try-error")) tab <- summary(added_fit)$coefficients$cond
    }
    if (!inherits(added_fit, "try-error") && "eta_hat_sq" %in% rownames(tab)) pregibon_p <- tab["eta_hat_sq", 4]
  }

  data.frame(scenario_id = scenario$scenario_id, family = scenario$family, replication = replication, replication_seed = replication_seed,
    interaction_p = interaction_p, interaction_coef = interaction_coef,
    fit_success = is.finite(interaction_p) && !wrong_problem, convergence_problem = wrong_problem || correct_problem,
    problem_message = problem_message, aic_generating = aic_correct, aic_wrong = aic_wrong,
    aic_favors_generating = if (is.finite(aic_correct) && is.finite(aic_wrong)) aic_correct <= aic_wrong else NA,
    dharma_uniformity_p = dh[["uniformity"]], dharma_dispersion_p = dh[["dispersion"]], dharma_quantile_fitted_p = dh[["quantile_fitted"]],
    dharma_quantile_predictor_p = dh[["quantile_predictor"]], dharma_categorical_design_p = dh[["categorical_design"]],
    dharma_computed = !is.na(dharma_n_sim), pregibon_p = pregibon_p)
}

cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, {library(glmmTMB); library(lme4); library(psyphy)}))
if (run_dharma) invisible(clusterEvalQ(cl, library(DHARMa)))
clusterExport(cl, "pval")
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
