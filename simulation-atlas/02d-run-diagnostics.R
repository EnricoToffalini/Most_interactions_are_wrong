# Targeted diagnostic sensitivity analysis; definitions preserved.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(glmmTMB)
library(lme4)
library(psyphy)
# Run from the repository root after 01-build-scenario-grid.R.
# Replications are parallel; scenarios are visited in declared grid order.
MODE <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (MODE == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK",
      max(1, parallel::detectCores() - 1))))
OVERWRITE <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)
grid <- utils::read.csv("simulation-atlas/data/diagnostic-grid.csv", stringsAsFactors = FALSE)
RUN_DHARMA <- as.logical(Sys.getenv("ATLAS_RUN_DHARMA", "FALSE"))
dharma_n_sim <- if (RUN_DHARMA) as.integer(Sys.getenv("DHARMA_N_SIM",
    if (MODE == "smoke") "25" else "250")) else NA_integer_
kind <- if (RUN_DHARMA) "diagnostic" else "diagnostic-nodharma"
scenarios <- grid[grid$family != "sum_scores", ]
if (MODE == "smoke") scenarios <- scenarios[scenarios$diagnostic_paper_anchor, ]

if (RUN_DHARMA) library(DHARMa)
run_one_replication <- function(replication, scenario, dharma_n_sim) {
  replication_seed <- as.integer((20260807 +
        ifelse(scenario$family == "forced_choice", 1000000, 3000000) + 4000000 +
        as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  settings <- as.list(scenario)
  correct_problem <- TRUE
  aic_correct <- aic_wrong <- NA_real_
  if (scenario$family == "forced_choice") {
    group_num <- stats::rbinom(settings$N, 1, 0.5)
    age <- stats::runif(settings$N, scenario$age_min, scenario$age_max)
    age_c <- age - settings$age_center
    sigma_u <- sqrt(settings$target_icc * (pi^2 / 3) / (1 - settings$target_icc))
    u <- stats::rnorm(settings$N, mean = 0, sd = sigma_u)
    eta <- scenario$beta_intercept + settings$beta_age * age_c +
      settings$beta_group * group_num +
      settings$beta_age_group * age_c * group_num + u
    d <- data.frame(
      id = factor(rep(seq_len(settings$N), each = settings$k_trials)),
      age = rep(age, each = settings$k_trials),
      age_c = rep(age_c, each = settings$k_trials),
      group = factor(
        rep(group_num, each = settings$k_trials),
        levels = c(0, 1), labels = c("Group 0", "Group 1")
      ),
      stringsAsFactors = FALSE
    )
    d$eta <- rep(eta, each = settings$k_trials)
    d$p <- settings$chance + (1 - settings$chance) * stats::plogis(d$eta)
    d$correct <- stats::rbinom(nrow(d), size = 1, prob = d$p)

    wrong_fit <- try(lme4::glmer(
      correct ~ age_c * group + (1 | id),
      data = d, family = stats::binomial("logit")
    ), silent = TRUE)
    correct_fit <- try(lme4::glmer(
      correct ~ age_c * group + (1 | id),
      data = d, family = stats::binomial(psyphy::mafc.logit(2))
    ), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- stats::AIC(correct_fit)
      correct_messages <- correct_fit@optinfo$conv$lme4$messages
      correct_problem <- length(correct_messages) > 0 ||
        lme4::isSingular(correct_fit, tol = 1e-4)
    }
  } else {
    # Individual binary trials, as in the manuscript diagnostic example.
    id <- rep(seq_len(scenario$n_subjects), each = 2 * scenario$k_trials)
    group_by_subject <- rep(c(0, 1), each = scenario$n_subjects / 2)
    d <- data.frame(id = factor(id),
      group_num = rep(group_by_subject, each = 2 * scenario$k_trials),
      condition_num = rep(rep(c(0, 1), each = scenario$k_trials), times = scenario$n_subjects))
    residual_variance <- if (scenario$generating_link == "logit") pi^2 / 3 else 1
    u_sd <- sqrt(scenario$target_icc * residual_variance / (1 - scenario$target_icc))
    random_effect <- stats::rnorm(scenario$n_subjects, sd = u_sd)
    eta <- scenario$beta_intercept + scenario$beta_group * d$group_num +
      scenario$beta_condition * d$condition_num + scenario$beta_group_condition * d$group_num * d$condition_num +
      random_effect[as.integer(d$id)]
    probability <- if (scenario$generating_link == "logit") stats::plogis(eta) else stats::pnorm(eta)
    d$y <- stats::rbinom(nrow(d), 1, probability)
    d$group <- factor(d$group_num, levels = c(0, 1), labels = c("Group 0", "Group 1"))
    d$condition <- factor(d$condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1"))
    wrong_fit <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id),
        data = d, family = stats::binomial("logit")), silent = TRUE)
    correct_fit <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id),
        data = d, family = stats::binomial("probit")), silent = TRUE)
    if (!inherits(correct_fit, "try-error")) {
      aic_correct <- stats::AIC(correct_fit)
      correct_problem <- !isTRUE(correct_fit$sdr$pdHess) || correct_fit$fit$convergence != 0
    }
  }
  interaction_p <- interaction_coef <- NA_real_
  wrong_problem <- TRUE
  problem_message <- ""
  dharma_uniformity_p <- dharma_dispersion_p <- dharma_quantile_fitted_p <- NA_real_
  dharma_quantile_predictor_p <- dharma_categorical_design_p <- pregibon_p <- NA_real_
  if (inherits(wrong_fit, "try-error")) {
    problem_message <- as.character(wrong_fit)
  } else {
    aic_wrong <- stats::AIC(wrong_fit)
    if (scenario$family == "forced_choice") {
      sm <- summary(wrong_fit)$coefficients
      term <- "age_c:groupGroup 1"
      wrong_messages <- wrong_fit@optinfo$conv$lme4$messages
      wrong_problem <- length(wrong_messages) > 0 ||
        lme4::isSingular(wrong_fit, tol = 1e-4)
    } else {
      sm <- summary(wrong_fit)$coefficients$cond
      term <- "groupGroup 1:conditionCondition 1"
      wrong_problem <- !isTRUE(wrong_fit$sdr$pdHess) || wrong_fit$fit$convergence != 0
    }
    if (term %in% rownames(sm)) {
      interaction_p <- sm[term, 4]
      interaction_coef <- sm[term, 1]
    }
    if (!is.na(dharma_n_sim)) {
      simulated <- try(DHARMa::simulateResiduals(fittedModel = wrong_fit,
          n = dharma_n_sim, plot = FALSE, seed = NULL), silent = TRUE)
      if (!inherits(simulated, "try-error")) {
        check <- try(DHARMa::testUniformity(simulated, plot = FALSE), silent = TRUE)
        if (!inherits(check, "try-error") && length(check$p.value) == 1L) dharma_uniformity_p <- check$p.value
        check <- try(DHARMa::testDispersion(simulated, plot = FALSE), silent = TRUE)
        if (!inherits(check, "try-error") && length(check$p.value) == 1L) dharma_dispersion_p <- check$p.value
        if (scenario$family == "forced_choice") {
          fitted <- as.numeric(stats::predict(wrong_fit, type = "response", re.form = NA))
          if (length(unique(round(fitted[is.finite(fitted)], 10))) >= 8) {
            check <- try(DHARMa::testQuantiles(simulated, plot = FALSE), silent = TRUE)
            if (!inherits(check, "try-error") && length(check$p.value) == 1L) dharma_quantile_fitted_p <- check$p.value
          }
          if (length(unique(round(d$age_c, 10))) >= 8) {
            check <- try(DHARMa::testQuantiles(simulated, predictor = d$age_c, plot = FALSE), silent = TRUE)
            if (!inherits(check, "try-error") && length(check$p.value) == 1L) dharma_quantile_predictor_p <- check$p.value
          }
        } else {
          design_cell <- interaction(d$group, d$condition, drop = TRUE)
          residual <- simulated$scaledResiduals
          check <- try(stats::kruskal.test(residual ~ design_cell), silent = TRUE)
          if (!inherits(check, "try-error") && length(check$p.value) == 1L) dharma_categorical_design_p <- check$p.value
        }
      }
    }
    # Pregibon added squared linear predictor, retaining the original formula.
    augmented <- d
    augmented$eta_hat_sq <- as.numeric(stats::predict(
      wrong_fit, type = "link", re.form = if (scenario$family == "forced_choice") NA else NULL
    ))^2
    if (scenario$family == "forced_choice") {
      added_fit <- try(lme4::glmer(
          correct ~ age_c * group + eta_hat_sq + (1 | id),
          data = augmented, family = stats::binomial("logit")
        ), silent = TRUE)
      if (!inherits(added_fit, "try-error")) {
        sm <- summary(added_fit)$coefficients
        if ("eta_hat_sq" %in% rownames(sm)) pregibon_p <- sm["eta_hat_sq", 4]
      }
    } else {
      added_fit <- try(glmmTMB::glmmTMB(y ~ group * condition + (1 | id) + eta_hat_sq,
          data = augmented, family = stats::binomial("logit")), silent = TRUE)
      if (!inherits(added_fit, "try-error")) {
        sm <- summary(added_fit)$coefficients$cond
        if ("eta_hat_sq" %in% rownames(sm)) pregibon_p <- sm["eta_hat_sq", 4]
      }
    }
  }
  data.frame(scenario_id = scenario$scenario_id, family = scenario$family,
    replication = replication, replication_seed = replication_seed,
    interaction_p = interaction_p, interaction_coef = interaction_coef,
    fit_success = is.finite(interaction_p) && !wrong_problem,
    convergence_problem = wrong_problem || correct_problem, problem_message = problem_message,
    aic_generating = aic_correct, aic_wrong = aic_wrong,
    aic_favors_generating = if (is.finite(aic_correct) && is.finite(aic_wrong)) aic_correct <= aic_wrong else NA,
    dharma_uniformity_p = dharma_uniformity_p, dharma_dispersion_p = dharma_dispersion_p,
    dharma_quantile_fitted_p = dharma_quantile_fitted_p,
    dharma_quantile_predictor_p = dharma_quantile_predictor_p,
    dharma_categorical_design_p = dharma_categorical_design_p,
    dharma_computed = !is.na(dharma_n_sim), pregibon_p = pregibon_p, stringsAsFactors = FALSE)
}

cluster <- NULL
if (n_cores > 1 && .Platform$OS.type != "unix") cluster <- parallel::makeCluster(n_cores)
for (i in seq_len(nrow(scenarios))) {
  scenario <- scenarios[i, ]
  output_file <- file.path("simulation-atlas/raw",
    sprintf("%s-%s-%s-B%d.rds", kind, scenario$scenario_id, MODE, B))
  if (file.exists(output_file) && !OVERWRITE) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")
  if (n_cores > 1 && .Platform$OS.type == "unix") {
    result <- parallel::mclapply(seq_len(B), run_one_replication,
      scenario = scenario, dharma_n_sim = dharma_n_sim,
      mc.cores = n_cores, mc.set.seed = FALSE)
  } else if (!is.null(cluster)) {
    result <- parallel::parLapply(cluster, seq_len(B), run_one_replication,
      scenario = scenario, dharma_n_sim = dharma_n_sim)
  } else {
    result <- lapply(seq_len(B), run_one_replication,
      scenario = scenario, dharma_n_sim = dharma_n_sim)
  }
  result <- do.call(rbind, result)
  result$B_requested <- B
  result$run_type <- MODE
  result$dharma_n_sim <- dharma_n_sim
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
