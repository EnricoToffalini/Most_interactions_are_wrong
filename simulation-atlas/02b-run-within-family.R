# Within-family Atlas; see scripts/03b-simulation-within-family-links.R.
library(glmmTMB)
# Run from the repository root after 01-build-scenario-grid.R.
# Replications are parallel; scenarios are visited in declared grid order.
MODE <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (MODE == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK",
                       max(1, parallel::detectCores() - 1))))
OVERWRITE <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)
grid <- utils::read.csv("simulation-atlas/data/scenario-grid.csv", stringsAsFactors = FALSE)

scenarios <- grid[grid$family == "within_family", ]
if (MODE == "smoke") scenarios <- scenarios[scenarios$scenario_id == "WF-002", ]
run_one_replication <- function(replication, scenario, deterministic) {
  replication_seed <- as.integer((20260807 + 3000000 +
    as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  # Preserve the Atlas's aggregated binomial draws (the manuscript uses individual trials).
  id <- rep(seq_len(scenario$n_subjects), each = 2)
  group_subject <- rep(c(0, 1), each = scenario$n_subjects / 2)
  group_num <- rep(group_subject, each = 2)
  condition_num <- rep(c(0, 1), times = scenario$n_subjects)
  residual_variance <- if (scenario$generating_link == "logit") pi^2 / 3 else 1
  u_sd <- sqrt(scenario$target_icc * residual_variance / (1 - scenario$target_icc))
  random_effect <- stats::rnorm(scenario$n_subjects, sd = u_sd)
  eta <- scenario$beta_intercept + scenario$beta_group * group_num +
    scenario$beta_condition * condition_num + scenario$beta_group_condition * group_num * condition_num + random_effect[id]
  probability <- if (scenario$generating_link == "logit") stats::plogis(eta) else stats::pnorm(eta)
  successes <- stats::rbinom(length(probability), scenario$k_trials, probability)
  d <- data.frame(id = factor(id), group_num = group_num, condition_num = condition_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    condition = factor(condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1")),
    successes = successes, k = scenario$k_trials, y = successes / scenario$k_trials)
  # The existing sensitivity design uses a GLM at ICC = 0.
  if (scenario$target_icc == 0) {
    fit_logit <- try(stats::glm(cbind(successes, k - successes) ~ group * condition,
      data = d, family = stats::binomial("logit")), silent = TRUE)
    fit_probit <- try(stats::glm(cbind(successes, k - successes) ~ group * condition,
      data = d, family = stats::binomial("probit")), silent = TRUE)
  } else {
    fit_logit <- try(glmmTMB::glmmTMB(cbind(successes, k - successes) ~ group * condition + (1 | id),
      data = d, family = stats::binomial("logit")), silent = TRUE)
    fit_probit <- try(glmmTMB::glmmTMB(cbind(successes, k - successes) ~ group * condition + (1 | id),
      data = d, family = stats::binomial("probit")), silent = TRUE)
  }
  nd <- expand.grid(group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")),
    condition = factor(c("Condition 0", "Condition 1"), levels = c("Condition 0", "Condition 1")))
  X <- stats::model.matrix(~ group * condition, nd)
  p_values <- coefficients <- standard_errors <- did <- rep(NA_real_, 2)
  problems <- rep(TRUE, 2)
  messages <- rep("", 2)
  if (inherits(fit_logit, "try-error")) messages[1] <- as.character(fit_logit)
  if (!inherits(fit_logit, "try-error")) {
    if (scenario$target_icc == 0) {
      sm <- summary(fit_logit)$coefficients
      beta <- stats::coef(fit_logit)
      problems[1] <- !isTRUE(fit_logit$converged)
    } else {
      sm <- summary(fit_logit)$coefficients$cond
      beta <- glmmTMB::fixef(fit_logit)$cond
      problems[1] <- !isTRUE(fit_logit$sdr$pdHess) || fit_logit$fit$convergence != 0
    }
    if ("groupGroup 1:conditionCondition 1" %in% rownames(sm)) {
      p_values[1] <- sm["groupGroup 1:conditionCondition 1", 4]
      coefficients[1] <- sm["groupGroup 1:conditionCondition 1", 1]
      standard_errors[1] <- sm["groupGroup 1:conditionCondition 1", 2]
    }
    probability <- stats::plogis(drop(X %*% beta[colnames(X)]))
    did[1] <- probability[4] - probability[2] - probability[3] + probability[1]
  }
  if (inherits(fit_probit, "try-error")) messages[2] <- as.character(fit_probit)
  if (!inherits(fit_probit, "try-error")) {
    if (scenario$target_icc == 0) {
      sm <- summary(fit_probit)$coefficients
      beta <- stats::coef(fit_probit)
      problems[2] <- !isTRUE(fit_probit$converged)
    } else {
      sm <- summary(fit_probit)$coefficients$cond
      beta <- glmmTMB::fixef(fit_probit)$cond
      problems[2] <- !isTRUE(fit_probit$sdr$pdHess) || fit_probit$fit$convergence != 0
    }
    if ("groupGroup 1:conditionCondition 1" %in% rownames(sm)) {
      p_values[2] <- sm["groupGroup 1:conditionCondition 1", 4]
      coefficients[2] <- sm["groupGroup 1:conditionCondition 1", 1]
      standard_errors[2] <- sm["groupGroup 1:conditionCondition 1", 2]
    }
    probability <- stats::pnorm(drop(X %*% beta[colnames(X)]))
    did[2] <- probability[4] - probability[2] - probability[3] + probability[1]
  }
  data.frame(scenario_id = scenario$scenario_id, family = "within_family",
    replication = replication, replication_seed = replication_seed,
    model_label = c("Binomial logit", "Binomial probit"), fitted_link = c("logit", "probit"),
    fit_structure = if (scenario$target_icc == 0) "GLM" else "random-intercept GLMM",
    interaction_p = p_values, interaction_coef = coefficients, interaction_se = standard_errors,
    response_scale_did = did, outcome_scale_did = did,
    deterministic_pseudo_interaction = deterministic$pseudo,
    deterministic_response_scale_did = deterministic$response_did,
    fit_success = is.finite(p_values) & !problems, convergence_problem = problems,
    problem_message = messages, stringsAsFactors = FALSE)
}

cluster <- NULL
if (n_cores > 1 && .Platform$OS.type != "unix") cluster <- parallel::makeCluster(n_cores)
for (i in seq_len(nrow(scenarios))) {
  scenario <- scenarios[i, ]
  output_file <- file.path("simulation-atlas/raw",
    sprintf("core-%s-%s-B%d.rds", scenario$scenario_id, MODE, B))
  if (file.exists(output_file) && !OVERWRITE) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")
  nd <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  eta <- scenario$beta_intercept + scenario$beta_group * nd$group_num +
    scenario$beta_condition * nd$condition_num + scenario$beta_group_condition * nd$group_num * nd$condition_num
  probability <- if (scenario$generating_link == "logit") stats::plogis(eta) else stats::pnorm(eta)
  p <- pmin(pmax(probability, 1e-10), 1 - 1e-10)
  fitted_values <- cbind(stats::qlogis(p), stats::qnorm(p))
  pseudo <- fitted_values[4, ] - fitted_values[2, ] - fitted_values[3, ] + fitted_values[1, ]
  pseudo[is.finite(pseudo) & abs(pseudo) < 1e-12] <- 0
  deterministic <- list(pseudo = unname(pseudo),
    response_did = probability[4] - probability[2] - probability[3] + probability[1])
  if (n_cores > 1 && .Platform$OS.type == "unix") {
    result <- parallel::mclapply(seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic,
      mc.cores = n_cores, mc.set.seed = FALSE)
  } else if (!is.null(cluster)) {
    result <- parallel::parLapply(cluster, seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic)
  } else {
    result <- lapply(seq_len(B), run_one_replication,
      scenario = scenario, deterministic = deterministic)
  }
  result <- do.call(rbind, result)
  result$B_requested <- B
  result$run_type <- MODE
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
if (!is.null(cluster)) parallel::stopCluster(cluster)
