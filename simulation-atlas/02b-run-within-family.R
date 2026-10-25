# Atlas, within family (logit vs probit): see scripts/03b-simulation-within-family-links.R.
# Unlike the manuscript script, the atlas draws aggregated binomial counts per
# subject and condition, and fits a GLM (no random intercept) when target ICC = 0.
# Run from the repository root after 01-build-scenario-grid.R.
# Scenarios run in grid order; replications run in parallel with their own seeds.
# Environment variables: ATLAS_MODE (full/smoke), N_SIM, N_CORES, ATLAS_OVERWRITE.

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(parallel)
mode <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (mode == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
overwrite <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)

grid <- read.csv("simulation-atlas/data/scenario-grid.csv")
scenarios <- grid[grid$family == "within_family", ]
if (mode == "smoke") scenarios <- scenarios[scenarios$scenario_id == "WF-002", ]

sim_one <- function(replication, scenario, deterministic) {
  replication_seed <- as.integer((20260807 + 3000000 + as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  n <- scenario$n_subjects
  id <- rep(1:n, each = 2)
  group_num <- rep(rep(c(0, 1), each = n / 2), each = 2)
  condition_num <- rep(c(0, 1), times = n)
  residual_variance <- if (scenario$generating_link == "logit") pi^2 / 3 else 1
  u <- rnorm(n, sd = sqrt(scenario$target_icc * residual_variance / (1 - scenario$target_icc)))
  eta <- scenario$beta_intercept + scenario$beta_group * group_num + scenario$beta_condition * condition_num +
    scenario$beta_group_condition * group_num * condition_num + u[id]
  prob <- if (scenario$generating_link == "logit") plogis(eta) else pnorm(eta)
  d <- data.frame(id = factor(id), group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    condition = factor(condition_num, levels = c(0, 1), labels = c("Condition 0", "Condition 1")),
    successes = rbinom(length(prob), scenario$k_trials, prob), k = scenario$k_trials)

  # the four cells, for the fixed-effects difference in differences on the probability scale
  X <- model.matrix(~ group * condition, expand.grid(group = factor(c("Group 0", "Group 1")), condition = factor(c("Condition 0", "Condition 1"))))
  links <- c("logit", "probit")
  p_values <- coefs <- ses <- did <- rep(NA_real_, 2)
  problems <- rep(TRUE, 2)
  messages <- rep("", 2)
  for (j in 1:2) {
    if (scenario$target_icc == 0) {
      fit <- try(glm(cbind(successes, k - successes) ~ group * condition, data = d, family = binomial(links[j])), silent = TRUE)
    } else {
      fit <- try(glmmTMB(cbind(successes, k - successes) ~ group * condition + (1 | id), data = d, family = binomial(links[j])), silent = TRUE)
    }
    if (inherits(fit, "try-error")) {
      messages[j] <- as.character(fit)
      next
    }
    if (scenario$target_icc == 0) {
      tab <- summary(fit)$coefficients
      beta <- coef(fit)
      problems[j] <- !isTRUE(fit$converged)
    } else {
      tab <- summary(fit)$coefficients$cond
      beta <- fixef(fit)$cond
      problems[j] <- !isTRUE(fit$sdr$pdHess) || fit$fit$convergence != 0
    }
    term <- "groupGroup 1:conditionCondition 1"
    if (term %in% rownames(tab)) {
      p_values[j] <- tab[term, 4]
      coefs[j] <- tab[term, 1]
      ses[j] <- tab[term, 2]
    }
    prob <- if (links[j] == "logit") plogis(drop(X %*% beta[colnames(X)])) else pnorm(drop(X %*% beta[colnames(X)]))
    did[j] <- prob[4] - prob[2] - prob[3] + prob[1]
  }
  data.frame(scenario_id = scenario$scenario_id, family = "within_family", replication = replication, replication_seed = replication_seed,
    model_label = c("Binomial logit", "Binomial probit"), fitted_link = links,
    fit_structure = if (scenario$target_icc == 0) "GLM" else "random-intercept GLMM",
    interaction_p = p_values, interaction_coef = coefs, interaction_se = ses, response_scale_did = did, outcome_scale_did = did,
    deterministic_pseudo_interaction = deterministic$pseudo, deterministic_response_scale_did = deterministic$response_did,
    fit_success = is.finite(p_values) & !problems, convergence_problem = problems, problem_message = messages)
}

cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, library(glmmTMB)))
for (i in 1:nrow(scenarios)) {
  scenario <- scenarios[i, ]
  output_file <- sprintf("simulation-atlas/raw/core-%s-%s-B%d.rds", scenario$scenario_id, mode, B)
  if (file.exists(output_file) && !overwrite) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")

  # deterministic pseudo-interaction on the logit and probit scales, at random intercept = 0
  nd <- expand.grid(group_num = c(0, 1), condition_num = c(0, 1))
  eta <- scenario$beta_intercept + scenario$beta_group * nd$group_num + scenario$beta_condition * nd$condition_num +
    scenario$beta_group_condition * nd$group_num * nd$condition_num
  prob <- if (scenario$generating_link == "logit") plogis(eta) else pnorm(eta)
  p <- pmin(pmax(prob, 1e-10), 1 - 1e-10)
  scales <- cbind(qlogis(p), qnorm(p))
  pseudo <- scales[4, ] - scales[2, ] - scales[3, ] + scales[1, ]
  pseudo[is.finite(pseudo) & abs(pseudo) < 1e-12] <- 0
  deterministic <- list(pseudo = unname(pseudo), response_did = prob[4] - prob[2] - prob[3] + prob[1])

  result <- do.call(rbind, parLapply(cl, 1:B, sim_one, scenario = scenario, deterministic = deterministic))
  result$B_requested <- B
  result$run_type <- mode
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
stopCluster(cl)
