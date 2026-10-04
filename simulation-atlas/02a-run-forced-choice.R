# Atlas, forced choice: same DGP and models as scripts/03a-simulation-forced-choice.R,
# repeated over the forced_choice rows of the scenario grid.
# Run from the repository root after 01-build-scenario-grid.R.
# Scenarios run in grid order; replications run in parallel. Each replication sets
# its own seed, so results do not depend on the number of cores.
# Environment variables: ATLAS_MODE (full/smoke), N_SIM, N_CORES, ATLAS_OVERWRITE.

Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(parallel)
mode <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (mode == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK", max(1, detectCores() - 1))))
overwrite <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)

grid <- read.csv("simulation-atlas/data/scenario-grid.csv")
scenarios <- grid[grid$family == "forced_choice", ]
if (mode == "smoke") scenarios <- scenarios[scenarios$scenario_id == "FC-002", ]

# Fits one model and returns the interaction test plus minimal fit-quality flags.
# Warnings are collected and errors return NA, so one bad fit does not stop the run.
fit_check <- function(fit_expr, nd) {
  warn <- character()
  out <- tryCatch(withCallingHandlers({
    fit <- fit_expr # the model is actually fitted here (lazy evaluation)
    list(fit = fit, tab = coef(summary(fit)))
  }, warning = function(w) {
    warn <<- c(warn, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) conditionMessage(e))
  warn <- unique(warn)

  if (is.character(out)) {
    problems <- c(out, warn)
    return(data.frame(interaction_p = NA_real_, interaction_coef = NA_real_, interaction_se = NA_real_, response_scale_did = NA_real_,
      fit_problem = TRUE, problem_message = paste(unique(problems[nzchar(problems)]), collapse = " | ")))
  }

  fit <- out$fit
  term <- "age_c:groupGroup 1"
  est <- if (term %in% rownames(out$tab)) out$tab[term, 1] else NA_real_
  se <- if (term %in% rownames(out$tab)) out$tab[term, 2] else NA_real_
  p <- if (is.finite(est) && is.finite(se) && se > 0) 2 * pnorm(abs(est / se), lower.tail = FALSE) else NA_real_
  pred <- try(predict(fit, newdata = nd, type = "response", re.form = NA), silent = TRUE)
  did <- if (inherits(pred, "try-error") || any(!is.finite(pred))) NA_real_ else pred[4] - pred[2] - pred[3] + pred[1]
  problems <- c(fit@optinfo$conv$lme4$messages,
                if (isSingular(fit, tol = 1e-4)) "singular fit",
                grep("failed to converge|identif|Hessian|positive definite|degenerate", warn, ignore.case = TRUE, value = TRUE),
                if (!is.finite(p)) "non-finite or non-positive interaction inference")
  data.frame(interaction_p = unname(p), interaction_coef = unname(est), interaction_se = unname(se),
    response_scale_did = unname(did), fit_problem = length(problems) > 0, problem_message = paste(unique(problems), collapse = " | "))
}

sim_one <- function(replication, scenario, deterministic) {
  replication_seed <- as.integer((20260807 + 1000000 + as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  N <- scenario$N
  k <- scenario$k_trials
  chance <- scenario$chance
  group_num <- rbinom(N, 1, 0.5)
  age <- runif(N, scenario$age_min, scenario$age_max)
  age_c <- age - scenario$age_center
  u <- rnorm(N, 0, sqrt(scenario$target_icc * (pi^2 / 3) / (1 - scenario$target_icc)))
  # no product term on the conditional chance-corrected logit scale
  eta <- scenario$beta_intercept + scenario$beta_age * age_c + scenario$beta_group * group_num + scenario$beta_age_group * age_c * group_num + u
  d <- data.frame(id = factor(rep(1:N, each = k)), age_c = rep(age_c, each = k),
    group = factor(rep(group_num, each = k), levels = c(0, 1), labels = c("Group 0", "Group 1")))
  d$correct <- rbinom(nrow(d), 1, chance + (1 - chance) * plogis(rep(eta, each = k)))

  nd <- expand.grid(age = c(scenario$age_min, scenario$age_max), group = factor(c("Group 0", "Group 1")))
  nd$age_c <- nd$age - scenario$age_center
  res <- rbind(
    fit_check(lmer(correct ~ age_c * group + (1 | id), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial("logit"), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial("probit"), data = d), nd),
    fit_check(glmer(correct ~ age_c * group + (1 | id), family = binomial(mafc.logit(as.integer(round(1 / chance)))), data = d), nd)
  )
  data.frame(scenario_id = scenario$scenario_id, family = "forced_choice", replication = replication,
    replication_seed = replication_seed,
    model_label = c("Gaussian identity", "Standard binomial logit", "Standard binomial probit", "Chance-corrected binomial logit"),
    fitted_link = c("identity", "logit", "probit", "chance-corrected logit"),
    interaction_p = res$interaction_p, interaction_coef = res$interaction_coef, interaction_se = res$interaction_se,
    response_scale_did = res$response_scale_did, outcome_scale_did = res$response_scale_did * k,
    deterministic_pseudo_interaction = deterministic$pseudo, deterministic_response_scale_did = deterministic$response_did,
    fit_success = is.finite(res$interaction_p) & !res$fit_problem, convergence_problem = res$fit_problem,
    problem_message = res$problem_message)
}

cl <- makeCluster(n_cores)
invisible(clusterEvalQ(cl, {library(lme4); library(psyphy)}))
clusterExport(cl, "fit_check")
for (i in 1:nrow(scenarios)) {
  scenario <- scenarios[i, ]
  output_file <- sprintf("simulation-atlas/raw/core-%s-%s-B%d.rds", scenario$scenario_id, mode, B)
  if (file.exists(output_file) && !overwrite) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")

  # deterministic pseudo-interaction of the four fitted scales, at random intercept = 0
  nd <- expand.grid(age = c(scenario$age_min, scenario$age_max), group_num = c(0, 1))
  eta <- scenario$beta_intercept + scenario$beta_age * (nd$age - scenario$age_center) + scenario$beta_group * nd$group_num +
    scenario$beta_age_group * (nd$age - scenario$age_center) * nd$group_num
  prob <- scenario$chance + (1 - scenario$chance) * plogis(eta)
  p <- pmin(pmax(prob, 1e-10), 1 - 1e-10)
  q <- pmin(pmax((prob - scenario$chance) / (1 - scenario$chance), 1e-10), 1 - 1e-10)
  scales <- cbind(prob, qlogis(p), qnorm(p), qlogis(q))
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
