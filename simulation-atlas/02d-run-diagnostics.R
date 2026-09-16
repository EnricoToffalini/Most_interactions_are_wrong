# Targeted diagnostic sensitivity analysis; definitions preserved.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
library(glmmTMB)
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
    eta <- scenario$beta_intercept + settings$beta_age * age_c +
      settings$beta_group * group_num + settings$beta_age_group * age_c * group_num
    p <- settings$chance + (1 - settings$chance) * stats::plogis(eta)
    y <- stats::rbinom(settings$N, size = settings$k_trials, prob = p)

    d <- data.frame(
      age = age,
      age_c = age_c,
      group_num = group_num,
      group = factor(
        group_num,
        levels = c(0, 1),
        labels = c("Group 0", "Group 1")
      ),
      y = y,
      k = settings$k_trials,
      accuracy = y / settings$k_trials,
      stringsAsFactors = FALSE
    )
    wrong_fit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
        data = d, family = stats::binomial("logit")), silent = TRUE)
    # Chance-corrected binomial logit: likelihood, three starts, and Wald test.
    X <- stats::model.matrix(~ age_c * group, data = d)
    y <- d$y
    k <- d$k
    chance <- settings$chance
    nll <- function(beta) {
      eta <- drop(X %*% beta)
      p <- chance + (1 - chance) * stats::plogis(eta)
      p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
      -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
    }
    gradient <- function(beta) {
      eta <- drop(X %*% beta)
      q <- stats::plogis(eta)
      p <- chance + (1 - chance) * q
      p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
      weight <- ((y - k * p) / (p * (1 - p))) * (1 - chance) * q * (1 - q)
      -drop(crossprod(X, weight))
    }
    zero <- stats::setNames(rep(0, ncol(X)), colnames(X))
    from_standard <- zero
    start_fit <- try(stats::glm.fit(X, cbind(y, k - y),
        family = stats::binomial("logit")), silent = TRUE)
    if (!inherits(start_fit, "try-error") && all(is.finite(stats::coef(start_fit)))) {
      from_standard[] <- stats::coef(start_fit)
    }
    above <- (y / k - chance) / (1 - chance)
    above <- pmin(pmax(above, 0.02), 0.98)
    from_above <- zero
    above_fit <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
    if (!inherits(above_fit, "try-error") && all(is.finite(stats::coef(above_fit)))) {
      from_above[] <- stats::coef(above_fit)
    }
    candidates <- list()
    for (start in list(zero, from_standard, from_above)) {
      candidate <- try(stats::optim(start, nll, gr = gradient, method = "BFGS",
          control = list(maxit = 1500, reltol = 1e-10)),
        silent = TRUE)
      if (!inherits(candidate, "try-error") && is.finite(candidate$value) &&
          all(is.finite(candidate$par))) {
        candidates[[length(candidates) + 1L]] <- candidate
      }
    }
    chance_coef <- chance_se <- chance_p <- rep(NA_real_, ncol(X))
    chance_vcov <- matrix(NA_real_, ncol(X), ncol(X))
    chance_usable <- FALSE
    chance_nll <- NA_real_
    if (length(candidates)) {
      best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "value"))]]
      chance_coef <- best$par
      chance_nll <- best$value
      hessian <- try(stats::optimHess(best$par, nll, gr = gradient), silent = TRUE)
      well_conditioned <- FALSE
      if (!inherits(hessian, "try-error") && all(is.finite(hessian))) {
        hess_sym <- (hessian + t(hessian)) / 2
        eig <- try(eigen(hess_sym, symmetric = TRUE, only.values = TRUE)$values,
          silent = TRUE)
        if (!inherits(eig, "try-error")) {
          well_conditioned <- all(is.finite(eig)) && min(eig) > 1e-7 &&
            min(eig) / max(eig) > sqrt(.Machine$double.eps)
        }
        if (well_conditioned) {
          V <- try(solve(hess_sym), silent = TRUE)
          if (!inherits(V, "try-error") && all(is.finite(V))) chance_vcov <- V
        }
      }
      variances <- diag(chance_vcov)
      variances[!is.finite(variances) | variances <= 0] <- NA_real_
      chance_se <- sqrt(variances)
      z <- chance_coef / chance_se
      chance_p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      chance_usable <- isTRUE(best$convergence == 0) && well_conditioned &&
        all(is.finite(chance_vcov)) && all(is.finite(chance_se)) && all(is.finite(chance_p))
      if (!chance_usable) chance_p[] <- NA_real_
    }
    names(chance_coef) <- names(chance_se) <- names(chance_p) <- colnames(X)
    aic_correct <- 2 * chance_nll + 2 * ncol(X)
    correct_problem <- !chance_usable
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
      wrong_problem <- !isTRUE(wrong_fit$converged)
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
          fitted <- as.numeric(stats::predict(wrong_fit, type = "response"))
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
    augmented$eta_hat_sq <- as.numeric(stats::predict(wrong_fit, type = "link"))^2
    if (scenario$family == "forced_choice") {
      added_fit <- try(stats::glm(cbind(y, k - y) ~ age_c * group + eta_hat_sq,
          data = augmented, family = stats::binomial("logit"),
          start = c(stats::coef(wrong_fit), eta_hat_sq = 0)), silent = TRUE)
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
