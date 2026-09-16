# forced_choice: same DGP and model calculations as scripts/03a-simulation-forced-choice.R.
# Run from the repository root after 01-build-scenario-grid.R.
# Replications are parallel; scenarios are visited in declared grid order.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
MODE <- tolower(Sys.getenv("ATLAS_MODE", "full"))
B <- as.integer(Sys.getenv("N_SIM", if (MODE == "smoke") "3" else "3000"))
n_cores <- as.integer(Sys.getenv("N_CORES", Sys.getenv("SLURM_CPUS_PER_TASK",
      max(1, parallel::detectCores() - 1))))
OVERWRITE <- as.logical(Sys.getenv("ATLAS_OVERWRITE", "FALSE"))
dir.create("simulation-atlas/raw", recursive = TRUE, showWarnings = FALSE)
grid <- utils::read.csv("simulation-atlas/data/scenario-grid.csv", stringsAsFactors = FALSE)
scenarios <- grid[grid$family == "forced_choice", ]
if (MODE == "smoke") scenarios <- scenarios[scenarios$scenario_id == "FC-002", ]

run_one_replication <- function(replication, scenario, deterministic) {
  settings <- as.list(scenario)
  replication_seed <- as.integer((20260807 + 1000000 +
        as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  settings$age_range <- c(scenario$age_min, scenario$age_max)
  beta_intercept <- scenario$beta_intercept

  group_num <- stats::rbinom(settings$N, 1, 0.5)
  age <- stats::runif(settings$N, settings$age_range[1], settings$age_range[2])
  age_c <- age - settings$age_center
  eta <- beta_intercept + settings$beta_age * age_c +
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

  fit_gaussian <- try(stats::lm(accuracy ~ age_c * group, data = d), silent = TRUE)
  fit_logit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
      family = stats::binomial("logit"), data = d), silent = TRUE)
  fit_probit <- try(stats::glm(cbind(y, k - y) ~ age_c * group,
      family = stats::binomial("probit"), data = d), silent = TRUE)

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

  nd <- expand.grid(age = settings$age_range,
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")))
  nd$age_c <- nd$age - settings$age_center
  p_values <- coefficients <- standard_errors <- did <- rep(NA_real_, 4)
  problems <- rep(TRUE, 4)
  messages <- rep("", 4)
  if (inherits(fit_gaussian, "try-error")) messages[1] <- as.character(fit_gaussian)
  if (!inherits(fit_gaussian, "try-error")) {
    problems[1] <- FALSE
    sm <- summary(fit_gaussian)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[1] <- sm["age_c:groupGroup 1", 4]
      coefficients[1] <- sm["age_c:groupGroup 1", 1]
      standard_errors[1] <- sm["age_c:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_gaussian, newdata = nd, type = "response")
    did[1] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (inherits(fit_logit, "try-error")) messages[2] <- as.character(fit_logit)
  if (!inherits(fit_logit, "try-error")) {
    problems[2] <- !isTRUE(fit_logit$converged)
    sm <- summary(fit_logit)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[2] <- sm["age_c:groupGroup 1", 4]
      coefficients[2] <- sm["age_c:groupGroup 1", 1]
      standard_errors[2] <- sm["age_c:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_logit, newdata = nd, type = "response")
    did[2] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (inherits(fit_probit, "try-error")) messages[3] <- as.character(fit_probit)
  if (!inherits(fit_probit, "try-error")) {
    problems[3] <- !isTRUE(fit_probit$converged)
    sm <- summary(fit_probit)$coefficients
    if ("age_c:groupGroup 1" %in% rownames(sm)) {
      p_values[3] <- sm["age_c:groupGroup 1", 4]
      coefficients[3] <- sm["age_c:groupGroup 1", 1]
      standard_errors[3] <- sm["age_c:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_probit, newdata = nd, type = "response")
    did[3] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  p_values[4] <- chance_p["age_c:groupGroup 1"]
  coefficients[4] <- chance_coef["age_c:groupGroup 1"]
  pred <- chance + (1 - chance) * stats::plogis(drop(stats::model.matrix(~ age_c * group, nd) %*% chance_coef))
  did[4] <- (pred[4] - pred[2]) - (pred[3] - pred[1])

  standard_errors[4] <- chance_se["age_c:groupGroup 1"]
  problems[4] <- !chance_usable
  messages[4] <- if (chance_usable) "" else "chance-corrected optimizer/Hessian check failed"
  data.frame(scenario_id = scenario$scenario_id, family = "forced_choice",
    replication = replication, replication_seed = replication_seed,
    model_label = c("Gaussian identity", "Standard binomial logit", "Standard binomial probit", "Chance-corrected binomial logit"), fitted_link = c("identity", "logit", "probit", "chance-corrected logit"),
    interaction_p = p_values, interaction_coef = coefficients, interaction_se = standard_errors,
    response_scale_did = did, outcome_scale_did = did * settings$k_trials,
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
  nd <- expand.grid(age = c(scenario$age_min, scenario$age_max), group_num = c(0, 1))
  eta <- scenario$beta_intercept + scenario$beta_age * (nd$age - scenario$age_center) +
    scenario$beta_group * nd$group_num + scenario$beta_age_group * (nd$age - scenario$age_center) * nd$group_num
  probability <- scenario$chance + (1 - scenario$chance) * stats::plogis(eta)
  p <- pmin(pmax(probability, 1e-10), 1 - 1e-10)
  q <- pmin(pmax((probability - scenario$chance) / (1 - scenario$chance), 1e-10), 1 - 1e-10)
  fitted_values <- cbind(probability, stats::qlogis(p), stats::qnorm(p), stats::qlogis(q))
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
