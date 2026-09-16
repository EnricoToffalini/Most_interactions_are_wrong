# sum_scores: same DGP and model calculations as scripts/03c-simulation-sum-scores.R.
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
scenarios <- grid[grid$family == "sum_scores", ]
if (MODE == "smoke") scenarios <- scenarios[scenarios$scenario_id == "SS-002", ]

run_one_replication <- function(replication, scenario, deterministic) {
  settings <- as.list(scenario)
  replication_seed <- as.integer((20260807 + 2000000 +
        as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  settings$max_score <- scenario$J * scenario$item_max
  x_low <- scenario$x_min
  x_high <- scenario$x_max
  threshold_shift <- scenario$threshold_shift

  x <- stats::rnorm(settings$N, 0, 1)
  group_num <- stats::rbinom(settings$N, 1, 0.5)
  mu <- settings$beta_x * x + settings$beta_group * group_num +
    settings$beta_x_group * x * group_num
  theta <- mu + stats::rnorm(settings$N, 0, settings$theta_sd)
  thresholds <- matrix(rep(c(-1, 0, 1), each = settings$J), nrow = settings$J) +
    seq(-0.45, 0.45, length.out = settings$J) + threshold_shift
  items <- matrix(NA_integer_, nrow = settings$N, ncol = settings$J)
  for (j in seq_len(settings$J)) {
    p_ge_1 <- stats::plogis(theta - thresholds[j, 1])
    p_ge_2 <- stats::plogis(theta - thresholds[j, 2])
    p_ge_3 <- stats::plogis(theta - thresholds[j, 3])
    u <- stats::runif(settings$N)
    items[, j] <- 0L + (u < p_ge_1) + (u < p_ge_2) + (u < p_ge_3)
  }
  sum_score <- rowSums(items)

  # The Gaussian-probit model below requires responses strictly inside (0, 1).
  # We therefore use a continuity-corrected score proportion for that model:
  # (sum_score + 0.5) / (max_score + 1).
  # The raw sum score and raw score proportion are left unchanged for all
  # descriptive summaries, plots, and the identity-link model.
  score_prop <- sum_score / settings$max_score
  score_prop_probit <- (sum_score + 0.5) / (settings$max_score + 1)

  d <- data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    latent_mean = mu,
    theta = theta,
    sum_score = sum_score,
    score_prop = score_prop,
    score_prop_probit = score_prop_probit,
    max_score = settings$max_score,
    stringsAsFactors = FALSE
  )

  fit_identity <- try(stats::lm(sum_score ~ x * group, data = d), silent = TRUE)
  fit_probit <- try(stats::glm(score_prop_probit ~ x * group,
      family = stats::gaussian(link = stats::make.link("probit")),
      data = d), silent = TRUE)
  fit_latent <- try(stats::lm(theta ~ x * group, data = d), silent = TRUE)
  nd <- expand.grid(x = c(x_low, x_high),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1")))
  nd$max_score <- settings$max_score
  p_values <- coefficients <- standard_errors <- did <- rep(NA_real_, 3)
  problems <- rep(TRUE, 3)
  messages <- rep("", 3)
  if (inherits(fit_identity, "try-error")) messages[1] <- as.character(fit_identity)
  if (!inherits(fit_identity, "try-error")) {
    problems[1] <- FALSE
    sm <- summary(fit_identity)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[1] <- sm["x:groupGroup 1", 4]
      coefficients[1] <- sm["x:groupGroup 1", 1]
      standard_errors[1] <- sm["x:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_identity, newdata = nd, type = "response")
    did[1] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (inherits(fit_probit, "try-error")) messages[2] <- as.character(fit_probit)
  if (!inherits(fit_probit, "try-error")) {
    problems[2] <- !isTRUE(fit_probit$converged)
    sm <- summary(fit_probit)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[2] <- sm["x:groupGroup 1", 4]
      coefficients[2] <- sm["x:groupGroup 1", 1]
      standard_errors[2] <- sm["x:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_probit, newdata = nd, type = "response") * settings$max_score
    did[2] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  if (inherits(fit_latent, "try-error")) messages[3] <- as.character(fit_latent)
  if (!inherits(fit_latent, "try-error")) {
    problems[3] <- FALSE
    sm <- summary(fit_latent)$coefficients
    if ("x:groupGroup 1" %in% rownames(sm)) {
      p_values[3] <- sm["x:groupGroup 1", 4]
      coefficients[3] <- sm["x:groupGroup 1", 1]
      standard_errors[3] <- sm["x:groupGroup 1", 2]
    }
    pred <- stats::predict(fit_latent, newdata = nd, type = "response")
    did[3] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }

  data.frame(scenario_id = scenario$scenario_id, family = "sum_scores",
    replication = replication, replication_seed = replication_seed,
    model_label = c("Observed sum-score identity", "Gaussian-probit bounded score", "Latent generating scale"), fitted_link = c("identity", "probit", "identity latent benchmark"),
    interaction_p = p_values, interaction_coef = coefficients, interaction_se = standard_errors,
    response_scale_did = did / settings$max_score, outcome_scale_did = did,
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
  nd <- expand.grid(x = c(scenario$x_min, scenario$x_max), group_num = c(0, 1))
  latent <- scenario$beta_x * nd$x + scenario$beta_group * nd$group_num + scenario$beta_x_group * nd$x * nd$group_num
  thresholds <- matrix(rep(c(-1, 0, 1), each = scenario$J), nrow = scenario$J) +
    seq(-0.45, 0.45, length.out = scenario$J) + scenario$threshold_shift
  quantiles <- stats::qnorm((seq_len(201) - 0.5) / 201) * scenario$theta_sd
  expected_sum <- vapply(latent, function(mu) {
      mean(vapply(mu + quantiles, function(theta) sum(stats::plogis(theta - as.vector(thresholds))), numeric(1)))
    }, numeric(1))
  max_score <- scenario$J * scenario$item_max
  proportion <- (expected_sum + 0.5) / (max_score + 1)
  fitted_values <- cbind(expected_sum, stats::qnorm(pmin(pmax(proportion, 1e-10), 1 - 1e-10)), latent)
  pseudo <- fitted_values[4, ] - fitted_values[2, ] - fitted_values[3, ] + fitted_values[1, ]
  pseudo[is.finite(pseudo) & abs(pseudo) < 1e-12] <- 0
  deterministic <- list(pseudo = unname(pseudo),
    response_did = (expected_sum[4] - expected_sum[2] - expected_sum[3] + expected_sum[1]) / max_score)
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
