# Atlas, sum scores: same DGP and models as scripts/03c-simulation-sum-scores.R,
# repeated over the sum_scores rows of the scenario grid.
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
scenarios <- grid[grid$family == "sum_scores", ]
if (mode == "smoke") scenarios <- scenarios[scenarios$scenario_id == "SS-002", ]

# deterministic item thresholds (J x 3)
make_thresholds <- function(J, threshold_shift) {
  matrix(rep(c(-1, 0, 1), each = J), nrow = J) + seq(-0.45, 0.45, length.out = J) + threshold_shift
}

sim_one <- function(replication, scenario, deterministic) {
  replication_seed <- as.integer((20260807 + 2000000 + as.double(sub(".*-", "", scenario$scenario_id)) * 10000 + replication) %% .Machine$integer.max)
  set.seed(replication_seed)
  N <- scenario$N
  J <- scenario$J
  max_score <- J * scenario$item_max
  x <- rnorm(N, 0, 1)
  group_num <- rbinom(N, 1, 0.5)
  theta <- scenario$beta_x * x + scenario$beta_group * group_num + scenario$beta_x_group * x * group_num + rnorm(N, 0, scenario$theta_sd)
  thresholds <- make_thresholds(J, scenario$threshold_shift)
  items <- matrix(NA, N, J)
  for (j in 1:J) {
    u <- runif(N)
    items[, j] <- (u < plogis(theta - thresholds[j, 1])) + (u < plogis(theta - thresholds[j, 2])) + (u < plogis(theta - thresholds[j, 3]))
  }
  d <- data.frame(x = x, group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")), theta = theta, sum_score = rowSums(items))
  # continuity-corrected proportion, strictly inside (0, 1), for the Gaussian-probit model
  d$score_prop_probit <- (d$sum_score + 0.5) / (max_score + 1)

  fits <- list(try(lm(sum_score ~ x * group, data = d), silent = TRUE),
               try(glm(score_prop_probit ~ x * group, family = gaussian(link = make.link("probit")), data = d), silent = TRUE),
               try(lm(theta ~ x * group, data = d), silent = TRUE))
  to_sum_score <- c(1, max_score, 1) # rescale bounded-score predictions to the sum-score metric
  nd <- expand.grid(x = c(scenario$x_min, scenario$x_max), group = factor(c("Group 0", "Group 1")))
  p_values <- coefs <- ses <- did <- rep(NA_real_, 3)
  problems <- rep(TRUE, 3)
  messages <- rep("", 3)
  for (j in 1:3) {
    if (inherits(fits[[j]], "try-error")) {
      messages[j] <- as.character(fits[[j]])
      next
    }
    problems[j] <- if (j == 2) !isTRUE(fits[[j]]$converged) else FALSE
    tab <- summary(fits[[j]])$coefficients
    if ("x:groupGroup 1" %in% rownames(tab)) {
      p_values[j] <- tab["x:groupGroup 1", 4]
      coefs[j] <- tab["x:groupGroup 1", 1]
      ses[j] <- tab["x:groupGroup 1", 2]
    }
    pred <- predict(fits[[j]], newdata = nd, type = "response") * to_sum_score[j]
    did[j] <- (pred[4] - pred[2]) - (pred[3] - pred[1])
  }
  data.frame(scenario_id = scenario$scenario_id, family = "sum_scores", replication = replication, replication_seed = replication_seed,
    model_label = c("Observed sum-score identity", "Gaussian-probit bounded score", "Latent generating scale"),
    fitted_link = c("identity", "probit", "identity latent benchmark"),
    interaction_p = p_values, interaction_coef = coefs, interaction_se = ses, response_scale_did = did / max_score, outcome_scale_did = did,
    deterministic_pseudo_interaction = deterministic$pseudo, deterministic_response_scale_did = deterministic$response_did,
    fit_success = is.finite(p_values) & !problems, convergence_problem = problems, problem_message = messages)
}

cl <- makeCluster(n_cores)
clusterExport(cl, "make_thresholds")
for (i in 1:nrow(scenarios)) {
  scenario <- scenarios[i, ]
  output_file <- sprintf("simulation-atlas/raw/core-%s-%s-B%d.rds", scenario$scenario_id, mode, B)
  if (file.exists(output_file) && !overwrite) {
    cat("Skipping existing scenario:", scenario$scenario_id, "\n")
    next
  }
  cat("Scenario:", scenario$scenario_id, "B =", B, "cores =", n_cores, "\n")

  # deterministic pseudo-interaction: expected sum scores (integrated over the latent
  # residual on a 201-point grid) read on the three fitted scales
  nd <- expand.grid(x = c(scenario$x_min, scenario$x_max), group_num = c(0, 1))
  latent <- scenario$beta_x * nd$x + scenario$beta_group * nd$group_num + scenario$beta_x_group * nd$x * nd$group_num
  thresholds <- make_thresholds(scenario$J, scenario$threshold_shift)
  q <- qnorm((1:201 - 0.5) / 201) * scenario$theta_sd
  expected_sum <- sapply(latent, function(mu) mean(sapply(mu + q, function(th) sum(plogis(th - as.vector(thresholds))))))
  max_score <- scenario$J * scenario$item_max
  proportion <- (expected_sum + 0.5) / (max_score + 1)
  scales <- cbind(expected_sum, qnorm(pmin(pmax(proportion, 1e-10), 1 - 1e-10)), latent)
  pseudo <- scales[4, ] - scales[2, ] - scales[3, ] + scales[1, ]
  pseudo[is.finite(pseudo) & abs(pseudo) < 1e-12] <- 0
  deterministic <- list(pseudo = unname(pseudo),
                        response_did = (expected_sum[4] - expected_sum[2] - expected_sum[3] + expected_sum[1]) / max_score)

  result <- do.call(rbind, parLapply(cl, 1:B, sim_one, scenario = scenario, deterministic = deterministic))
  result$B_requested <- B
  result$run_type <- mode
  result$base_seed <- 20260807L
  result$atlas_version <- "0.1.0"
  result$generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  saveRDS(result, output_file, compress = "xz")
}
stopCluster(cl)
