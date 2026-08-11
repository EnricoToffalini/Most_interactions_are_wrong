# Sum-score atlas: local adaptation of scripts/03c-simulation-sum-scores.R.

# Model label -> fitted link; see the note on FORCED_LINKS.
SUM_SCORE_LINKS <- c(
  "Observed sum-score identity" = "identity",
  "Gaussian-probit bounded score" = "probit",
  "Latent generating scale" = "identity latent benchmark"
)
SUM_SCORE_MODELS <- names(SUM_SCORE_LINKS)

atlas_sum_score_rows <- function() {
  base <- list(
    family = "sum_scores",
    paper_anchor = FALSE,
    generating_model = "Deterministic graded-response item model",
    generating_link = "graded logistic latent scale",
    fitted_model = paste(SUM_SCORE_MODELS, collapse = ";"),
    fitted_link = paste(SUM_SCORE_LINKS, collapse = ";"),
    base_seed = ATLAS_BASE_SEED,
    seed_rule = ATLAS_SEED_RULE,
    B = 500L,
    alpha = ATLAS_ALPHA,
    N = 600L,
    J = 9L,
    item_max = 3L,
    theta_sd = 1,
    beta_x = 0.85,
    beta_group = -0.90,
    beta_x_group = 0,
    x_min = -1,
    x_max = 1,
    threshold_shift = 0,
    generating_interaction = 0,
    beta_main_1_multiplier = 1,
    beta_main_2_multiplier = 1,
    varied_parameter = NA_character_,
    varied_value = NA_real_
  )
  make <- atlas_row_maker(base)

  rows <- list()
  anchors <- data.frame(
    scenario_label = c("Lower range", "Middle range", "Upper range"),
    threshold_shift = c(1.60, 0, -1.60),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(anchors))) {
    rows[[length(rows) + 1L]] <- make(
      "paper_anchor", anchors$scenario_label[i], TRUE,
      threshold_shift = anchors$threshold_shift[i],
      varied_parameter = "threshold_shift", varied_value = anchors$threshold_shift[i]
    )
  }

  surface <- atlas_main_effect_surface()
  for (i in seq_len(nrow(surface))) {
    m1 <- surface$beta_main_1_multiplier[i]
    m2 <- surface$beta_main_2_multiplier[i]
    rows[[length(rows) + 1L]] <- make(
      "main_effect_surface",
      sprintf("x effect x %.2f; group effect x %.2f", m1, m2),
      beta_x = 0.85 * m1, beta_group = -0.90 * m2,
      beta_main_1_multiplier = m1, beta_main_2_multiplier = m2,
      varied_parameter = "beta_x_multiplier;beta_group_multiplier"
    )
  }

  # Denser below the manuscript anchor (600); see the forced-choice note.
  for (value in c(75L, 150L, 225L, 300L, 450L, 600L, 1200L)) {
    rows[[length(rows) + 1L]] <- make(
      "sample_size", paste("N =", value), N = value,
      varied_parameter = "N", varied_value = value
    )
  }
  for (value in c(-2.00, -1.60, 0, 1.60, 2.00)) {
    rows[[length(rows) + 1L]] <- make(
      "scale_location", paste("Threshold shift =", format(value, nsmall = 2)),
      threshold_shift = value, varied_parameter = "threshold_shift", varied_value = value
    )
  }
  for (value in c(5L, 9L, 15L)) {
    rows[[length(rows) + 1L]] <- make(
      "item_count", paste("Items =", value), J = value,
      varied_parameter = "J", varied_value = value
    )
  }
  for (value in c(0.70, 1.00, 1.30)) {
    rows[[length(rows) + 1L]] <- make(
      "latent_dispersion", paste("Latent SD =", format(value, nsmall = 2)),
      theta_sd = value, varied_parameter = "theta_sd", varied_value = value
    )
  }

  rows <- do.call(rbind, rows)
  atlas_dedupe_family(
    rows,
    c("N", "J", "item_max", "theta_sd", "beta_x", "beta_group",
      "beta_x_group", "x_min", "x_max", "threshold_shift",
      "generating_model", "generating_link"),
    family = "sum_scores"
  )
}

sum_score_thresholds <- function(scenario) {
  if (scenario$item_max != 3L) {
    stop("The atlas preserves the manuscript's four ordinal categories (item_max = 3).",
         call. = FALSE)
  }
  offsets <- seq(-0.45, 0.45, length.out = scenario$J)
  thresholds <- matrix(rep(c(-1, 0, 1), each = scenario$J), nrow = scenario$J)
  thresholds <- thresholds + offsets + scenario$threshold_shift
  if (any(apply(thresholds, 1, function(x) any(diff(x) <= 0)))) {
    stop("Sum-score thresholds are not strictly ordered.", call. = FALSE)
  }
  thresholds
}

sum_score_latent_mean <- function(scenario, x, group_num) {
  scenario$beta_x * x + scenario$beta_group * group_num +
    scenario$beta_x_group * x * group_num
}

sum_score_expected_given_mu <- function(scenario, mu, thresholds = sum_score_thresholds(scenario),
                                        quadrature_n = 201L) {
  quantiles <- stats::qnorm((seq_len(quadrature_n) - 0.5) / quadrature_n) * scenario$theta_sd
  vapply(mu, function(one_mu) {
    mean(vapply(one_mu + quantiles, function(theta) {
      sum(stats::plogis(theta - as.vector(thresholds)))
    }, numeric(1)))
  }, numeric(1))
}

sum_score_deterministic_quantities <- function(scenario, model_label) {
  grid <- expand.grid(x = c(scenario$x_min, scenario$x_max), group_num = c(0, 1))
  latent <- sum_score_latent_mean(scenario, grid$x, grid$group_num)
  expected_sum <- sum_score_expected_given_mu(scenario, latent)
  max_score <- scenario$J * scenario$item_max
  fitted_value <- switch(
    model_label,
    "Observed sum-score identity" = expected_sum,
    "Gaussian-probit bounded score" = stats::qnorm(atlas_clip_probability(
      (expected_sum + 0.5) / (max_score + 1)
    )),
    "Latent generating scale" = latent,
    stop("Unknown sum-score model: ", model_label, call. = FALSE)
  )
  list(
    deterministic_pseudo_interaction = atlas_zero_small(atlas_difference_in_differences(
      fitted_value, as.integer(grid$x == scenario$x_max), grid$group_num
    )),
    deterministic_response_scale_did = atlas_difference_in_differences(
      expected_sum / max_score, as.integer(grid$x == scenario$x_max), grid$group_num
    ),
    deterministic_outcome_scale_did = atlas_difference_in_differences(
      expected_sum, as.integer(grid$x == scenario$x_max), grid$group_num
    )
  )
}

simulate_sum_scores <- function(scenario) {
  thresholds <- sum_score_thresholds(scenario)
  x <- stats::rnorm(scenario$N)
  group_num <- stats::rbinom(scenario$N, 1, 0.5)
  latent_mean <- sum_score_latent_mean(scenario, x, group_num)
  theta <- latent_mean + stats::rnorm(scenario$N, sd = scenario$theta_sd)
  items <- matrix(NA_integer_, nrow = scenario$N, ncol = scenario$J)
  for (j in seq_len(scenario$J)) {
    p_ge <- vapply(thresholds[j, ], function(threshold) stats::plogis(theta - threshold),
                   numeric(scenario$N))
    if (any(!is.finite(p_ge)) || any(p_ge < 0 | p_ge > 1)) {
      stop("Invalid graded-response probability.", call. = FALSE)
    }
    draw <- stats::runif(scenario$N)
    items[, j] <- rowSums(draw < p_ge)
  }
  sum_score <- rowSums(items)
  max_score <- scenario$J * scenario$item_max
  data.frame(
    x = x,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    latent_mean = latent_mean,
    theta = theta,
    sum_score = sum_score,
    score_prop = sum_score / max_score,
    score_prop_probit = (sum_score + 0.5) / (max_score + 1),
    max_score = max_score
  )
}

fit_sum_score_model <- function(data, model_label) {
  atlas_capture(switch(
    model_label,
    "Observed sum-score identity" = stats::lm(sum_score ~ x * group, data = data),
    "Gaussian-probit bounded score" = stats::glm(
      score_prop_probit ~ x * group,
      family = stats::gaussian(link = stats::make.link("probit")), data = data
    ),
    "Latent generating scale" = stats::lm(theta ~ x * group, data = data),
    stop("Unknown sum-score model: ", model_label, call. = FALSE)
  ))
}

sum_score_fitted_did <- function(fit, scenario, model_label) {
  if (is.null(fit)) return(c(response = NA_real_, outcome = NA_real_))
  grid <- expand.grid(
    x = c(scenario$x_min, scenario$x_max),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  prediction <- try({
    if (model_label == "Gaussian-probit bounded score") {
      stats::predict(fit, newdata = grid, type = "response") * (scenario$J * scenario$item_max)
    } else {
      stats::predict(fit, newdata = grid)
    }
  }, silent = TRUE)
  if (inherits(prediction, "try-error") || any(!is.finite(prediction))) {
    return(c(response = NA_real_, outcome = NA_real_))
  }
  did <- atlas_difference_in_differences(
    as.numeric(prediction), as.integer(grid$x == scenario$x_max),
    as.integer(grid$group == "Group 1")
  )
  scale <- scenario$J * scenario$item_max
  c(response = did / scale, outcome = did)
}

run_sum_score_replication <- function(scenario, replication) {
  set.seed(atlas_seed("sum_scores", scenario$scenario_id, replication))
  data <- simulate_sum_scores(scenario)
  rows <- lapply(SUM_SCORE_MODELS, function(model_label) {
    captured <- fit_sum_score_model(data, model_label)
    fit <- captured$value
    statistics <- atlas_interaction_stats(fit)
    problem <- atlas_fit_problem(fit)
    did <- sum_score_fitted_did(fit, scenario, model_label)
    deterministic <- sum_score_deterministic_quantities(scenario, model_label)
    data.frame(
      scenario_id = scenario$scenario_id,
      family = "sum_scores",
      replication = replication,
      replication_seed = atlas_seed("sum_scores", scenario$scenario_id, replication),
      model_label = model_label,
      fitted_link = unname(SUM_SCORE_LINKS[[model_label]]),
      interaction_p = statistics$p,
      interaction_coef = statistics$coef,
      interaction_se = statistics$se,
      response_scale_did = unname(did["response"]),
      outcome_scale_did = unname(did["outcome"]),
      deterministic_pseudo_interaction = deterministic$deterministic_pseudo_interaction,
      deterministic_response_scale_did = deterministic$deterministic_response_scale_did,
      fit_success = is.finite(statistics$p) && !problem,
      convergence_problem = problem,
      problem_message = atlas_problem_text(captured, fit),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
