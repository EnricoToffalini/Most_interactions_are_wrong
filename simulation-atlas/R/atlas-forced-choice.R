# Forced-choice atlas: local adaptation of scripts/03a-simulation-forced-choice.R.

# Model label -> fitted link. Single source of truth: the grid column, the
# replication rows, and the failure rows in 02-run-atlas.R all read this.
FORCED_LINKS <- c(
  "Gaussian identity" = "identity",
  "Standard binomial logit" = "logit",
  "Standard binomial probit" = "probit",
  "Chance-corrected binomial logit" = "chance-corrected logit"
)
FORCED_MODELS <- names(FORCED_LINKS)

atlas_forced_choice_rows <- function() {
  base <- list(
    family = "forced_choice",
    paper_anchor = FALSE,
    generating_model = "Chance-corrected binomial",
    generating_link = "chance-corrected logit",
    fitted_model = paste(FORCED_MODELS, collapse = ";"),
    fitted_link = paste(FORCED_LINKS, collapse = ";"),
    base_seed = ATLAS_BASE_SEED,
    seed_rule = ATLAS_SEED_RULE,
    B = 500L,
    alpha = ATLAS_ALPHA,
    N = 250L,
    k_trials = 20L,
    chance = 0.50,
    age_min = 6,
    age_max = 10,
    age_center = 8,
    beta_intercept = 0,
    beta_age = 0.60,
    beta_group = -0.90,
    beta_age_group = 0,
    generating_interaction = 0,
    beta_main_1_multiplier = 1,
    beta_main_2_multiplier = 1,
    varied_parameter = NA_character_,
    varied_value = NA_real_
  )
  make <- atlas_row_maker(base)

  rows <- list()
  anchor_values <- data.frame(
    scenario_label = c("Lower performance", "Middle performance", "Higher performance"),
    beta_intercept = c(-0.80, 0, 0.80),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(anchor_values))) {
    rows[[length(rows) + 1L]] <- make(
      "paper_anchor", anchor_values$scenario_label[i], TRUE,
      beta_intercept = anchor_values$beta_intercept[i],
      varied_parameter = "beta_intercept", varied_value = anchor_values$beta_intercept[i]
    )
  }

  surface <- atlas_main_effect_surface()
  for (i in seq_len(nrow(surface))) {
    m1 <- surface$beta_main_1_multiplier[i]
    m2 <- surface$beta_main_2_multiplier[i]
    rows[[length(rows) + 1L]] <- make(
      "main_effect_surface",
      sprintf("Age effect x %.2f; group effect x %.2f", m1, m2),
      beta_age = 0.60 * m1, beta_group = -0.90 * m2,
      beta_main_1_multiplier = m1, beta_main_2_multiplier = m2,
      varied_parameter = "beta_age_multiplier;beta_group_multiplier"
    )
  }

  # Denser below the manuscript anchor (250): small N is where applied studies
  # sit and where the extra scenarios cost almost nothing.
  for (value in c(50L, 100L, 150L, 200L, 250L, 500L, 1000L)) {
    rows[[length(rows) + 1L]] <- make(
      "sample_size", paste("N =", value), N = value,
      varied_parameter = "N", varied_value = value
    )
  }
  for (value in c(-1.50, -0.80, 0, 0.80, 1.50)) {
    rows[[length(rows) + 1L]] <- make(
      "scale_location", paste("Intercept =", format(value, nsmall = 2)),
      beta_intercept = value, varied_parameter = "beta_intercept", varied_value = value
    )
  }
  for (value in c(5L, 10L, 20L, 50L)) {
    rows[[length(rows) + 1L]] <- make(
      "trial_count", paste("Trials =", value), k_trials = value,
      varied_parameter = "k_trials", varied_value = value
    )
  }
  for (value in c(0.25, 1 / 3, 0.50)) {
    rows[[length(rows) + 1L]] <- make(
      "chance_level", paste("Chance =", format(value, digits = 4)), chance = value,
      varied_parameter = "chance", varied_value = value
    )
  }

  rows <- do.call(rbind, rows)
  atlas_dedupe_family(
    rows,
    c("N", "k_trials", "chance", "age_min", "age_max", "age_center",
      "beta_intercept", "beta_age", "beta_group", "beta_age_group",
      "generating_model", "generating_link"),
    family = "forced_choice"
  )
}

forced_eta <- function(scenario, age, group_num) {
  (age - scenario$age_center) * scenario$beta_age + scenario$beta_intercept +
    scenario$beta_group * group_num +
    scenario$beta_age_group * (age - scenario$age_center) * group_num
}

forced_expected_probability <- function(scenario, age, group_num) {
  atlas_chance_inv(forced_eta(scenario, age, group_num), scenario$chance)
}

forced_deterministic_quantities <- function(scenario, model_label) {
  grid <- expand.grid(age = c(scenario$age_min, scenario$age_max), group_num = c(0, 1))
  probability <- forced_expected_probability(scenario, grid$age, grid$group_num)
  fitted_value <- switch(
    model_label,
    "Gaussian identity" = probability,
    "Standard binomial logit" = stats::qlogis(atlas_clip_probability(probability)),
    "Standard binomial probit" = stats::qnorm(atlas_clip_probability(probability)),
    "Chance-corrected binomial logit" = stats::qlogis(atlas_clip_probability(
      (probability - scenario$chance) / (1 - scenario$chance)
    )),
    stop("Unknown forced-choice model: ", model_label, call. = FALSE)
  )
  list(
    deterministic_pseudo_interaction = atlas_zero_small(atlas_difference_in_differences(
      fitted_value, as.integer(grid$age == scenario$age_max), grid$group_num
    )),
    deterministic_response_scale_did = atlas_difference_in_differences(
      probability, as.integer(grid$age == scenario$age_max), grid$group_num
    )
  )
}

simulate_forced_choice <- function(scenario) {
  if (!is.finite(scenario$chance) || scenario$chance < 0 || scenario$chance >= 1) {
    stop("Invalid forced-choice chance level.", call. = FALSE)
  }
  group_num <- stats::rbinom(scenario$N, 1, 0.5)
  age <- stats::runif(scenario$N, scenario$age_min, scenario$age_max)
  age_c <- age - scenario$age_center
  probability <- forced_expected_probability(scenario, age, group_num)
  if (any(!is.finite(probability)) || any(probability < 0 | probability > 1)) {
    stop("Forced-choice generating probabilities are outside [0, 1].", call. = FALSE)
  }
  y <- stats::rbinom(scenario$N, scenario$k_trials, probability)
  data.frame(
    age = age,
    age_c = age_c,
    group_num = group_num,
    group = factor(group_num, levels = c(0, 1), labels = c("Group 0", "Group 1")),
    y = y,
    k = scenario$k_trials,
    accuracy = y / scenario$k_trials
  )
}

fit_forced_model <- function(data, scenario, model_label) {
  atlas_capture(switch(
    model_label,
    "Gaussian identity" = stats::lm(accuracy ~ age_c * group, data = data),
    "Standard binomial logit" = stats::glm(
      cbind(y, k - y) ~ age_c * group, family = stats::binomial("logit"), data = data
    ),
    "Standard binomial probit" = stats::glm(
      cbind(y, k - y) ~ age_c * group, family = stats::binomial("probit"), data = data
    ),
    "Chance-corrected binomial logit" = fit_atlas_chance_binomial(
      ~ age_c * group, data = data, chance = scenario$chance
    ),
    stop("Unknown forced-choice model: ", model_label, call. = FALSE)
  ))
}

forced_fitted_did <- function(fit, scenario, model_label) {
  if (is.null(fit)) return(c(response = NA_real_, outcome = NA_real_))
  grid <- expand.grid(
    age = c(scenario$age_min, scenario$age_max),
    group = factor(c("Group 0", "Group 1"), levels = c("Group 0", "Group 1"))
  )
  grid$age_c <- grid$age - scenario$age_center
  prediction <- try({
    if (model_label == "Gaussian identity") {
      stats::predict(fit, newdata = grid)
    } else {
      stats::predict(fit, newdata = grid, type = "response")
    }
  }, silent = TRUE)
  if (inherits(prediction, "try-error") || any(!is.finite(prediction))) {
    return(c(response = NA_real_, outcome = NA_real_))
  }
  did <- atlas_difference_in_differences(
    as.numeric(prediction), as.integer(grid$age == scenario$age_max),
    as.integer(grid$group == "Group 1")
  )
  c(response = did, outcome = did * scenario$k_trials)
}

run_forced_choice_replication <- function(scenario, replication) {
  set.seed(atlas_seed("forced_choice", scenario$scenario_id, replication))
  data <- simulate_forced_choice(scenario)
  rows <- lapply(FORCED_MODELS, function(model_label) {
    captured <- fit_forced_model(data, scenario, model_label)
    fit <- captured$value
    statistics <- atlas_interaction_stats(fit)
    problem <- atlas_fit_problem(fit)
    did <- forced_fitted_did(fit, scenario, model_label)
    deterministic <- forced_deterministic_quantities(scenario, model_label)
    data.frame(
      scenario_id = scenario$scenario_id,
      family = "forced_choice",
      replication = replication,
      replication_seed = atlas_seed("forced_choice", scenario$scenario_id, replication),
      model_label = model_label,
      fitted_link = unname(FORCED_LINKS[[model_label]]),
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
