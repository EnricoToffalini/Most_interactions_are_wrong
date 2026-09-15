# Declared sensitivity grid. Run from the repository root.
# Anchor values are intentionally repeated here, independently of scripts/.
# Each unique scenario appears once, with explicit IDs and slice memberships.
# FULL publication setting: 3000 replications for every family.
# N_SIM is a runner override; this grid records the intended FULL design.
dir.create("simulation-atlas/data", recursive = TRUE, showWarnings = FALSE)
rows <- list()

# forced_choice
anchor <- data.frame(
  scenario_id = "FC-001",
  family = "forced_choice",
  slice = "paper_anchor",
  slice_membership = "paper_anchor;scale_location",
  paper_anchor = TRUE,
  scenario_label = "Lower performance",
  generating_model = "Chance-corrected binomial",
  generating_link = "chance-corrected logit",
  fitted_model = "Gaussian identity;Standard binomial logit;Standard binomial probit;Chance-corrected binomial logit",
  fitted_link = "identity;logit;probit;chance-corrected logit",
  base_seed = 20260807,
  seed_rule = "seed = (base_seed + family_offset + stream_offset + scenario_number * 10000 + replication) modulo .Machine$integer.max",
  B = 3000,
  alpha = 0.05,
  generating_interaction = 0,
  N = 250,
  k_trials = 20,
  chance = 0.5,
  age_min = 6,
  age_max = 10,
  age_center = 8,
  beta_intercept = -0.8,
  beta_age = 0.6,
  beta_group = -0.9,
  beta_age_group = 0,
  beta_main_1_multiplier = 1,
  beta_main_2_multiplier = 1,
  varied_parameter = "beta_intercept",
  varied_value = -0.8,
  J = NA,
  item_max = NA,
  theta_sd = NA,
  beta_x = NA,
  beta_x_group = NA,
  x_min = NA,
  x_max = NA,
  threshold_shift = NA,
  n_subjects = NA,
  target_icc = NA,
  reference_beta_intercept = NA,
  reference_beta_group = NA,
  reference_beta_condition = NA,
  reference_beta_group_condition = NA,
  logit_probit_scale = NA,
  coefficient_scale = NA,
  beta_condition = NA,
  beta_group_condition = NA,
  stringsAsFactors = FALSE)

row <- anchor
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-002"
row$slice_membership <- "paper_anchor;main_effect_surface;sample_size;scale_location;trial_count;chance_level"
row$scenario_label <- "Middle performance"
row$beta_intercept <- 0
row$varied_value <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-003"
row$scenario_label <- "Higher performance"
row$beta_intercept <- 0.8
row$varied_value <- 0.8
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-004"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 0.50; group effect x 0.50"
row$beta_intercept <- 0
row$beta_age <- 0.3
row$beta_group <- -0.45
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-005"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 1.00; group effect x 0.50"
row$beta_intercept <- 0
row$beta_group <- -0.45
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-006"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 1.50; group effect x 0.50"
row$beta_intercept <- 0
row$beta_age <- 0.9
row$beta_group <- -0.45
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-007"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 0.50; group effect x 1.00"
row$beta_intercept <- 0
row$beta_age <- 0.3
row$beta_main_1_multiplier <- 0.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-008"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 1.50; group effect x 1.00"
row$beta_intercept <- 0
row$beta_age <- 0.9
row$beta_main_1_multiplier <- 1.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-009"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 0.50; group effect x 1.50"
row$beta_intercept <- 0
row$beta_age <- 0.3
row$beta_group <- -1.35
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-010"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 1.00; group effect x 1.50"
row$beta_intercept <- 0
row$beta_group <- -1.35
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-011"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "Age effect x 1.50; group effect x 1.50"
row$beta_intercept <- 0
row$beta_age <- 0.9
row$beta_group <- -1.35
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_age_multiplier;beta_group_multiplier"
row$varied_value <- NA
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-012"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 50"
row$N <- 50
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 50
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-013"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 100"
row$N <- 100
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 100
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-014"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 150"
row$N <- 150
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 150
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-015"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 200"
row$N <- 200
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 200
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-016"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 500"
row$N <- 500
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 500
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-017"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 1000"
row$N <- 1000
row$beta_intercept <- 0
row$varied_parameter <- "N"
row$varied_value <- 1000
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-018"
row$slice <- "scale_location"
row$slice_membership <- "scale_location"
row$paper_anchor <- FALSE
row$scenario_label <- "Intercept = -1.50"
row$beta_intercept <- -1.5
row$varied_value <- -1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-019"
row$slice <- "scale_location"
row$slice_membership <- "scale_location"
row$paper_anchor <- FALSE
row$scenario_label <- "Intercept = 1.50"
row$beta_intercept <- 1.5
row$varied_value <- 1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-020"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "Trials = 5"
row$k_trials <- 5
row$beta_intercept <- 0
row$varied_parameter <- "k_trials"
row$varied_value <- 5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-021"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "Trials = 10"
row$k_trials <- 10
row$beta_intercept <- 0
row$varied_parameter <- "k_trials"
row$varied_value <- 10
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-022"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "Trials = 50"
row$k_trials <- 50
row$beta_intercept <- 0
row$varied_parameter <- "k_trials"
row$varied_value <- 50
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-023"
row$slice <- "chance_level"
row$slice_membership <- "chance_level"
row$paper_anchor <- FALSE
row$scenario_label <- "Chance = 0.25"
row$chance <- 0.25
row$beta_intercept <- 0
row$varied_parameter <- "chance"
row$varied_value <- 0.25
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "FC-024"
row$slice <- "chance_level"
row$slice_membership <- "chance_level"
row$paper_anchor <- FALSE
row$scenario_label <- "Chance = 0.3333"
row$chance <- 0.333333333333333
row$beta_intercept <- 0
row$varied_parameter <- "chance"
row$varied_value <- 0.333333333333333
rows[[length(rows) + 1L]] <- row


# sum_scores
anchor <- data.frame(
  scenario_id = "SS-001",
  family = "sum_scores",
  slice = "paper_anchor",
  slice_membership = "paper_anchor;scale_location",
  paper_anchor = TRUE,
  scenario_label = "Lower range",
  generating_model = "Deterministic graded-response item model",
  generating_link = "graded logistic latent scale",
  fitted_model = "Observed sum-score identity;Gaussian-probit bounded score;Latent generating scale",
  fitted_link = "identity;probit;identity latent benchmark",
  base_seed = 20260807,
  seed_rule = "seed = (base_seed + family_offset + stream_offset + scenario_number * 10000 + replication) modulo .Machine$integer.max",
  B = 3000,
  alpha = 0.05,
  generating_interaction = 0,
  N = 600,
  k_trials = NA,
  chance = NA,
  age_min = NA,
  age_max = NA,
  age_center = NA,
  beta_intercept = NA,
  beta_age = NA,
  beta_group = -0.9,
  beta_age_group = NA,
  beta_main_1_multiplier = 1,
  beta_main_2_multiplier = 1,
  varied_parameter = "threshold_shift",
  varied_value = 1.6,
  J = 9,
  item_max = 3,
  theta_sd = 1,
  beta_x = 0.85,
  beta_x_group = 0,
  x_min = -1,
  x_max = 1,
  threshold_shift = 1.6,
  n_subjects = NA,
  target_icc = NA,
  reference_beta_intercept = NA,
  reference_beta_group = NA,
  reference_beta_condition = NA,
  reference_beta_group_condition = NA,
  logit_probit_scale = NA,
  coefficient_scale = NA,
  beta_condition = NA,
  beta_group_condition = NA,
  stringsAsFactors = FALSE)

row <- anchor
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-002"
row$slice_membership <- "paper_anchor;main_effect_surface;sample_size;scale_location;item_count;latent_dispersion"
row$scenario_label <- "Middle range"
row$varied_value <- 0
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-003"
row$scenario_label <- "Upper range"
row$varied_value <- -1.6
row$threshold_shift <- -1.6
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-004"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 0.50; group effect x 0.50"
row$beta_group <- -0.45
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 0.425
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-005"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 1.00; group effect x 0.50"
row$beta_group <- -0.45
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-006"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 1.50; group effect x 0.50"
row$beta_group <- -0.45
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 1.275
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-007"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 0.50; group effect x 1.00"
row$beta_main_1_multiplier <- 0.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 0.425
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-008"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 1.50; group effect x 1.00"
row$beta_main_1_multiplier <- 1.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 1.275
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-009"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 0.50; group effect x 1.50"
row$beta_group <- -1.35
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 0.425
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-010"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 1.00; group effect x 1.50"
row$beta_group <- -1.35
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-011"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "x effect x 1.50; group effect x 1.50"
row$beta_group <- -1.35
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_x_multiplier;beta_group_multiplier"
row$varied_value <- NA
row$beta_x <- 1.275
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-012"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 75"
row$N <- 75
row$varied_parameter <- "N"
row$varied_value <- 75
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-013"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 150"
row$N <- 150
row$varied_parameter <- "N"
row$varied_value <- 150
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-014"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 225"
row$N <- 225
row$varied_parameter <- "N"
row$varied_value <- 225
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-015"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 300"
row$N <- 300
row$varied_parameter <- "N"
row$varied_value <- 300
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-016"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 450"
row$N <- 450
row$varied_parameter <- "N"
row$varied_value <- 450
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-017"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "N = 1200"
row$N <- 1200
row$varied_parameter <- "N"
row$varied_value <- 1200
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-018"
row$slice <- "scale_location"
row$slice_membership <- "scale_location"
row$paper_anchor <- FALSE
row$scenario_label <- "Threshold shift = -2.00"
row$varied_value <- -2
row$threshold_shift <- -2
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-019"
row$slice <- "scale_location"
row$slice_membership <- "scale_location"
row$paper_anchor <- FALSE
row$scenario_label <- "Threshold shift = 2.00"
row$varied_value <- 2
row$threshold_shift <- 2
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-020"
row$slice <- "item_count"
row$slice_membership <- "item_count"
row$paper_anchor <- FALSE
row$scenario_label <- "Items = 5"
row$varied_parameter <- "J"
row$varied_value <- 5
row$J <- 5
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-021"
row$slice <- "item_count"
row$slice_membership <- "item_count"
row$paper_anchor <- FALSE
row$scenario_label <- "Items = 15"
row$varied_parameter <- "J"
row$varied_value <- 15
row$J <- 15
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-022"
row$slice <- "latent_dispersion"
row$slice_membership <- "latent_dispersion"
row$paper_anchor <- FALSE
row$scenario_label <- "Latent SD = 0.70"
row$varied_parameter <- "theta_sd"
row$varied_value <- 0.7
row$theta_sd <- 0.7
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "SS-023"
row$slice <- "latent_dispersion"
row$slice_membership <- "latent_dispersion"
row$paper_anchor <- FALSE
row$scenario_label <- "Latent SD = 1.30"
row$varied_parameter <- "theta_sd"
row$varied_value <- 1.3
row$theta_sd <- 1.3
row$threshold_shift <- 0
rows[[length(rows) + 1L]] <- row


# within_family
anchor <- data.frame(
  scenario_id = "WF-001",
  family = "within_family",
  slice = "paper_anchor",
  slice_membership = "paper_anchor;main_effect_surface;sample_size;trial_count;latent_location;icc",
  paper_anchor = TRUE,
  scenario_label = "Logit-generated manuscript scenario",
  generating_model = "Random-intercept binomial",
  generating_link = "logit",
  fitted_model = "Binomial logit;Binomial probit",
  fitted_link = "logit;probit",
  base_seed = 20260807,
  seed_rule = "seed = (base_seed + family_offset + stream_offset + scenario_number * 10000 + replication) modulo .Machine$integer.max",
  B = 3000,
  alpha = 0.05,
  generating_interaction = 0,
  N = NA,
  k_trials = 15,
  chance = NA,
  age_min = NA,
  age_max = NA,
  age_center = NA,
  beta_intercept = 2.475,
  beta_age = NA,
  beta_group = -1.65,
  beta_age_group = NA,
  beta_main_1_multiplier = 1,
  beta_main_2_multiplier = 1,
  varied_parameter = NA,
  varied_value = NA,
  J = NA,
  item_max = NA,
  theta_sd = NA,
  beta_x = NA,
  beta_x_group = NA,
  x_min = NA,
  x_max = NA,
  threshold_shift = NA,
  n_subjects = 700,
  target_icc = 0.3,
  reference_beta_intercept = 1.5,
  reference_beta_group = -1,
  reference_beta_condition = -1,
  reference_beta_group_condition = 0,
  logit_probit_scale = 1.65,
  coefficient_scale = 1.65,
  beta_condition = -1.65,
  beta_group_condition = 0,
  stringsAsFactors = FALSE)

row <- anchor
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-002"
row$scenario_label <- "Probit-generated manuscript scenario"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-003"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 0.50; condition x 0.50"
row$beta_group <- -0.825
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
row$reference_beta_condition <- -0.5
row$beta_condition <- -0.825
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-004"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 1.00; condition x 0.50"
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_condition <- -0.5
row$beta_condition <- -0.825
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-005"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 1.50; condition x 0.50"
row$beta_group <- -2.475
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
row$reference_beta_condition <- -0.5
row$beta_condition <- -0.825
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-006"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 0.50; condition x 1.00"
row$beta_group <- -0.825
row$beta_main_1_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-007"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 1.50; condition x 1.00"
row$beta_group <- -2.475
row$beta_main_1_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-008"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 0.50; condition x 1.50"
row$beta_group <- -0.825
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
row$reference_beta_condition <- -1.5
row$beta_condition <- -2.475
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-009"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 1.00; condition x 1.50"
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_condition <- -1.5
row$beta_condition <- -2.475
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-010"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: group x 1.50; condition x 1.50"
row$beta_group <- -2.475
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
row$reference_beta_condition <- -1.5
row$beta_condition <- -2.475
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-011"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 0.50; condition x 0.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -0.5
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
row$reference_beta_condition <- -0.5
row$coefficient_scale <- 1
row$beta_condition <- -0.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-012"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 1.00; condition x 0.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_condition <- -0.5
row$coefficient_scale <- 1
row$beta_condition <- -0.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-013"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 1.50; condition x 0.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1.5
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
row$reference_beta_condition <- -0.5
row$coefficient_scale <- 1
row$beta_condition <- -0.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-014"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 0.50; condition x 1.00"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -0.5
row$beta_main_1_multiplier <- 0.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-015"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 1.50; condition x 1.00"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1.5
row$beta_main_1_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-016"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 0.50; condition x 1.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -0.5
row$beta_main_1_multiplier <- 0.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -0.5
row$reference_beta_condition <- -1.5
row$coefficient_scale <- 1
row$beta_condition <- -1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-017"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 1.00; condition x 1.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_condition <- -1.5
row$coefficient_scale <- 1
row$beta_condition <- -1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-018"
row$slice <- "main_effect_surface"
row$slice_membership <- "main_effect_surface"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: group x 1.50; condition x 1.50"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1.5
row$beta_main_1_multiplier <- 1.5
row$beta_main_2_multiplier <- 1.5
row$varied_parameter <- "beta_group_multiplier;beta_condition_multiplier"
row$reference_beta_group <- -1.5
row$reference_beta_condition <- -1.5
row$coefficient_scale <- 1
row$beta_condition <- -1.5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-019"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 100"
row$varied_parameter <- "n_subjects"
row$varied_value <- 100
row$n_subjects <- 100
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-020"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 200"
row$varied_parameter <- "n_subjects"
row$varied_value <- 200
row$n_subjects <- 200
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-021"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 300"
row$varied_parameter <- "n_subjects"
row$varied_value <- 300
row$n_subjects <- 300
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-022"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 400"
row$varied_parameter <- "n_subjects"
row$varied_value <- 400
row$n_subjects <- 400
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-023"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 1200"
row$varied_parameter <- "n_subjects"
row$varied_value <- 1200
row$n_subjects <- 1200
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-024"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: subjects = 2000"
row$varied_parameter <- "n_subjects"
row$varied_value <- 2000
row$n_subjects <- 2000
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-025"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: trials per cell = 5"
row$k_trials <- 5
row$varied_parameter <- "k_trials"
row$varied_value <- 5
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-026"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: trials per cell = 30"
row$k_trials <- 30
row$varied_parameter <- "k_trials"
row$varied_value <- 30
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-027"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: reference intercept = 0"
row$beta_intercept <- 0
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 0
row$reference_beta_intercept <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-028"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: reference intercept = 0.75"
row$beta_intercept <- 1.2375
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 0.75
row$reference_beta_intercept <- 0.75
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-029"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: reference intercept = 2.25"
row$beta_intercept <- 3.7125
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 2.25
row$reference_beta_intercept <- 2.25
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-030"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: target ICC = 0"
row$varied_parameter <- "target_icc"
row$varied_value <- 0
row$target_icc <- 0
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-031"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: target ICC = 0.15"
row$varied_parameter <- "target_icc"
row$varied_value <- 0.15
row$target_icc <- 0.15
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-032"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "logit generated: target ICC = 0.45"
row$varied_parameter <- "target_icc"
row$varied_value <- 0.45
row$target_icc <- 0.45
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-033"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 100"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 100
row$n_subjects <- 100
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-034"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 200"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 200
row$n_subjects <- 200
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-035"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 300"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 300
row$n_subjects <- 300
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-036"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 400"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 400
row$n_subjects <- 400
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-037"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 1200"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 1200
row$n_subjects <- 1200
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-038"
row$slice <- "sample_size"
row$slice_membership <- "sample_size"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: subjects = 2000"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "n_subjects"
row$varied_value <- 2000
row$n_subjects <- 2000
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-039"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: trials per cell = 5"
row$generating_link <- "probit"
row$k_trials <- 5
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "k_trials"
row$varied_value <- 5
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-040"
row$slice <- "trial_count"
row$slice_membership <- "trial_count"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: trials per cell = 30"
row$generating_link <- "probit"
row$k_trials <- 30
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "k_trials"
row$varied_value <- 30
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-041"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: reference intercept = 0"
row$generating_link <- "probit"
row$beta_intercept <- 0
row$beta_group <- -1
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 0
row$reference_beta_intercept <- 0
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-042"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: reference intercept = 0.75"
row$generating_link <- "probit"
row$beta_intercept <- 0.75
row$beta_group <- -1
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 0.75
row$reference_beta_intercept <- 0.75
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-043"
row$slice <- "latent_location"
row$slice_membership <- "latent_location"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: reference intercept = 2.25"
row$generating_link <- "probit"
row$beta_intercept <- 2.25
row$beta_group <- -1
row$varied_parameter <- "reference_beta_intercept"
row$varied_value <- 2.25
row$reference_beta_intercept <- 2.25
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-044"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: target ICC = 0"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "target_icc"
row$varied_value <- 0
row$target_icc <- 0
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-045"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: target ICC = 0.15"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "target_icc"
row$varied_value <- 0.15
row$target_icc <- 0.15
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

row <- anchor
row$scenario_id <- "WF-046"
row$slice <- "icc"
row$slice_membership <- "icc"
row$paper_anchor <- FALSE
row$scenario_label <- "probit generated: target ICC = 0.45"
row$generating_link <- "probit"
row$beta_intercept <- 1.5
row$beta_group <- -1
row$varied_parameter <- "target_icc"
row$varied_value <- 0.45
row$target_icc <- 0.45
row$coefficient_scale <- 1
row$beta_condition <- -1
rows[[length(rows) + 1L]] <- row

grid <- do.call(rbind, rows)
rownames(grid) <- NULL
stopifnot(all(grid$generating_interaction == 0))
utils::write.csv(grid, "simulation-atlas/data/scenario-grid.csv", row.names = FALSE, na = "")
cat("Wrote", nrow(grid), "core scenarios.\n")

# Diagnostic sensitivities, in the existing D-FC / D-WF order.
# These rows deliberately retain the core-anchor metadata carried by the old plan.
diagnostic_rows <- list()
forced <- grid[grid$scenario_id == "FC-001", ]
for (i in seq_along(c(100, 250, 500, 1000, 250, 250, 250, 250))) {
  row <- forced
  row$N <- c(100, 250, 500, 1000, 250, 250, 250, 250)[i]
  row$beta_intercept <- c(-0.8, -0.8, -0.8, -0.8, -1.5, 0, 0.8, 1.5)[i]
  row$scenario_id <- sprintf("D-FC-%03d", i)
  row$diagnostic_source_slice <- if (i <= 4) "sample_size" else "severity"
  row$diagnostic_paper_anchor <- i == 2
  row$diagnostic_slice_membership <- if (i == 2) "diagnostic_anchor;sample_size;severity" else row$diagnostic_source_slice
  row$wrong_fitted_link <- "logit"
  row$wrong_model_label <- "Standard binomial logit"
  row$diagnostic_note <- "Paper framework: chance-corrected logit generated, standard logit fitted."
  diagnostic_rows[[length(diagnostic_rows) + 1L]] <- row
}
row <- grid[grid$scenario_id == "SS-002", ]
row$scenario_id <- "D-SS-001"
row$diagnostic_source_slice <- "diagnostic_not_defined"
row$diagnostic_paper_anchor <- FALSE
row$diagnostic_slice_membership <- "diagnostic_not_defined"
row$wrong_fitted_link <- NA_character_
row$wrong_model_label <- "No comparable paper diagnostic"
row$diagnostic_note <- paste("The paper does not compare diagnostics for the sum-score models because",
  "their response variables/scales do not support a same-likelihood AIC comparison.")
diagnostic_rows[[length(diagnostic_rows) + 1L]] <- row

within <- grid[grid$scenario_id == "WF-002", ]
for (i in seq_along(c(200, 300, 700, 1200, 300, 300, 300))) {
  row <- within
  row$n_subjects <- c(200, 300, 700, 1200, 300, 300, 300)[i]
  row$reference_beta_intercept <- c(1.5, 1.5, 1.5, 1.5, 0, 0.75, 2.25)[i]
  row$beta_intercept <- row$reference_beta_intercept
  row$scenario_id <- sprintf("D-WF-%03d", i)
  row$diagnostic_source_slice <- if (i <= 4) "sample_size" else "severity"
  row$diagnostic_paper_anchor <- i == 2
  row$diagnostic_slice_membership <- if (i == 2) "diagnostic_anchor;sample_size;severity" else row$diagnostic_source_slice
  row$wrong_fitted_link <- "logit"
  row$wrong_model_label <- "Binomial logit GLMM"
  row$diagnostic_note <- "Exact paper diagnostic anchor uses 300 subjects; probit generated, logit fitted."
  diagnostic_rows[[length(diagnostic_rows) + 1L]] <- row
}
diagnostic_grid <- do.call(rbind, diagnostic_rows)
diagnostic_grid$paper_anchor <- diagnostic_grid$diagnostic_paper_anchor
diagnostic_grid$slice <- diagnostic_grid$diagnostic_source_slice
diagnostic_grid$slice_membership <- diagnostic_grid$diagnostic_slice_membership
utils::write.csv(diagnostic_grid, "simulation-atlas/data/diagnostic-grid.csv", row.names = FALSE, na = "")
cat("Wrote", nrow(diagnostic_grid), "diagnostic design rows (sum scores explicitly inapplicable).\n")
