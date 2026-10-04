# Declared sensitivity grid of the simulation atlas. Run from the repository root.
# Anchor values are deliberately repeated here, independently of scripts/.
# Each scenario appears once, with explicit ID and slice memberships.
# B = 3000 is the intended full design; N_SIM is only a runner override.
# All generating interactions are zero.

dir.create("simulation-atlas/data", recursive = TRUE, showWarnings = FALSE)

columns <- c("scenario_id", "family", "slice", "slice_membership", "paper_anchor", "scenario_label", "generating_model",
  "generating_link", "fitted_model", "fitted_link", "base_seed", "seed_rule", "B", "alpha", "generating_interaction",
  "N", "k_trials", "chance", "age_min", "age_max", "age_center", "beta_intercept", "beta_age", "beta_group",
  "beta_age_group", "beta_main_1_multiplier", "beta_main_2_multiplier", "varied_parameter", "varied_value",
  "J", "item_max", "theta_sd", "beta_x", "beta_x_group", "x_min", "x_max", "threshold_shift", "n_subjects", "target_icc",
  "reference_beta_intercept", "reference_beta_group", "reference_beta_condition", "reference_beta_group_condition",
  "logit_probit_scale", "coefficient_scale", "beta_condition", "beta_group_condition")
# columns not used by a family stay NA
add_missing <- function(df) {
  for (v in setdiff(columns, names(df))) df[[v]] <- NA
  df[, columns]
}

# main-effect surface: multipliers of the two main effects, in this order in every family
m1 <- c(0.5, 1, 1.5, 0.5, 1.5, 0.5, 1, 1.5)
m2 <- c(0.5, 0.5, 0.5, 1, 1, 1.5, 1.5, 1.5)
mult_label <- function(x) sprintf("%.2f", x)

####################################################
# Forced choice (FC-001 to FC-024)
####################################################

fc <- rbind(
  data.frame(slice = "paper_anchor", scenario_label = c("Lower performance", "Middle performance", "Higher performance"),
    N = 250, k_trials = 20, chance = 0.5, beta_intercept = c(-0.8, 0, 0.8), beta_age = 0.6, beta_group = -0.9,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "beta_intercept", varied_value = c(-0.8, 0, 0.8)),
  data.frame(slice = "main_effect_surface", scenario_label = paste0("Age effect x ", mult_label(m1), "; group effect x ", mult_label(m2)),
    N = 250, k_trials = 20, chance = 0.5, beta_intercept = 0,
    beta_age = c(0.3, 0.6, 0.9)[match(m1, c(0.5, 1, 1.5))], beta_group = c(-0.45, -0.9, -1.35)[match(m2, c(0.5, 1, 1.5))],
    beta_main_1_multiplier = m1, beta_main_2_multiplier = m2, varied_parameter = "beta_age_multiplier;beta_group_multiplier", varied_value = NA),
  data.frame(slice = "sample_size", scenario_label = paste("N =", c(50, 100, 150, 200, 500, 1000)),
    N = c(50, 100, 150, 200, 500, 1000), k_trials = 20, chance = 0.5, beta_intercept = 0, beta_age = 0.6, beta_group = -0.9,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "N", varied_value = c(50, 100, 150, 200, 500, 1000)),
  data.frame(slice = "scale_location", scenario_label = c("Intercept = -1.50", "Intercept = 1.50"),
    N = 250, k_trials = 20, chance = 0.5, beta_intercept = c(-1.5, 1.5), beta_age = 0.6, beta_group = -0.9,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "beta_intercept", varied_value = c(-1.5, 1.5)),
  data.frame(slice = "trial_count", scenario_label = paste("Trials =", c(5, 10, 50)),
    N = 250, k_trials = c(5, 10, 50), chance = 0.5, beta_intercept = 0, beta_age = 0.6, beta_group = -0.9,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "k_trials", varied_value = c(5, 10, 50)),
  data.frame(slice = "chance_level", scenario_label = c("Chance = 0.25", "Chance = 0.3333"),
    N = 250, k_trials = 20, chance = c(0.25, 0.333333333333333), beta_intercept = 0, beta_age = 0.6, beta_group = -0.9,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "chance", varied_value = c(0.25, 0.333333333333333))
)
fc$scenario_id <- sprintf("FC-%03d", 1:nrow(fc))
fc$family <- "forced_choice"
fc$slice_membership <- fc$slice
fc$slice_membership[1:3] <- c("paper_anchor;scale_location",
                              "paper_anchor;main_effect_surface;sample_size;scale_location;trial_count;chance_level",
                              "paper_anchor;scale_location")
fc$paper_anchor <- fc$slice == "paper_anchor"
fc$generating_model <- "Chance-corrected binomial random-intercept model"
fc$generating_link <- "chance-corrected logit"
fc$fitted_model <- "Gaussian identity;Standard binomial logit;Standard binomial probit;Chance-corrected binomial logit"
fc$fitted_link <- "identity;logit;probit;chance-corrected logit"
fc$age_min <- 6
fc$age_max <- 10
fc$age_center <- 8
fc$beta_age_group <- 0
fc$target_icc <- 0.30

####################################################
# Sum scores (SS-001 to SS-023)
####################################################

ss <- rbind(
  data.frame(slice = "paper_anchor", scenario_label = c("Lower range", "Middle range", "Upper range"),
    N = 600, J = 9, theta_sd = 1, beta_x = 0.85, beta_group = -0.9, threshold_shift = c(1.6, 0, -1.6),
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "threshold_shift", varied_value = c(1.6, 0, -1.6)),
  data.frame(slice = "main_effect_surface", scenario_label = paste0("x effect x ", mult_label(m1), "; group effect x ", mult_label(m2)),
    N = 600, J = 9, theta_sd = 1, beta_x = c(0.425, 0.85, 1.275)[match(m1, c(0.5, 1, 1.5))],
    beta_group = c(-0.45, -0.9, -1.35)[match(m2, c(0.5, 1, 1.5))], threshold_shift = 0,
    beta_main_1_multiplier = m1, beta_main_2_multiplier = m2, varied_parameter = "beta_x_multiplier;beta_group_multiplier", varied_value = NA),
  data.frame(slice = "sample_size", scenario_label = paste("N =", c(75, 150, 225, 300, 450, 1200)),
    N = c(75, 150, 225, 300, 450, 1200), J = 9, theta_sd = 1, beta_x = 0.85, beta_group = -0.9, threshold_shift = 0,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "N", varied_value = c(75, 150, 225, 300, 450, 1200)),
  data.frame(slice = "scale_location", scenario_label = c("Threshold shift = -2.00", "Threshold shift = 2.00"),
    N = 600, J = 9, theta_sd = 1, beta_x = 0.85, beta_group = -0.9, threshold_shift = c(-2, 2),
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "threshold_shift", varied_value = c(-2, 2)),
  data.frame(slice = "item_count", scenario_label = paste("Items =", c(5, 15)),
    N = 600, J = c(5, 15), theta_sd = 1, beta_x = 0.85, beta_group = -0.9, threshold_shift = 0,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "J", varied_value = c(5, 15)),
  data.frame(slice = "latent_dispersion", scenario_label = c("Latent SD = 0.70", "Latent SD = 1.30"),
    N = 600, J = 9, theta_sd = c(0.7, 1.3), beta_x = 0.85, beta_group = -0.9, threshold_shift = 0,
    beta_main_1_multiplier = 1, beta_main_2_multiplier = 1, varied_parameter = "theta_sd", varied_value = c(0.7, 1.3))
)
ss$scenario_id <- sprintf("SS-%03d", 1:nrow(ss))
ss$family <- "sum_scores"
ss$slice_membership <- ss$slice
ss$slice_membership[1:3] <- c("paper_anchor;scale_location",
                              "paper_anchor;main_effect_surface;sample_size;scale_location;item_count;latent_dispersion",
                              "paper_anchor;scale_location")
ss$paper_anchor <- ss$slice == "paper_anchor"
ss$generating_model <- "Deterministic graded-response item model"
ss$generating_link <- "graded logistic latent scale"
ss$fitted_model <- "Observed sum-score identity;Gaussian-probit bounded score;Latent generating scale"
ss$fitted_link <- "identity;probit;identity latent benchmark"
ss$item_max <- 3
ss$beta_x_group <- 0
ss$x_min <- -1
ss$x_max <- 1

####################################################
# Within family (WF-001 to WF-046)
####################################################

# Logit coefficients are the probit reference coefficients x 1.65 (written out as literals).
# Columns: link, intercept, group, condition, coefficient scale, reference intercept/group/condition
wf_block <- function(slice, label, link, b0, bg, bc, ref_b0 = 1.5, ref_bg = -1, ref_bc = -1, m1 = 1, m2 = 1,
                     n_subjects = 700, k_trials = 15, target_icc = 0.3, varied_parameter = NA, varied_value = NA) {
  data.frame(slice = slice, scenario_label = label, generating_link = link, beta_intercept = b0, beta_group = bg,
    beta_condition = bc, coefficient_scale = if (link == "logit") 1.65 else 1, reference_beta_intercept = ref_b0,
    reference_beta_group = ref_bg, reference_beta_condition = ref_bc, beta_main_1_multiplier = m1, beta_main_2_multiplier = m2,
    n_subjects = n_subjects, k_trials = k_trials, target_icc = target_icc, varied_parameter = varied_parameter, varied_value = varied_value)
}
surface <- "beta_group_multiplier;beta_condition_multiplier"
logit_main <- c(-0.825, -1.65, -2.475) # 0.5, 1, 1.5 times -1.65
wf <- rbind(
  wf_block("paper_anchor", "Logit-generated manuscript scenario", "logit", 2.475, -1.65, -1.65),
  wf_block("paper_anchor", "Probit-generated manuscript scenario", "probit", 1.5, -1, -1),
  wf_block("main_effect_surface", paste0("logit generated: group x ", mult_label(m1), "; condition x ", mult_label(m2)), "logit", 2.475,
           logit_main[match(m1, c(0.5, 1, 1.5))], logit_main[match(m2, c(0.5, 1, 1.5))], ref_bg = -m1, ref_bc = -m2, m1 = m1, m2 = m2,
           varied_parameter = surface),
  wf_block("main_effect_surface", paste0("probit generated: group x ", mult_label(m1), "; condition x ", mult_label(m2)), "probit", 1.5,
           -m1, -m2, ref_bg = -m1, ref_bc = -m2, m1 = m1, m2 = m2, varied_parameter = surface),
  wf_block("sample_size", paste("logit generated: subjects =", c(100, 200, 300, 400, 1200, 2000)), "logit", 2.475, -1.65, -1.65,
           n_subjects = c(100, 200, 300, 400, 1200, 2000), varied_parameter = "n_subjects", varied_value = c(100, 200, 300, 400, 1200, 2000)),
  wf_block("trial_count", paste("logit generated: trials per cell =", c(5, 30)), "logit", 2.475, -1.65, -1.65,
           k_trials = c(5, 30), varied_parameter = "k_trials", varied_value = c(5, 30)),
  wf_block("latent_location", paste("logit generated: reference intercept =", c(0, 0.75, 2.25)), "logit", c(0, 1.2375, 3.7125), -1.65, -1.65,
           ref_b0 = c(0, 0.75, 2.25), varied_parameter = "reference_beta_intercept", varied_value = c(0, 0.75, 2.25)),
  wf_block("icc", paste("logit generated: target ICC =", c(0, 0.15, 0.45)), "logit", 2.475, -1.65, -1.65,
           target_icc = c(0, 0.15, 0.45), varied_parameter = "target_icc", varied_value = c(0, 0.15, 0.45)),
  wf_block("sample_size", paste("probit generated: subjects =", c(100, 200, 300, 400, 1200, 2000)), "probit", 1.5, -1, -1,
           n_subjects = c(100, 200, 300, 400, 1200, 2000), varied_parameter = "n_subjects", varied_value = c(100, 200, 300, 400, 1200, 2000)),
  wf_block("trial_count", paste("probit generated: trials per cell =", c(5, 30)), "probit", 1.5, -1, -1,
           k_trials = c(5, 30), varied_parameter = "k_trials", varied_value = c(5, 30)),
  wf_block("latent_location", paste("probit generated: reference intercept =", c(0, 0.75, 2.25)), "probit", c(0, 0.75, 2.25), -1, -1,
           ref_b0 = c(0, 0.75, 2.25), varied_parameter = "reference_beta_intercept", varied_value = c(0, 0.75, 2.25)),
  wf_block("icc", paste("probit generated: target ICC =", c(0, 0.15, 0.45)), "probit", 1.5, -1, -1,
           target_icc = c(0, 0.15, 0.45), varied_parameter = "target_icc", varied_value = c(0, 0.15, 0.45))
)
wf$scenario_id <- sprintf("WF-%03d", 1:nrow(wf))
wf$family <- "within_family"
wf$slice_membership <- wf$slice
wf$slice_membership[1:2] <- "paper_anchor;main_effect_surface;sample_size;trial_count;latent_location;icc"
wf$paper_anchor <- wf$slice == "paper_anchor"
wf$generating_model <- "Random-intercept binomial"
wf$fitted_model <- "Binomial logit;Binomial probit"
wf$fitted_link <- "logit;probit"
wf$reference_beta_group_condition <- 0
wf$logit_probit_scale <- 1.65
wf$beta_group_condition <- 0

####################################################
# Full core grid
####################################################

grid <- rbind(add_missing(fc), add_missing(ss), add_missing(wf))
grid$base_seed <- 20260807
grid$seed_rule <- "seed = (base_seed + family_offset + stream_offset + scenario_number * 10000 + replication) modulo .Machine$integer.max"
grid$B <- 3000
grid$alpha <- 0.05
grid$generating_interaction <- 0
write.csv(grid, "simulation-atlas/data/scenario-grid.csv", row.names = FALSE, na = "")
cat("Wrote", nrow(grid), "core scenarios.\n")

####################################################
# Diagnostic grid (D-FC, D-SS, D-WF)
####################################################

# Rows copy their core anchor (FC-001, SS-002, WF-002), including its labels and
# metadata, and change only sample size and severity (intercept).
d_fc <- grid[rep(which(grid$scenario_id == "FC-001"), 8), ]
d_fc$N <- c(100, 250, 500, 1000, 250, 250, 250, 250)
d_fc$beta_intercept <- c(-0.8, -0.8, -0.8, -0.8, -1.5, 0, 0.8, 1.5)
d_fc$scenario_id <- sprintf("D-FC-%03d", 1:8)
d_fc$wrong_fitted_link <- "logit"
d_fc$wrong_model_label <- "Standard binomial logit"
d_fc$diagnostic_note <- "Paper framework: chance-corrected logit generated, standard logit fitted."

d_ss <- grid[grid$scenario_id == "SS-002", ]
d_ss$scenario_id <- "D-SS-001"
d_ss$wrong_fitted_link <- NA
d_ss$wrong_model_label <- "No comparable paper diagnostic"
d_ss$diagnostic_note <- paste("The paper does not compare diagnostics for the sum-score models because",
  "their response variables/scales do not support a same-likelihood AIC comparison.")

d_wf <- grid[rep(which(grid$scenario_id == "WF-002"), 7), ]
d_wf$n_subjects <- c(200, 300, 700, 1200, 300, 300, 300)
d_wf$reference_beta_intercept <- c(1.5, 1.5, 1.5, 1.5, 0, 0.75, 2.25)
d_wf$beta_intercept <- d_wf$reference_beta_intercept
d_wf$scenario_id <- sprintf("D-WF-%03d", 1:7)
d_wf$wrong_fitted_link <- "logit"
d_wf$wrong_model_label <- "Binomial logit GLMM"
d_wf$diagnostic_note <- "Exact paper diagnostic anchor uses 300 subjects; probit generated, logit fitted."

diagnostic_grid <- rbind(d_fc, d_ss, d_wf)
# the first four rows of each family vary sample size, the others severity; row 2 is the paper anchor
diagnostic_grid$diagnostic_source_slice <- c(rep(c("sample_size", "severity"), c(4, 4)), "diagnostic_not_defined", rep(c("sample_size", "severity"), c(4, 3)))
diagnostic_grid$diagnostic_paper_anchor <- diagnostic_grid$scenario_id %in% c("D-FC-002", "D-WF-002")
diagnostic_grid$diagnostic_slice_membership <- ifelse(diagnostic_grid$diagnostic_paper_anchor, "diagnostic_anchor;sample_size;severity",
                                                      diagnostic_grid$diagnostic_source_slice)
diagnostic_grid$paper_anchor <- diagnostic_grid$diagnostic_paper_anchor
diagnostic_grid$slice <- diagnostic_grid$diagnostic_source_slice
diagnostic_grid$slice_membership <- diagnostic_grid$diagnostic_slice_membership
diagnostic_grid <- diagnostic_grid[, c(columns, "diagnostic_source_slice", "diagnostic_paper_anchor", "diagnostic_slice_membership",
                                       "wrong_fitted_link", "wrong_model_label", "diagnostic_note")]
write.csv(diagnostic_grid, "simulation-atlas/data/diagnostic-grid.csv", row.names = FALSE, na = "")
cat("Wrote", nrow(diagnostic_grid), "diagnostic design rows (sum scores explicitly inapplicable).\n")
