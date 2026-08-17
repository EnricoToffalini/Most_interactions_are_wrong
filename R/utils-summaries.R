# R/utils-summaries.R
# Monte Carlo summary helpers.

wilson_ci <- function(x, n, conf = 0.95) {
  if (!is.finite(n) || n == 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }

  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n

  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denom

  c(lower = center - half, upper = center + half)
}

summarise_detection <- function(p_values, alpha = 0.05, conf = 0.95) {
  ok <- is.finite(p_values)
  n <- sum(ok)
  sig <- sum(p_values[ok] < alpha)
  ci <- wilson_ci(sig, n, conf = conf)

  data.frame(
    n_successful_fits = n,
    n_significant = sig,
    rate = if (n == 0) NA_real_ else sig / n,
    ci_low = ci["lower"],
    ci_high = ci["upper"],
    stringsAsFactors = FALSE
  )
}

safe_median <- function(x) {
  if (all(!is.finite(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::sd(x)
}

safe_quantile <- function(x, probs) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  unname(stats::quantile(x, probs = probs, names = FALSE))
}

summarise_model_simulation <- function(dat, alpha = 0.05, conf = 0.95) {
  det <- summarise_detection(dat$p_value, alpha = alpha, conf = conf)

  data.frame(
    n_successful_fits = det$n_successful_fits,
    n_rejections = det$n_significant,
    rejection_rate = det$rate,
    ci_low = det$ci_low,
    ci_high = det$ci_high,
    median_interaction_coef = safe_median(dat$interaction_coef),
    mean_interaction_coef = safe_mean(dat$interaction_coef),
    sd_interaction_coef = safe_sd(dat$interaction_coef),
    q25_interaction_coef = safe_quantile(dat$interaction_coef, 0.25),
    q75_interaction_coef = safe_quantile(dat$interaction_coef, 0.75),
    q025_interaction_coef = safe_quantile(dat$interaction_coef, 0.025),
    q975_interaction_coef = safe_quantile(dat$interaction_coef, 0.975),
    median_abs_coef_significant = safe_median(
      abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value < alpha])
    ),
    median_abs_coef_nonsignificant = safe_median(
      abs(dat$interaction_coef[is.finite(dat$p_value) & dat$p_value >= alpha])
    ),
    median_change_in_group_difference_response_scale = safe_median(dat$change_in_group_difference_response_scale),
    median_change_in_group_difference_outcome_units = safe_median(dat$change_in_group_difference_outcome_units),
    mean_change_in_group_difference_outcome_units = safe_mean(dat$change_in_group_difference_outcome_units),
    sd_change_in_group_difference_outcome_units = safe_sd(dat$change_in_group_difference_outcome_units),
    stringsAsFactors = FALSE
  )
}
