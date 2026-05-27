# R/utils-link-functions.R
# Link functions, contrasts, and model extraction helpers.

inv_link <- function(eta, link = "logit") {
  switch(
    link,
    logit = stats::plogis(eta),
    probit = stats::pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    stop("Unsupported link: ", link, call. = FALSE)
  )
}

link_fun <- function(mu, link = "logit") {
  eps <- 1e-8
  mu <- pmin(pmax(mu, eps), 1 - eps)
  switch(
    link,
    logit = stats::qlogis(mu),
    probit = stats::qnorm(mu),
    cloglog = log(-log1p(-mu)),
    stop("Unsupported link: ", link, call. = FALSE)
  )
}

chance_linkinv <- function(eta, chance = 0.5, link = "logit") {
  chance + (1 - chance) * inv_link(eta, link = link)
}

# Backward-compatible alias used in older drafts.
inv_chance_link <- function(eta, c = 0.5, link = "logit") {
  chance_linkinv(eta, chance = c, link = link)
}

chance_link <- function(p, chance = 0.5, link = "logit") {
  eps <- 1e-8
  q <- (p - chance) / (1 - chance)
  q <- pmin(pmax(q, eps), 1 - eps)
  link_fun(q, link = link)
}

group_difference <- function(group0, group1) {
  group1 - group0
}

change_in_group_difference <- function(low_group0, low_group1, high_group0, high_group1) {
  group_difference(high_group0, high_group1) - group_difference(low_group0, low_group1)
}

interaction_row_name <- function(fit) {
  rn <- names(stats::coef(fit))
  hit <- grep(":", rn, value = TRUE)
  if (length(hit) == 0) return(NA_character_)
  hit[length(hit)]
}

interaction_coef_from_lm <- function(fit) {
  rn <- interaction_row_name(fit)
  if (is.na(rn)) return(NA_real_)
  unname(stats::coef(fit)[rn])
}

interaction_p_from_lm <- function(fit) {
  rn <- interaction_row_name(fit)
  if (is.na(rn)) return(NA_real_)
  sm <- summary(fit)$coefficients
  pcol <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(pcol) || !rn %in% rownames(sm)) return(NA_real_)
  unname(sm[rn, pcol])
}

interaction_coef_from_glm <- function(fit) {
  rn <- interaction_row_name(fit)
  if (is.na(rn)) return(NA_real_)
  unname(stats::coef(fit)[rn])
}

interaction_p_from_glm <- function(fit) {
  rn <- interaction_row_name(fit)
  if (is.na(rn)) return(NA_real_)
  sm <- summary(fit)$coefficients
  pcol <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(pcol) || !rn %in% rownames(sm)) return(NA_real_)
  unname(sm[rn, pcol])
}

# Robust chance-corrected binomial model.
# This uses the binomial likelihood directly:
#   p_i = chance + (1 - chance) * F_link(X_i b)
# It does not transform observed proportions, so observations below chance
# due to binomial sampling remain valid.
fit_chance_binom <- function(formula, data, y_col = "y", k_col = "k", chance = 0.5, link = "logit") {
  mf <- stats::model.frame(formula, data = data)
  X <- stats::model.matrix(formula, data = mf)
  y <- data[[y_col]]
  k <- data[[k_col]]
  if (any(is.na(y)) || any(is.na(k))) stop("Missing y or k values in chance-corrected fit.", call. = FALSE)
  if (any(y < 0 | k <= 0 | y > k)) stop("Invalid binomial counts in chance-corrected fit.", call. = FALSE)

  # Start from a standard binomial GLM when possible, then shift the intercept
  # toward the chance-corrected scale. This is only a starting value.
  start <- rep(0, ncol(X))
  start_fit <- try(
    stats::glm(stats::cbind(y, k - y) ~ X[, -1, drop = FALSE], family = stats::binomial(link), data = data),
    silent = TRUE
  )
  if (!inherits(start_fit, "try-error")) {
    cf <- stats::coef(start_fit)
    cf <- cf[!is.na(cf)]
    if (length(cf) == length(start)) start <- cf
  }

  nll <- function(beta) {
    eta <- drop(X %*% beta)
    p <- chance_linkinv(eta, chance = chance, link = link)
    p <- pmin(pmax(p, 1e-10), 1 - 1e-10)
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }

  opt <- try(stats::optim(start, nll, method = "BFGS", hessian = TRUE, control = list(maxit = 1000)), silent = TRUE)
  if (inherits(opt, "try-error") || !is.finite(opt$value)) {
    return(list(coefficients = rep(NA_real_, ncol(X)), vcov = matrix(NA_real_, ncol(X), ncol(X)),
                p_value = rep(NA_real_, ncol(X)), formula = formula, terms = stats::terms(formula),
                xlevels = stats::.getXlevels(stats::terms(formula), mf), chance = chance, link = link,
                converged = FALSE, model_matrix_names = colnames(X), nll = NA_real_))
  }

  H <- opt$hessian
  V <- try(solve(H), silent = TRUE)
  if (inherits(V, "try-error") || any(!is.finite(V))) {
    V <- matrix(NA_real_, ncol(X), ncol(X))
  }
  se <- sqrt(pmax(diag(V), 0))
  z <- opt$par / se
  pval <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  names(opt$par) <- colnames(X)
  names(pval) <- colnames(X)
  colnames(V) <- rownames(V) <- colnames(X)
  list(
    coefficients = opt$par,
    vcov = V,
    p_value = pval,
    formula = formula,
    terms = stats::terms(formula),
    xlevels = stats::.getXlevels(stats::terms(formula), mf),
    chance = chance,
    link = link,
    converged = isTRUE(opt$convergence == 0),
    model_matrix_names = colnames(X),
    nll = opt$value
  )
}

predict_chance_binom <- function(object, newdata, type = c("response", "link")) {
  type <- match.arg(type)
  X <- stats::model.matrix(delete.response(object$terms), data = newdata, xlev = object$xlevels)
  eta <- drop(X %*% object$coefficients)
  if (type == "link") return(eta)
  chance_linkinv(eta, chance = object$chance, link = object$link)
}

interaction_coef_from_chance <- function(fit) {
  rn <- grep(":", names(fit$coefficients), value = TRUE)
  if (length(rn) == 0) return(NA_real_)
  unname(fit$coefficients[rn[length(rn)]])
}

interaction_p_from_chance <- function(fit) {
  rn <- grep(":", names(fit$p_value), value = TRUE)
  if (length(rn) == 0) return(NA_real_)
  unname(fit$p_value[rn[length(rn)]])
}
