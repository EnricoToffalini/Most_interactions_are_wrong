# R/utils-link-functions.R
# Link functions, chance-corrected binomial models, and interaction helpers.

clip_probability <- function(p, eps = 1e-8) {
  pmin(pmax(p, eps), 1 - eps)
}

inv_link <- function(eta, link = "logit") {
  switch(
    link,
    logit = stats::plogis(eta),
    probit = stats::pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    log = exp(eta),
    inverse = 1 / eta,
    identity = eta
  )
}

link_fun <- function(mu, link = "logit") {
  switch(
    link,
    logit = stats::qlogis(clip_probability(mu)),
    probit = stats::qnorm(clip_probability(mu)),
    cloglog = log(-log1p(-clip_probability(mu))),
    log = log(pmax(mu, 1e-8)),
    inverse = 1 / pmax(mu, 1e-8),
    identity = mu
  )
}

chance_linkinv <- function(eta, chance = 0.5, link = "logit") {
  q <- inv_link(eta, link = link)
  chance + (1 - chance) * q
}

chance_link <- function(p, chance = 0.5, link = "logit") {
  q <- (p - chance) / (1 - chance)
  link_fun(clip_probability(q), link = link)
}

group_difference <- function(group0, group1) {
  group1 - group0
}

change_in_group_difference <- function(low_group0, low_group1, high_group0, high_group1) {
  group_difference(high_group0, high_group1) -
    group_difference(low_group0, low_group1)
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

fit_chance_binom <- function(formula, data, y_col = "y", k_col = "k",
                             chance = 0.5, link = "logit") {
  mf <- stats::model.frame(formula, data = data)
  X <- stats::model.matrix(formula, data = mf)
  y <- data[[y_col]]
  k <- data[[k_col]]

  if (any(is.na(y)) || any(is.na(k))) {
    stop("Missing y or k values in chance-corrected fit.", call. = FALSE)
  }
  if (any(y < 0 | k <= 0 | y > k)) {
    stop("Invalid binomial counts in chance-corrected fit.", call. = FALSE)
  }

  start <- rep(0, ncol(X))
  names(start) <- colnames(X)

  start_fit <- try(
    stats::glm.fit(
      x = X,
      y = stats::cbind(y, k - y),
      family = stats::binomial(link = link)
    ),
    silent = TRUE
  )

  if (!inherits(start_fit, "try-error") && length(stats::coef(start_fit)) == length(start)) {
    cf <- stats::coef(start_fit)
    if (all(is.finite(cf))) start <- cf
  }

  nll <- function(beta) {
    eta <- drop(X %*% beta)
    p <- chance_linkinv(eta, chance = chance, link = link)
    p <- clip_probability(p, eps = 1e-10)
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }

  opt <- try(
    stats::optim(
      start,
      nll,
      method = "BFGS",
      hessian = TRUE,
      control = list(maxit = 1000)
    ),
    silent = TRUE
  )

  if (inherits(opt, "try-error") || !is.finite(opt$value)) {
    return(list(
      coefficients = stats::setNames(rep(NA_real_, ncol(X)), colnames(X)),
      vcov = matrix(NA_real_, ncol(X), ncol(X), dimnames = list(colnames(X), colnames(X))),
      p_value = stats::setNames(rep(NA_real_, ncol(X)), colnames(X)),
      formula = formula,
      terms = stats::terms(formula),
      xlevels = stats::.getXlevels(stats::terms(formula), mf),
      chance = chance,
      link = link,
      converged = FALSE,
      model_matrix_names = colnames(X),
      nll = NA_real_
    ))
  }

  V <- try(solve(opt$hessian), silent = TRUE)
  if (inherits(V, "try-error") || any(!is.finite(V))) {
    V <- matrix(NA_real_, ncol(X), ncol(X))
  }

  se <- sqrt(pmax(diag(V), 0))
  z <- opt$par / se
  pval <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

  names(opt$par) <- colnames(X)
  names(pval) <- colnames(X)
  dimnames(V) <- list(colnames(X), colnames(X))

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

  X <- stats::model.matrix(
    stats::delete.response(object$terms),
    data = newdata,
    xlev = object$xlevels
  )

  eta <- drop(X %*% object$coefficients)
  if (type == "link") return(eta)

  p <- chance_linkinv(eta, chance = object$chance, link = object$link)
  pmin(pmax(p, 0), 1)
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
