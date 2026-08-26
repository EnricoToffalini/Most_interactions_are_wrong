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

  zero <- stats::setNames(rep(0, ncol(X)), colnames(X))

  start_fit <- try(
    stats::glm.fit(
      x = X,
      y = stats::cbind(y, k - y),
      family = stats::binomial(link = link)
    ),
    silent = TRUE
  )

  from_standard <- zero
  if (!inherits(start_fit, "try-error") && length(stats::coef(start_fit)) == length(zero)) {
    cf <- stats::coef(start_fit)
    if (all(is.finite(cf))) from_standard[] <- cf
  }

  nll <- function(beta) {
    eta <- drop(X %*% beta)
    p <- chance_linkinv(eta, chance = chance, link = link)
    p <- clip_probability(p, eps = 1e-10)
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }

  gradient <- function(beta) {
    eta <- drop(X %*% beta)
    q <- inv_link(eta, link = link)
    p <- chance + (1 - chance) * q
    p <- clip_probability(p, eps = 1e-10)

    # This score is analytic for the chance-corrected logit used by the
    # manuscript simulation. Other links retain the previous numerical
    # optimization path rather than silently using the wrong derivative.
    if (link != "logit") return(NULL)

    weight <- ((y - k * p) / (p * (1 - p))) *
      (1 - chance) * q * (1 - q)
    -drop(crossprod(X, weight))
  }

  # A standard-logit fit is on the wrong scale when chance > 0. Use it as one
  # candidate start, together with zero and a start estimated directly on the
  # observed above-chance proportions. This mirrors the robust Atlas fitter and
  # avoids false convergence on the flat likelihood region near the chance
  # floor.
  above <- (y / k - chance) / (1 - chance)
  above <- clip_probability(above, eps = 0.02)
  from_above <- zero
  above_fit <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
  if (!inherits(above_fit, "try-error") && all(is.finite(stats::coef(above_fit)))) {
    from_above[] <- stats::coef(above_fit)
  }

  starts <- list(zero, from_standard, from_above)
  candidates <- lapply(starts, function(start) {
    try(
      stats::optim(
        start,
        nll,
        gr = if (link == "logit") gradient else NULL,
        method = "BFGS",
        control = list(maxit = 1500, reltol = 1e-10)
      ),
      silent = TRUE
    )
  })
  ok <- vapply(
    candidates,
    function(x) {
      !inherits(x, "try-error") && is.finite(x$value) && all(is.finite(x$par))
    },
    logical(1)
  )

  if (!any(ok)) {
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
      optimizer_convergence = NA_integer_,
      gradient_max = NA_real_,
      hessian_min_eigen = NA_real_,
      hessian_eigen_ratio = NA_real_,
      model_matrix_names = colnames(X),
      nll = NA_real_
    ))
  }

  valid_candidates <- candidates[ok]
  best <- valid_candidates[[which.min(vapply(
    valid_candidates,
    `[[`,
    numeric(1),
    "value"
  ))]]
  names(best$par) <- colnames(X)

  hessian <- try(
    stats::optimHess(
      best$par,
      nll,
      gr = if (link == "logit") gradient else NULL
    ),
    silent = TRUE
  )

  # A Wald test is only defined when the observed information matrix is positive
  # definite and well conditioned. Near the chance floor the likelihood can be
  # flat enough that it is neither, and inverting it anyway returns variances
  # that are negative (clamped to zero, giving an infinite z) or implausibly
  # small (giving a z large enough to underflow the normal tail to exactly 0).
  # Both are recorded as missing, so the fit counts as unusable.
  hess_sym <- if (!inherits(hessian, "try-error") && all(is.finite(hessian))) {
    (hessian + t(hessian)) / 2
  } else {
    NULL
  }
  eig <- if (is.null(hess_sym)) {
    structure("Hessian calculation failed", class = "try-error")
  } else {
    try(eigen(hess_sym, symmetric = TRUE, only.values = TRUE)$values, silent = TRUE)
  }
  hessian_min_eigen <- if (inherits(eig, "try-error")) NA_real_ else min(eig)
  hessian_eigen_ratio <- if (inherits(eig, "try-error")) {
    NA_real_
  } else {
    min(eig) / max(eig)
  }
  well_conditioned <- !inherits(eig, "try-error") &&
    all(is.finite(eig)) &&
    hessian_min_eigen > 1e-7 &&
    hessian_eigen_ratio > sqrt(.Machine$double.eps)

  V <- if (well_conditioned) try(solve(hess_sym), silent = TRUE) else NULL
  if (is.null(V) || inherits(V, "try-error") || any(!is.finite(V))) {
    V <- matrix(NA_real_, ncol(X), ncol(X))
  }

  variances <- diag(V)
  variances[!is.finite(variances) | variances <= 0] <- NA_real_
  se <- sqrt(variances)
  z <- best$par / se
  pval <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)

  names(pval) <- colnames(X)
  dimnames(V) <- list(colnames(X), colnames(X))

  optimizer_converged <- isTRUE(best$convergence == 0)
  usable <- optimizer_converged && well_conditioned &&
    all(is.finite(V)) && all(is.finite(pval))
  if (!usable) pval[] <- NA_real_

  list(
    coefficients = best$par,
    vcov = V,
    p_value = pval,
    formula = formula,
    terms = stats::terms(formula),
    xlevels = stats::.getXlevels(stats::terms(formula), mf),
    chance = chance,
    link = link,
    converged = usable,
    optimizer_convergence = best$convergence,
    gradient_max = if (link == "logit") max(abs(gradient(best$par))) else NA_real_,
    hessian_min_eigen = hessian_min_eigen,
    hessian_eigen_ratio = hessian_eigen_ratio,
    model_matrix_names = colnames(X),
    nll = best$value
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
