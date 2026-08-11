# Shared, atlas-local utilities. Nothing in this file writes manuscript outputs.

ATLAS_VERSION <- "0.1.0"
ATLAS_BASE_SEED <- 20260807L
ATLAS_ALPHA <- 0.05
ATLAS_SEED_RULE <- paste0(
  "seed = (base_seed + stream_offset + scenario_number * 10000 + replication) ",
  "modulo .Machine$integer.max"
)

atlas_root <- function() {
  candidates <- c("simulation-atlas", ".")
  hit <- candidates[file.exists(file.path(candidates, "R", "atlas-common.R"))]
  if (!length(hit)) stop("Cannot locate simulation-atlas from the working directory.", call. = FALSE)
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

atlas_repo_root <- function() {
  root <- atlas_root()
  if (basename(root) == "simulation-atlas") {
    normalizePath(file.path(root, ".."), winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(".", winslash = "/", mustWork = TRUE)
  }
}

atlas_source <- function(file) {
  source(file.path(atlas_root(), "R", file), local = .GlobalEnv)
}

atlas_family_code <- function(family) {
  unname(c(forced_choice = "FC", sum_scores = "SS", within_family = "WF")[[family]])
}

atlas_seed <- function(family, scenario_id, replication, stream = "core",
                       base_seed = ATLAS_BASE_SEED) {
  family_offset <- c(forced_choice = 1000000L, sum_scores = 2000000L,
                     within_family = 3000000L)[[family]]
  stream_offset <- c(core = 0L, diagnostic = 4000000L)[[stream]]
  if (is.null(family_offset) || is.null(stream_offset)) {
    stop("Unknown family or seed stream.", call. = FALSE)
  }
  number <- suppressWarnings(as.integer(sub(".*-", "", scenario_id)))
  if (!is.finite(number) || number < 1L || replication < 1L) {
    stop("scenario_id and replication must contain positive integer indices.", call. = FALSE)
  }
  value <- (as.double(base_seed) + family_offset + stream_offset +
              as.double(number) * 10000 + as.double(replication)) %%
    .Machine$integer.max
  as.integer(value)
}

# The settings live at the top of 02-run-atlas.R, so these three helpers only
# check that what the user typed there makes sense and say plainly what to fix.
atlas_check_count <- function(value, name, minimum = 1L) {
  value <- suppressWarnings(as.integer(value))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    stop(name, " must be a whole number >= ", minimum, ".", call. = FALSE)
  }
  value
}

atlas_check_flag <- function(value, name) {
  if (length(value) != 1L || !is.logical(value) || is.na(value)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

atlas_check_mode <- function(mode, name = "MODE") {
  mode <- tolower(as.character(mode))
  if (length(mode) != 1L || !mode %in% c("smoke", "full")) {
    stop(name, ' must be "smoke" or "full".', call. = FALSE)
  }
  mode
}

# Smoke mode must mean exactly the same thing to the runner and to the
# summarizer, so the scenario selection and the replication counts are defined
# once here instead of being restated in both scripts.
atlas_smoke_scenario_ids <- function(grid) {
  c(
    grid$scenario_id[grid$family == "forced_choice" & grid$paper_anchor &
                       abs(grid$beta_intercept) < 1e-12][1],
    grid$scenario_id[grid$family == "sum_scores" & grid$paper_anchor &
                       abs(grid$threshold_shift) < 1e-12][1],
    grid$scenario_id[grid$family == "within_family" & grid$paper_anchor &
                       grid$generating_link == "probit"][1]
  )
}

# DHARMa residual checks dominate the diagnostic runtime, so RUN_DHARMA in
# 02-run-atlas.R defaults to FALSE and they are run in a later pass. When they
# are skipped, every other diagnostic (AIC, Pregibon) is still computed and the
# atlas stays usable; the DHARMa rows are recorded as "not computed in this
# run", which is a different claim from the structural "not applicable" already
# used for checks that make no sense for a family.
#
# Raw diagnostic files from a DHARMa-free pass must not be mistaken for
# complete ones, so they carry their own name and can coexist.
atlas_diagnostic_kind <- function(dharma_enabled) {
  if (dharma_enabled) "diagnostic" else "diagnostic-nodharma"
}

# Replications per scenario. B and B_within come from the settings block at the
# top of 02-run-atlas.R; NA there means "use the default for this mode".
atlas_replication_counts <- function(mode, B = NA, B_within = NA) {
  defaults <- if (mode == "smoke") c(core = 3L, within = 2L) else c(core = 500L, within = 300L)
  core <- if (all(is.na(B))) defaults[["core"]] else atlas_check_count(B, "B")
  within <- if (all(is.na(B_within))) defaults[["within"]] else atlas_check_count(B_within, "B_WITHIN")
  if (mode == "smoke" && (!core %in% 2:3 || !within %in% 2:3)) {
    stop('Smoke mode requires B and B_WITHIN to be 2 or 3 (or NA for the defaults).',
         call. = FALSE)
  }
  list(core = core, within = within)
}

# Default number of simulated data sets per DHARMa residual check.
atlas_default_dharma_n_sim <- function(mode) if (mode == "smoke") 25L else 250L

# Vectorized so it can label a whole grid or a single scenario.
atlas_requested_B <- function(family, counts) {
  ifelse(family == "within_family", counts$within, counts$core)
}

# The pseudo-interaction is a smooth monotone function of main-effect
# magnitude, and its deterministic component is computed analytically at every
# grid point, so three levels trace the same surface a denser grid would.
ATLAS_MAIN_EFFECT_MULTIPLIERS <- c(0.50, 1.00, 1.50)

# All three families share one main-effect surface; the grid validator asserts
# 25 cells per family, so they must stay identical.
atlas_main_effect_surface <- function() {
  expand.grid(beta_main_1_multiplier = ATLAS_MAIN_EFFECT_MULTIPLIERS,
              beta_main_2_multiplier = ATLAS_MAIN_EFFECT_MULTIPLIERS)
}

# Builds one scenario row from a family's anchor list plus named overrides.
atlas_row_maker <- function(base) {
  function(slice, label, paper_anchor = FALSE, ...) {
    values <- utils::modifyList(base, list(...))
    values$.source_slice <- slice
    values$scenario_label <- label
    values$paper_anchor <- paper_anchor
    as.data.frame(values, stringsAsFactors = FALSE)
  }
}

atlas_bind_rows <- function(rows) {
  if (!length(rows)) return(data.frame())
  names_all <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(names_all, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[names_all]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

atlas_signature <- function(dat, columns) {
  values <- lapply(dat[columns], function(x) {
    if (is.numeric(x)) {
      ifelse(is.na(x), "NA", format(x, digits = 17, scientific = FALSE, trim = TRUE))
    } else {
      ifelse(is.na(x), "NA", as.character(x))
    }
  })
  do.call(paste, c(values, sep = "|"))
}

atlas_dedupe_family <- function(rows, scientific_columns, family) {
  key <- atlas_signature(rows, scientific_columns)
  groups <- split(seq_len(nrow(rows)), factor(key, levels = unique(key)))
  out <- lapply(groups, function(index) {
    first <- rows[index[1], , drop = FALSE]
    memberships <- unique(rows$.source_slice[index])
    first$slice <- first$.source_slice
    first$slice_membership <- paste(memberships, collapse = ";")
    first$paper_anchor <- any(rows$paper_anchor[index])
    if (first$paper_anchor) {
      anchor_index <- index[which(rows$paper_anchor[index])[1]]
      first$scenario_label <- rows$scenario_label[anchor_index]
      first$slice <- "paper_anchor"
    }
    first$.source_slice <- NULL
    first
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out$scenario_id <- sprintf("%s-%03d", atlas_family_code(family), seq_len(nrow(out)))
  out
}

atlas_expand_slice_membership <- function(dat) {
  if (!nrow(dat)) return(dat)
  rows <- lapply(seq_len(nrow(dat)), function(i) {
    memberships <- strsplit(as.character(dat$slice_membership[i]), ";", fixed = TRUE)[[1]]
    out <- dat[rep(i, length(memberships)), , drop = FALSE]
    out$slice_view <- memberships
    out
  })
  do.call(rbind, rows)
}

atlas_wilson_ci <- function(x, n, conf = 0.95) {
  if (!is.finite(n) || n <= 0) return(c(low = NA_real_, high = NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- x / n
  denominator <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denominator
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / denominator
  c(low = max(0, centre - half), high = min(1, centre + half))
}

atlas_safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else stats::median(x)
}

atlas_zero_small <- function(x, tolerance = 1e-12) {
  ifelse(is.finite(x) & abs(x) < tolerance, 0, x)
}

atlas_capture <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    list(value = NULL, error = conditionMessage(value), warnings = paste(unique(warnings), collapse = "; "))
  } else {
    list(value = value, error = "", warnings = paste(unique(warnings), collapse = "; "))
  }
}

atlas_interaction_stats <- function(fit) {
  empty <- list(coef = NA_real_, se = NA_real_, p = NA_real_)
  if (is.null(fit)) return(empty)
  if (inherits(fit, "atlas_chance_binomial")) {
    term <- grep(":", names(fit$coefficients), value = TRUE)[1]
    if (is.na(term)) return(empty)
    return(list(
      coef = unname(fit$coefficients[term]),
      se = unname(sqrt(fit$vcov[term, term])),
      p = unname(fit$p_value[term])
    ))
  }
  if (inherits(fit, "glmmTMB")) {
    sm <- try(summary(fit)$coefficients$cond, silent = TRUE)
  } else {
    sm <- try(stats::coef(summary(fit)), silent = TRUE)
  }
  if (inherits(sm, "try-error") || is.null(rownames(sm))) return(empty)
  term <- grep(":", rownames(sm), value = TRUE)[1]
  p_col <- grep("Pr\\(", colnames(sm), value = TRUE)[1]
  if (is.na(term) || is.na(p_col)) return(empty)
  list(
    coef = unname(sm[term, "Estimate"]),
    se = unname(sm[term, "Std. Error"]),
    p = unname(sm[term, p_col])
  )
}

atlas_fit_problem <- function(fit) {
  if (is.null(fit)) return(TRUE)
  if (inherits(fit, "atlas_chance_binomial")) return(!isTRUE(fit$converged))
  if (inherits(fit, "glm")) return(!isTRUE(fit$converged))
  if (inherits(fit, "glmmTMB")) {
    bad_hessian <- !isTRUE(fit$sdr$pdHess)
    bad_code <- !is.null(fit$fit$convergence) && fit$fit$convergence != 0
    return(bad_hessian || bad_code)
  }
  FALSE
}

atlas_problem_text <- function(captured, fit) {
  pieces <- c(captured$error, captured$warnings)
  if (!is.null(fit) && inherits(fit, "glmmTMB")) {
    if (!isTRUE(fit$sdr$pdHess)) pieces <- c(pieces, "non-positive-definite Hessian")
    if (!is.null(fit$fit$convergence) && fit$fit$convergence != 0 && !is.null(fit$fit$message)) {
      pieces <- c(pieces, as.character(fit$fit$message))
    }
  }
  if (!is.null(fit) && inherits(fit, "atlas_chance_binomial") && !isTRUE(fit$converged)) {
    pieces <- c(pieces, "chance-corrected optimizer/Hessian check failed")
  }
  pieces <- unique(pieces[nzchar(pieces)])
  paste(pieces, collapse = "; ")
}

atlas_difference_in_differences <- function(values, first, second) {
  cell <- function(a, b) values[first == a & second == b][1]
  cell(1, 1) - cell(1, 0) - cell(0, 1) + cell(0, 0)
}

atlas_clip_probability <- function(p, eps = 1e-10) {
  pmin(pmax(p, eps), 1 - eps)
}

atlas_chance_inv <- function(eta, chance) {
  if (!is.finite(chance) || chance < 0 || chance >= 1) {
    stop("chance must be in [0, 1).", call. = FALSE)
  }
  chance + (1 - chance) * stats::plogis(eta)
}

# Local chance-corrected binomial MLE, adapted from the manuscript's
# scripts/03a and scripts/04 implementations. The analytic score and several
# transparent starting values reduce failures near the chance floor.
fit_atlas_chance_binomial <- function(formula, data, chance) {
  if (!is.finite(chance) || chance < 0 || chance >= 1) {
    stop("chance must be in [0, 1).", call. = FALSE)
  }
  model_frame <- stats::model.frame(formula, data = data)
  X <- stats::model.matrix(formula, data = model_frame)
  y <- data$y
  k <- data$k
  if (any(!is.finite(y)) || any(!is.finite(k)) || any(y < 0 | y > k | k <= 0)) {
    stop("Invalid binomial counts in chance-corrected fit.", call. = FALSE)
  }

  nll <- function(beta) {
    eta <- as.vector(X %*% beta)
    p <- atlas_clip_probability(atlas_chance_inv(eta, chance))
    -sum(stats::dbinom(y, size = k, prob = p, log = TRUE))
  }
  gradient <- function(beta) {
    eta <- as.vector(X %*% beta)
    s <- stats::plogis(eta)
    p <- atlas_clip_probability(chance + (1 - chance) * s)
    weight <- ((y - k * p) / (p * (1 - p))) * (1 - chance) * s * (1 - s)
    -as.vector(crossprod(X, weight))
  }

  zero <- stats::setNames(rep(0, ncol(X)), colnames(X))
  standard <- try(stats::glm.fit(X, cbind(y, k - y), family = stats::binomial("logit")), silent = TRUE)
  from_standard <- zero
  if (!inherits(standard, "try-error") && all(is.finite(stats::coef(standard)))) {
    from_standard[] <- stats::coef(standard)
  }
  above <- atlas_clip_probability((y / k - chance) / (1 - chance), eps = 0.02)
  from_above <- zero
  start_lm <- try(stats::lm.fit(X, stats::qlogis(above)), silent = TRUE)
  if (!inherits(start_lm, "try-error") && all(is.finite(stats::coef(start_lm)))) {
    from_above[] <- stats::coef(start_lm)
  }

  candidates <- lapply(list(zero, from_standard, from_above), function(start) {
    try(stats::optim(start, nll, gr = gradient, method = "BFGS",
                     control = list(maxit = 1500, reltol = 1e-10)), silent = TRUE)
  })
  ok <- vapply(candidates, function(x) !inherits(x, "try-error") && is.finite(x$value) &&
                 all(is.finite(x$par)), logical(1))
  if (!any(ok)) stop("Chance-corrected likelihood optimization failed.", call. = FALSE)
  best <- candidates[which(ok)][[which.min(vapply(candidates[ok], `[[`, numeric(1), "value"))]]
  names(best$par) <- colnames(X)
  hessian <- try(stats::optimHess(best$par, nll, gradient), silent = TRUE)
  variance <- matrix(NA_real_, ncol(X), ncol(X), dimnames = list(colnames(X), colnames(X)))
  min_eigen <- NA_real_
  if (!inherits(hessian, "try-error") && all(is.finite(hessian))) {
    min_eigen <- min(eigen(hessian, symmetric = TRUE, only.values = TRUE)$values)
    inverse <- try(solve(hessian), silent = TRUE)
    if (!inherits(inverse, "try-error") && all(is.finite(inverse))) {
      variance[] <- inverse
    }
  }
  se <- sqrt(pmax(diag(variance), 0))
  p_value <- 2 * stats::pnorm(abs(best$par / se), lower.tail = FALSE)
  names(p_value) <- colnames(X)
  eta <- as.vector(X %*% best$par)
  converged <- best$convergence == 0 && is.finite(min_eigen) && min_eigen > 1e-7 &&
    all(is.finite(p_value))

  structure(list(
    coefficients = best$par,
    vcov = variance,
    p_value = p_value,
    converged = converged,
    gradient_max = max(abs(gradient(best$par))),
    hessian_min_eigen = min_eigen,
    logLik_value = -best$value,
    aic = -2 * (-best$value) + 2 * length(best$par),
    formula = formula,
    terms = stats::terms(formula),
    xlevels = stats::.getXlevels(stats::terms(formula), model_frame),
    model_frame = model_frame,
    linear_predictors = eta,
    fitted_probability = atlas_chance_inv(eta, chance),
    observed = y,
    trials = k,
    chance = chance
  ), class = "atlas_chance_binomial")
}

coef.atlas_chance_binomial <- function(object, ...) object$coefficients
vcov.atlas_chance_binomial <- function(object, ...) object$vcov
family.atlas_chance_binomial <- function(object, ...) stats::binomial("logit")
nobs.atlas_chance_binomial <- function(object, ...) length(object$observed)
formula.atlas_chance_binomial <- function(x, ...) x$formula
model.frame.atlas_chance_binomial <- function(formula, ...) formula$model_frame
logLik.atlas_chance_binomial <- function(object, ...) {
  structure(object$logLik_value, df = length(object$coefficients), nobs = length(object$observed),
            class = "logLik")
}
predict.atlas_chance_binomial <- function(object, newdata = NULL,
                                          type = c("link", "response"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    eta <- object$linear_predictors
  } else {
    X <- stats::model.matrix(stats::delete.response(object$terms), newdata,
                             xlev = object$xlevels)
    eta <- as.vector(X %*% object$coefficients)
  }
  if (type == "link") as.numeric(eta) else atlas_chance_inv(eta, object$chance)
}
simulate.atlas_chance_binomial <- function(object, nsim = 1, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  out <- replicate(nsim, stats::rbinom(length(object$observed), object$trials,
                                      object$fitted_probability), simplify = FALSE)
  names(out) <- paste0("sim_", seq_len(nsim))
  as.data.frame(out, optional = TRUE)
}
residuals.atlas_chance_binomial <- function(object, type = c("response", "pearson"), ...) {
  type <- match.arg(type)
  raw <- object$observed - object$trials * object$fitted_probability
  if (type == "response") return(raw)
  variance <- object$trials * object$fitted_probability * (1 - object$fitted_probability)
  raw / sqrt(variance)
}

atlas_raw_path <- function(kind, scenario_id, run_type, B_requested) {
  file.path(atlas_root(), "raw", sprintf("%s-%s-%s-B%d.rds", kind, scenario_id,
                                          run_type, B_requested))
}

atlas_raw_is_complete <- function(value, B_requested) {
  is.data.frame(value) && nrow(value) > 0 &&
    all(c("replication", "B_requested") %in% names(value)) &&
    identical(sort(unique(value$replication)), seq_len(B_requested)) &&
    all(value$B_requested == B_requested)
}

atlas_raw_complete <- function(path, B_requested) {
  if (!file.exists(path)) return(FALSE)
  value <- try(readRDS(path), silent = TRUE)
  !inherits(value, "try-error") && atlas_raw_is_complete(value, B_requested)
}

atlas_write_rds_atomic <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(object, temporary, compress = "xz")
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) stop("Could not finalize raw file: ", path, call. = FALSE)
  invisible(path)
}
