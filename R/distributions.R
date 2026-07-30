#' Supported coefficient distributions
#'
#' One place that knows every distribution `bcoeff` accepts, which arguments it
#' needs, how to draw from it, and what its mean and standard deviation are.
#' Everything else in the package reads from here, so adding a distribution is a
#' single edit.
#'
#' @format A named list. Each element describes one distribution and holds
#'   \describe{
#'     \item{`args`}{the required arguments, in the order they are reported}
#'     \item{`draw`}{`function(n, spec)` returning `n` draws}
#'     \item{`mean`}{`function(spec)` giving the population mean}
#'     \item{`sd`}{`function(spec)` giving the population standard deviation}
#'   }
#' @noRd
#' @details
#' The `mixed` element says whether a mixed logit can be written for this shape in
#' mixl, and if so which arguments its two estimated parameters correspond to.
#' `wrap` turns the linear index `b + sigma * draw` into the coefficient. mixl's
#' `draw_*` tokens are standard normal, so a normal parameter needs no wrapping and
#' a lognormal one is the exponential of the index.
#' @noRd
dce_distributions <- list(
  fixed = list(
    args = "value",
    draw = function(n, spec) rep(spec[["value"]], n),
    q    = function(u, spec) rep(spec[["value"]], length(u)),
    mean = function(spec) spec[["value"]],
    sd   = function(spec) 0,
    mixed = NULL
  ),
  normal = list(
    args = c("mean", "sd"),
    draw = function(n, spec) stats::rnorm(n, mean = spec[["mean"]], sd = spec[["sd"]]),
    q    = function(u, spec) stats::qnorm(u, mean = spec[["mean"]], sd = spec[["sd"]]),
    mean = function(spec) spec[["mean"]],
    sd   = function(spec) spec[["sd"]],
    mixed = list(
      location = "mean", scale = "sd",
      wrap = function(index) paste0("(", index, ")")
    )
  ),
  lognormal = list(
    args = c("meanlog", "sdlog"),
    draw = function(n, spec) exp(stats::rnorm(n, mean = spec[["meanlog"]], sd = spec[["sdlog"]])),
    q    = function(u, spec) exp(stats::qnorm(u, mean = spec[["meanlog"]], sd = spec[["sdlog"]])),
    mean = function(spec) exp(spec[["meanlog"]] + spec[["sdlog"]]^2 / 2),
    sd   = function(spec) {
      s2 <- spec[["sdlog"]]^2
      sqrt((exp(s2) - 1) * exp(2 * spec[["meanlog"]] + s2))
    },
    mixed = list(
      location = "meanlog", scale = "sdlog",
      wrap = function(index) paste0("(exp(", index, "))")
    )
  ),
  neg_lognormal = list(
    args = c("meanlog", "sdlog"),
    draw = function(n, spec) -exp(stats::rnorm(n, mean = spec[["meanlog"]], sd = spec[["sdlog"]])),
    ## the sign flip reverses the order, so the upper tail of u maps to the lower
    ## tail of the coefficient
    q    = function(u, spec) -exp(stats::qnorm(1 - u, mean = spec[["meanlog"]], sd = spec[["sdlog"]])),
    mean = function(spec) -exp(spec[["meanlog"]] + spec[["sdlog"]]^2 / 2),
    sd   = function(spec) {
      s2 <- spec[["sdlog"]]^2
      sqrt((exp(s2) - 1) * exp(2 * spec[["meanlog"]] + s2))
    },
    mixed = list(
      location = "meanlog", scale = "sdlog",
      wrap = function(index) paste0("(-exp(", index, "))")
    )
  ),
  uniform = list(
    args = c("min", "max"),
    draw = function(n, spec) stats::runif(n, min = spec[["min"]], max = spec[["max"]]),
    q    = function(u, spec) stats::qunif(u, min = spec[["min"]], max = spec[["max"]]),
    mean = function(spec) (spec[["min"]] + spec[["max"]]) / 2,
    sd   = function(spec) (spec[["max"]] - spec[["min"]]) / sqrt(12)
  ),
  triangular = list(
    args = c("min", "max", "mode"),
    draw = function(n, spec) draw_triangular(n, spec[["min"]], spec[["max"]], spec[["mode"]]),
    q    = function(u, spec) qtriangular(u, spec[["min"]], spec[["max"]], spec[["mode"]]),
    mean = function(spec) (spec[["min"]] + spec[["max"]] + spec[["mode"]]) / 3,
    sd   = function(spec) {
      a <- spec[["min"]]
      b <- spec[["max"]]
      c <- spec[["mode"]]
      sqrt((a^2 + b^2 + c^2 - a * b - a * c - b * c) / 18)
    }
  ),
  truncated_normal = list(
    args = c("mean", "sd", "min", "max"),
    draw = function(n, spec) {
      draw_truncated_normal(n, spec[["mean"]], spec[["sd"]], spec[["min"]], spec[["max"]])
    },
    q    = function(u, spec) {
      qtruncnorm(u, spec[["mean"]], spec[["sd"]], spec[["min"]], spec[["max"]])
    },
    mean = function(spec) truncnorm_moments(spec)[["mean"]],
    sd   = function(spec) truncnorm_moments(spec)[["sd"]]
  )
)

#' Names of the supported distributions
#' @noRd
supported_dists <- function() names(dce_distributions)

#' Turn a bcoeff element into a canonical distribution spec
#'
#' A bare number is shorthand for `list(dist = "fixed", value = x)`. Anything
#' else must be a list carrying a `dist` element. Errors name the offending
#' parameter, because `bcoeff` lists get long.
#'
#' @param spec One element of `bcoeff`.
#' @param nm The name of that element, used in error messages.
#' @return The spec as a list, validated.
#' @noRd
as_dist_spec <- function(spec, nm = "<unnamed>") {
  if (is.numeric(spec)) {
    if (length(spec) != 1L) {
      stop(glue::glue(
        "`bcoeff[['{nm}']]` must be a single number, not a vector of length {length(spec)}."
      ), call. = FALSE)
    }
    return(list(dist = "fixed", value = spec))
  }

  if (!is.list(spec) || is.null(spec[["dist"]])) {
    stop(glue::glue(
      "`bcoeff[['{nm}']]` must be a numeric scalar or a list with a `dist` element."
    ), call. = FALSE)
  }

  dist <- spec[["dist"]]
  if (!is.character(dist) || length(dist) != 1L) {
    stop(glue::glue("`dist` for parameter '{nm}' must be a single distribution name."),
      call. = FALSE
    )
  }
  if (!dist %in% supported_dists()) {
    stop(
      glue::glue(
        "Unknown distribution '{dist}' for parameter '{nm}'. ",
        "Supported: {paste(supported_dists(), collapse = ', ')}."
      ),
      did_you_mean(dist, supported_dists()),
      call. = FALSE
    )
  }

  required <- dce_distributions[[dist]]$args
  missing_args <- setdiff(required, names(spec))
  if (length(missing_args) > 0) {
    stop(glue::glue(
      "Parameter '{nm}' (dist = '{dist}') is missing required ",
      "argument(s): {paste(missing_args, collapse = ', ')}."
    ), call. = FALSE)
  }

  empty <- required[vapply(required, function(a) length(spec[[a]]) != 1L, logical(1))]
  if (length(empty) > 0) {
    stop(glue::glue(
      "Parameter '{nm}' (dist = '{dist}') needs a single value for ",
      "argument(s): {paste(empty, collapse = ', ')}."
    ), call. = FALSE)
  }

  not_numeric <- required[vapply(required, function(a) !is.numeric(spec[[a]]), logical(1))]
  if (length(not_numeric) > 0) {
    stop(glue::glue(
      "Parameter '{nm}' (dist = '{dist}') needs numeric ",
      "argument(s): {paste(not_numeric, collapse = ', ')}."
    ), call. = FALSE)
  }

  spec
}

#' Draw n values from a canonical spec
#' @noRd
draw_from_spec <- function(spec, n) {
  dce_distributions[[spec[["dist"]]]]$draw(n, spec)
}

#' Map uniform numbers on (0, 1) to a spec's marginal distribution
#'
#' This is what makes correlated draws possible: correlate on the uniform scale,
#' then push each one through its own marginal.
#' @noRd
quantile_from_spec <- function(spec, u) {
  dce_distributions[[spec[["dist"]]]]$q(u, spec)
}

#' Population mean implied by a bcoeff element
#'
#' This is the value an MNL should recover, so it is what `truepar` reports.
#' @noRd
spec_mean <- function(spec, nm = "<unnamed>") {
  spec <- as_dist_spec(spec, nm)
  as.numeric(dce_distributions[[spec[["dist"]]]]$mean(spec))
}

#' Population standard deviation implied by a bcoeff element
#'
#' Zero for fixed parameters.
#' @noRd
spec_sd <- function(spec, nm = "<unnamed>") {
  spec <- as_dist_spec(spec, nm)
  as.numeric(dce_distributions[[spec[["dist"]]]]$sd(spec))
}

#' Is any element of bcoeff a random parameter?
#' @noRd
has_random_params <- function(bcoeff) {
  !all(vapply(bcoeff, is.numeric, logical(1)))
}

#' One line describing a bcoeff element, for the startup summary
#' @noRd
describe_spec <- function(spec, nm) {
  spec <- as_dist_spec(spec, nm)
  dist <- spec[["dist"]]

  if (identical(dist, "fixed")) {
    return(sprintf("  %-20s %-16s value = %g", nm, "fixed", spec[["value"]]))
  }

  args <- dce_distributions[[dist]]$args
  detail <- paste(
    sprintf("%s = %g", args, vapply(args, function(a) as.numeric(spec[[a]]), numeric(1))),
    collapse = ", "
  )

  ## Spell out what the specification implies for the coefficient itself, unless
  ## the arguments already are the mean and sd and repeating them adds nothing.
  m <- spec_mean(spec, nm)
  s <- spec_sd(spec, nm)
  declared_moments <- identical(args, c("mean", "sd")) &&
    isTRUE(all.equal(m, as.numeric(spec[["mean"]]))) &&
    isTRUE(all.equal(s, as.numeric(spec[["sd"]])))

  if (declared_moments) {
    return(sprintf("  %-20s %-16s %s", nm, dist, detail))
  }
  sprintf(
    "  %-20s %-16s %s  -> mean = %g, sd = %g",
    nm, dist, detail, m, s
  )
}

#' A tidy table of the true parameter values behind a bcoeff list
#'
#' Used by `aggregateResults()` to attach `truepar` and `truesd` to the summary.
#' Dots in names become underscores, matching what mixl does to the coefficient
#' names during estimation.
#'
#' With `model = "mnl"` there is one row per coefficient, and `truepar` is the mean
#' of the mixing distribution, which is what a multinomial logit recovers. With
#' `model = "mixed"` each random coefficient contributes two rows, because a mixed
#' logit estimates the distribution's own parameters rather than its moments: for a
#' normal that is the mean and the standard deviation, for a lognormal the `meanlog`
#' and `sdlog` of the underlying normal.
#' @noRd
bcoeff_table <- function(bcoeff, model = c("mnl", "mixed")) {
  model <- match.arg(model)
  empty <- data.frame(
    parname = character(0), truepar = numeric(0), truesd = numeric(0),
    stringsAsFactors = FALSE
  )
  if (is.null(bcoeff) || length(bcoeff) == 0) {
    return(empty)
  }

  nms <- names(bcoeff)
  clean <- stringr::str_replace_all(nms, "\\.", "_")

  if (identical(model, "mnl")) {
    return(data.frame(
      parname = clean,
      truepar = vapply(nms, function(nm) spec_mean(bcoeff[[nm]], nm), numeric(1)),
      truesd  = vapply(nms, function(nm) spec_sd(bcoeff[[nm]], nm), numeric(1)),
      row.names = NULL,
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (i in seq_along(nms)) {
    spec <- as_dist_spec(bcoeff[[nms[i]]], nms[i])
    mixed <- dce_distributions[[spec[["dist"]]]]$mixed

    if (identical(spec[["dist"]], "fixed") || is.null(mixed)) {
      ## a fixed coefficient, or a shape with no mixed form, is estimated as a
      ## single number and compared against its mean
      rows[[length(rows) + 1L]] <- data.frame(
        parname = clean[i],
        truepar = spec_mean(spec, nms[i]),
        truesd = spec_sd(spec, nms[i]),
        stringsAsFactors = FALSE
      )
      next
    }

    rows[[length(rows) + 1L]] <- data.frame(
      parname = clean[i],
      truepar = as.numeric(spec[[mixed$location]]),
      truesd = NA_real_,
      stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
      parname = paste0("sigma_", clean[i]),
      truepar = as.numeric(spec[[mixed$scale]]),
      truesd = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Which coefficients would a mixed logit treat as random?
#' @noRd
mixed_random_params <- function(bcoeff) {
  nms <- names(bcoeff)
  keep <- vapply(nms, function(nm) {
    spec <- as_dist_spec(bcoeff[[nm]], nm)
    !identical(spec[["dist"]], "fixed")
  }, logical(1))
  nms[keep]
}

#' Check that every random coefficient has a shape mixl can estimate
#' @noRd
check_mixed_supported <- function(bcoeff) {
  random <- mixed_random_params(bcoeff)

  if (length(random) == 0) {
    stop(
      'model = "mixed" needs at least one random coefficient, but every entry of ',
      "`bcoeff` is a fixed number. Either give one of them a distribution, or use ",
      'model = "mnl".',
      call. = FALSE
    )
  }

  supported <- names(Filter(function(d) !is.null(d$mixed), dce_distributions))
  shapes <- vapply(random, function(nm) as_dist_spec(bcoeff[[nm]], nm)[["dist"]], character(1))
  bad <- random[!shapes %in% supported]

  if (length(bad) > 0) {
    stop(
      "A mixed logit cannot be written for ",
      and_list(paste0("`", bad, "` (", shapes[bad], ")")),
      ". Shapes that can be estimated: ", and_list(supported, "and"),
      ". Either change the distribution, or simulate with estimate = FALSE and fit ",
      "the model you want yourself.",
      call. = FALSE
    )
  }

  random
}

#' Rewrite an mixl utility script so the random coefficients vary by respondent
#'
#' Each random coefficient `b` becomes `b + sigma_b * draw_b`, wrapped according to
#' its distribution. mixl treats any `draw_*` token as a standard normal draw held
#' constant within a respondent, and assigns the draw dimensions in order of first
#' appearance, so the token names only have to be distinct.
#' @noRd
build_mixed_script <- function(script, bcoeff, random = NULL) {
  if (is.null(random)) random <- check_mixed_supported(bcoeff)

  for (nm in random) {
    spec <- as_dist_spec(bcoeff[[nm]], nm)
    mixed <- dce_distributions[[spec[["dist"]]]]$mixed
    index <- paste0("@", nm, " + @sigma_", nm, " * draw_", nm)
    script <- gsub(paste0("@", nm), mixed$wrap(index), script, fixed = TRUE)
  }

  script
}

#' Start values for a mixed logit, warm-started from the multinomial logit fit
#'
#' A scale parameter must not start at zero: the draws are symmetric, so the
#' gradient there is zero and the optimiser cannot move. For the lognormal shapes
#' the location is on the log scale, so the multinomial estimate is logged first.
#' @noRd
mixed_start_values <- function(mnl_coefficients, beta_names, bcoeff, random) {
  start <- stats::setNames(rep(0, length(beta_names)), beta_names)

  for (nm in intersect(names(mnl_coefficients), beta_names)) {
    start[[nm]] <- mnl_coefficients[[nm]]
  }

  for (nm in random) {
    spec <- as_dist_spec(bcoeff[[nm]], nm)
    mixed <- dce_distributions[[spec[["dist"]]]]$mixed
    from_mnl <- if (nm %in% names(mnl_coefficients)) mnl_coefficients[[nm]] else NA_real_

    if (nm %in% beta_names) {
      start[[nm]] <- if (identical(mixed$location, "meanlog")) {
        ## the multinomial estimate approximates the coefficient's mean, and
        ## meanlog is roughly its log
        if (is.finite(from_mnl) && abs(from_mnl) > 1e-8) log(abs(from_mnl)) else -1
      } else if (is.finite(from_mnl)) {
        from_mnl
      } else {
        0
      }
    }

    sigma <- paste0("sigma_", nm)
    if (sigma %in% beta_names) {
      start[[sigma]] <- max(0.1, abs(start[[nm]]) / 2)
    }
  }

  start[beta_names]
}

#' Draw from a triangular distribution via the inverse CDF
#' @noRd
draw_triangular <- function(n, a, b, c) {
  qtriangular(stats::runif(n), a, b, c)
}

#' Quantile function of a triangular distribution
#' @noRd
qtriangular <- function(u, a, b, c) {
  if (a >= b) stop("Triangular distribution requires min < max.", call. = FALSE)
  if (c < a || c > b) stop("Triangular distribution requires min <= mode <= max.", call. = FALSE)
  fc <- (c - a) / (b - a)
  ifelse(
    u < fc,
    a + sqrt(u * (b - a) * (c - a)),
    b - sqrt((1 - u) * (b - a) * (b - c))
  )
}

#' Draw from a normal truncated to [lower, upper] via the inverse CDF
#' @noRd
draw_truncated_normal <- function(n, mean, sd, lower, upper) {
  qtruncnorm(stats::runif(n), mean, sd, lower, upper)
}

#' Quantile function of a truncated normal
#' @noRd
qtruncnorm <- function(u, mean, sd, lower, upper) {
  if (lower >= upper) stop("Truncated normal requires min < max.", call. = FALSE)
  if (sd <= 0) stop("Truncated normal requires sd > 0.", call. = FALSE)
  p_lo <- stats::pnorm(lower, mean = mean, sd = sd)
  p_hi <- stats::pnorm(upper, mean = mean, sd = sd)
  if (p_hi - p_lo < .Machine$double.eps) {
    stop("Truncated normal has no probability mass between min and max.", call. = FALSE)
  }
  stats::qnorm(p_lo + u * (p_hi - p_lo), mean = mean, sd = sd)
}

#' Mean and sd of a truncated normal
#'
#' One-sided truncation is common (a price coefficient held below zero, say), so
#' an infinite bound has to work. At an infinite bound the density is zero and the
#' `bound * density` term vanishes, which `Inf * 0` would otherwise turn into NaN.
#' @noRd
truncnorm_moments <- function(spec) {
  mu <- spec[["mean"]]
  sg <- spec[["sd"]]
  alpha <- (spec[["min"]] - mu) / sg
  beta <- (spec[["max"]] - mu) / sg
  z <- stats::pnorm(beta) - stats::pnorm(alpha)
  d_a <- stats::dnorm(alpha)
  d_b <- stats::dnorm(beta)

  times_density <- function(bound, density) if (is.finite(bound)) bound * density else 0

  m <- mu + sg * (d_a - d_b) / z
  v <- sg^2 * (1 +
    (times_density(alpha, d_a) - times_density(beta, d_b)) / z -
    ((d_a - d_b) / z)^2)
  list(mean = m, sd = sqrt(max(v, 0)))
}
