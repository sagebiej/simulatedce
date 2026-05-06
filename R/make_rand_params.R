#' Generate random parameter draws from simple distribution specifications
#'
#' @description
#' A convenience wrapper that generates individual-level random coefficients
#' from a simple list of distribution specifications.
#' Unlike \code{draw_rand_params}, which uses an apollo-style formula
#' interface, this function lets you specify each parameter's distribution
#' and moments directly.
#'
#' @param bcoef Named list of parameter specifications.
#'   Each element is itself a named list with:
#'   \describe{
#'     \item{\code{dist}}{Character. Distribution type: \code{"normal"},
#'       \code{"lognormal"}, \code{"neg_lognormal"}, \code{"uniform"},
#'       \code{"triangular"}, or \code{"fixed"}.}
#'     \item{\code{...}}{Distribution-specific moment parameters (see Details).}
#'   }
#' @param n_resp Positive integer. Number of respondents.
#' @param respondent_ids Optional vector of length \code{n_resp} for the
#'   \code{ID} column. Defaults to \code{1:n_resp}.
#'
#' @return A data frame with \code{n_resp} rows and columns \code{ID} plus
#'   one column per parameter in \code{bcoef}.
#'
#' @details
#' Supported distributions and their required moment parameters:
#'
#' \describe{
#'   \item{\code{"normal"}}{\code{mean}, \code{sd}:
#'     draws from \eqn{N(\mu, \sigma)}.}
#'   \item{\code{"lognormal"}}{\code{meanlog}, \code{sdlog}:
#'     draws from \eqn{\exp(N(\mu_{log}, \sigma_{log}))}.
#'     All values are positive.}
#'   \item{\code{"neg_lognormal"}}{\code{meanlog}, \code{sdlog}:
#'     draws from \eqn{-\exp(N(\mu_{log}, \sigma_{log}))}.
#'     All values are negative. Useful for price coefficients.}
#'   \item{\code{"uniform"}}{\code{min}, \code{max}:
#'     draws from \eqn{U(\min, \max)}.}
#'   \item{\code{"triangular"}}{\code{min}, \code{max}, \code{mode}:
#'     draws from a triangular distribution with given bounds and mode.}
#'   \item{\code{"fixed"}}{\code{value}: no heterogeneity;
#'     every respondent gets the same value.}
#' }
#'
#' @seealso \code{draw_rand_params} for a more flexible, formula-based
#'   interface.
#' @export
#'
#' @examples
#' bcoef <- list(
#'   bprice = list(dist = "normal", mean = -0.5, sd = 0.2),
#'   bqual  = list(dist = "lognormal", meanlog = -1, sdlog = 0.3),
#'   benv   = list(dist = "fixed", value = 0.5)
#' )
#'
#' set.seed(42)
#' draws <- make_rand_params(bcoef, n_resp = 100)
#' head(draws)
#'
make_rand_params <- function(bcoef, n_resp, respondent_ids = NULL) {

  # ── validate inputs ──────────────────────────────────────────────────────────
  if (!is.list(bcoef) || is.null(names(bcoef)) || any(names(bcoef) == ""))
    stop("`bcoef` must be a fully named list.")

  if (!is.numeric(n_resp) || length(n_resp) != 1L ||
      n_resp < 1 || n_resp != as.integer(n_resp))
    stop("`n_resp` must be a single positive integer.")
  n_resp <- as.integer(n_resp)

  if (!is.null(respondent_ids)) {
    if (length(respondent_ids) != n_resp)
      stop("`respondent_ids` must have length equal to `n_resp` (", n_resp, ").")
  } else {
    respondent_ids <- seq_len(n_resp)
  }

  supported <- c("normal", "lognormal", "neg_lognormal",
                  "uniform", "triangular", "fixed")

  out <- data.frame(ID = respondent_ids)

  for (nm in names(bcoef)) {
    spec <- bcoef[[nm]]

    if (!is.list(spec) || is.null(spec[["dist"]]))
      stop(glue::glue(
        "`bcoef[['{nm}']]` must be a list with at least a `dist` element."
      ))

    dist <- spec[["dist"]]
    if (!dist %in% supported)
      stop(glue::glue(
        "Unknown distribution '{dist}' for parameter '{nm}'. ",
        "Supported: {paste(supported, collapse = ', ')}."
      ))

    out[[nm]] <- switch(dist,
      "normal" = {
        check_moments(spec, nm, c("mean", "sd"))
        stats::rnorm(n_resp, mean = spec[["mean"]], sd = spec[["sd"]])
      },
      "lognormal" = {
        check_moments(spec, nm, c("meanlog", "sdlog"))
        exp(stats::rnorm(n_resp, mean = spec[["meanlog"]], sd = spec[["sdlog"]]))
      },
      "neg_lognormal" = {
        check_moments(spec, nm, c("meanlog", "sdlog"))
        -exp(stats::rnorm(n_resp, mean = spec[["meanlog"]], sd = spec[["sdlog"]]))
      },
      "uniform" = {
        check_moments(spec, nm, c("min", "max"))
        stats::runif(n_resp, min = spec[["min"]], max = spec[["max"]])
      },
      "triangular" = {
        check_moments(spec, nm, c("min", "max", "mode"))
        draw_triangular(n_resp, spec[["min"]], spec[["max"]], spec[["mode"]])
      },
      "fixed" = {
        check_moments(spec, nm, "value")
        rep(spec[["value"]], n_resp)
      }
    )
  }

  out
}

#' Check that required moment parameters are present
#' @noRd
check_moments <- function(spec, param_name, required) {
  missing_args <- setdiff(required, names(spec))
  if (length(missing_args) > 0)
    stop(glue::glue(
      "Parameter '{param_name}' (dist = '{spec$dist}') is missing required ",
      "argument(s): {paste(missing_args, collapse = ', ')}."
    ))
}

#' Draw from a triangular distribution via inverse CDF
#' @noRd
draw_triangular <- function(n, a, b, c) {
  if (a >= b) stop("Triangular distribution requires min < max.")
  if (c < a || c > b) stop("Triangular distribution requires min <= mode <= max.")
  u <- stats::runif(n)
  fc <- (c - a) / (b - a)
  ifelse(
    u < fc,
    a + sqrt(u * (b - a) * (c - a)),
    b - sqrt((1 - u) * (b - a) * (b - c))
  )
}
