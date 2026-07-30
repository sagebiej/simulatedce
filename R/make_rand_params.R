#' Generate random parameter draws from simple distribution specifications
#'
#' @description
#' Generates individual-level random coefficients from a simple list of
#' distribution specifications. Used internally by \code{\link{simulate_choices}}
#' when any element of \code{bcoeff} is a distribution spec list.
#'
#' @param bcoeff Named list of parameter specifications.
#'   Each element is either:
#'   \describe{
#'     \item{A numeric scalar}{Represents a fixed coefficient (same for all respondents).}
#'     \item{A named list}{Defines a random distribution. Must contain a \code{dist} element.}
#'   }
#'   For random distributions, use:
#'   \describe{
#'     \item{\code{"normal"}}{\code{mean}, \code{sd}:
#'       draws from \eqn{N(\mu, \sigma)}.}
#'     \item{\code{"lognormal"}}{\code{meanlog}, \code{sdlog}:
#'       draws from \eqn{\exp(N(\mu_{log}, \sigma_{log}))}.
#'       All values are positive.}
#'     \item{\code{"neg_lognormal"}}{\code{meanlog}, \code{sdlog}:
#'       draws from \eqn{-\exp(N(\mu_{log}, \sigma_{log}))}.
#'       All values are negative. Useful for price coefficients.}
#'     \item{\code{"uniform"}}{\code{min}, \code{max}:
#'       draws from \eqn{U(\min, \max)}.}
#'     \item{\code{"triangular"}}{\code{min}, \code{max}, \code{mode}:
#'       draws from a triangular distribution with given bounds and mode.}
#'     \item{\code{"truncated_normal"}}{\code{mean}, \code{sd}, \code{min},
#'       \code{max}: draws from \eqn{N(\mu, \sigma)} restricted to
#'       \eqn{[\min, \max]}. Useful when a coefficient must keep its sign.}
#'   }
#'   Note that \code{meanlog} and \code{sdlog} describe the underlying normal,
#'   not the coefficient itself. Call \code{\link{bcoeff_moments}} to see the
#'   mean and standard deviation each specification implies.
#'
#' @param n_resp Positive integer. Number of respondents.
#' @param respondent_ids Optional vector of length \code{n_resp} for the
#'   \code{ID} column. Defaults to \code{1:n_resp}.
#'
#' @return A data frame with \code{n_resp} rows and columns \code{ID} plus
#'   one column per parameter in \code{bcoeff}.
#'
#' @seealso \code{\link{bcoeff_moments}} to inspect the implied moments of a
#'   specification without drawing from it.
#'
#' @export
#'
#' @examples
#' bcoeff <- list(
#'   bprice = list(dist = "normal", mean = -0.5, sd = 0.2),
#'   bqual  = 0.8
#' )
#'
#' set.seed(42)
#' draws <- make_rand_params(bcoeff, n_resp = 100)
#' head(draws)
#'
make_rand_params <- function(bcoeff, n_resp, respondent_ids = NULL) {
  check_bcoeff_list(bcoeff)

  if (!is.numeric(n_resp) || length(n_resp) != 1L || is.na(n_resp) ||
    n_resp < 1 || n_resp != as.integer(n_resp)) {
    stop(
      "`n_resp` must be a single positive whole number, not ",
      describe_value(n_resp), ".",
      call. = FALSE
    )
  }
  n_resp <- as.integer(n_resp)

  if (!is.null(respondent_ids)) {
    if (length(respondent_ids) != n_resp) {
      stop(
        "`respondent_ids` has ", length(respondent_ids), " element(s) but ",
        "`n_resp` is ", n_resp, ". Supply one id per respondent, or leave ",
        "`respondent_ids` empty to use 1:", n_resp, ".",
        call. = FALSE
      )
    }
    if (anyDuplicated(respondent_ids)) {
      stop(
        "`respondent_ids` must be unique: each respondent gets one draw. ",
        "Duplicated: ",
        paste(unique(respondent_ids[duplicated(respondent_ids)]), collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  } else {
    respondent_ids <- seq_len(n_resp)
  }

  out <- data.frame(ID = respondent_ids)

  for (nm in names(bcoeff)) {
    spec <- as_dist_spec(bcoeff[[nm]], nm)
    out[[nm]] <- draw_from_spec(spec, n_resp)
  }

  out
}


#' Report the mean and standard deviation implied by a bcoeff list
#'
#' @description
#' Shows what each entry of \code{bcoeff} means in the units of the coefficient
#' itself. This is useful for the distributions that are parameterised on
#' another scale: \code{lognormal} and \code{neg_lognormal} take
#' \code{meanlog} and \code{sdlog}, which describe the underlying normal rather
#' than the coefficient. The \code{mean} column is the value a multinomial logit
#' should recover, and is what \code{\link{aggregateResults}} reports as
#' \code{truepar}.
#'
#' @param bcoeff A named list of parameter specifications, as passed to
#'   \code{\link{sim_all}}. See \code{\link{make_rand_params}}.
#'
#' @return A data frame with one row per parameter and the columns
#'   \code{parameter}, \code{dist}, \code{mean} and \code{sd}.
#'
#' @export
#'
#' @examples
#' bcoeff <- list(
#'   bprice = list(dist = "neg_lognormal", meanlog = -3, sdlog = 0.5),
#'   bqual  = list(dist = "triangular", min = 0, max = 1, mode = 0.2),
#'   basc   = 0.4
#' )
#' bcoeff_moments(bcoeff)
#'
bcoeff_moments <- function(bcoeff) {
  check_bcoeff_list(bcoeff)
  nms <- names(bcoeff)
  data.frame(
    parameter = nms,
    dist = vapply(nms, function(nm) as_dist_spec(bcoeff[[nm]], nm)[["dist"]], character(1)),
    mean = vapply(nms, function(nm) spec_mean(bcoeff[[nm]], nm), numeric(1)),
    sd   = vapply(nms, function(nm) spec_sd(bcoeff[[nm]], nm), numeric(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


#' Validate the shape of a bcoeff list
#'
#' Checks only the container. Individual specifications are validated by
#' `as_dist_spec()` when they are used, so the error can name the parameter.
#' @noRd
check_bcoeff_list <- function(bcoeff, arg = "bcoeff") {
  if (!is.list(bcoeff)) {
    stop(
      "`", arg, "` must be a list of parameter values, not ",
      describe_value(bcoeff), ". For example: ",
      "list(bprice = -0.2, bqual = list(dist = \"normal\", mean = 1, sd = 0.5)).",
      call. = FALSE
    )
  }
  if (length(bcoeff) == 0) {
    stop("`", arg, "` is empty. Give one entry per coefficient in your utility functions.",
      call. = FALSE
    )
  }
  nms <- names(bcoeff)
  if (is.null(nms) || any(is.na(nms)) || any(!nzchar(nms))) {
    unnamed <- if (is.null(nms)) seq_along(bcoeff) else which(is.na(nms) | !nzchar(nms))
    stop(
      "Every element of `", arg, "` needs a name matching a coefficient in your ",
      "utility functions. Unnamed element(s) at position(s): ",
      paste(unnamed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(nms)) {
    stop(
      "`", arg, "` has duplicated names: ",
      paste(unique(nms[duplicated(nms)]), collapse = ", "),
      ". Each coefficient may appear only once.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
