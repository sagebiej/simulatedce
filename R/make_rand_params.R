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
#' @param correlation Optional correlation matrix over some or all of the random
#'   coefficients, with row and column names saying which. [correlate()] builds one
#'   from pairwise values. Coefficients it does not mention are drawn independently
#'   as before.
#'
#'   Correlation is imposed with a Gaussian copula: correlated standard normals are
#'   drawn, turned into uniforms, and pushed through each coefficient's own
#'   marginal. That works whatever the marginals are, so a lognormal price
#'   coefficient can be correlated with a triangular quality coefficient. The
#'   number you give is the correlation of the underlying normals. For normal
#'   marginals that is exactly the correlation of the coefficients; for the others
#'   it is close but not identical, because the transformation is not linear. The
#'   rank correlation is preserved exactly in every case.
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
make_rand_params <- function(bcoeff, n_resp, respondent_ids = NULL,
                             correlation = NULL) {
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

  if (is.null(correlation)) {
    for (nm in names(bcoeff)) {
      spec <- as_dist_spec(bcoeff[[nm]], nm)
      out[[nm]] <- draw_from_spec(spec, n_resp)
    }
    return(out)
  }

  ## ---- correlated draws ----------------------------------------------------
  ##
  ## Correlate on the normal scale, turn that into uniforms, then push each one
  ## through its own marginal. This is a Gaussian copula, and it works whatever the
  ## marginals are, so a correlated lognormal price coefficient and a correlated
  ## triangular quality coefficient are both expressible.

  r <- check_correlation(correlation, bcoeff)
  correlated <- colnames(r)

  chol_r <- chol(r)
  z <- matrix(stats::rnorm(n_resp * ncol(r)), nrow = n_resp, ncol = ncol(r)) %*% chol_r
  u <- stats::pnorm(z)
  colnames(u) <- correlated

  for (nm in names(bcoeff)) {
    spec <- as_dist_spec(bcoeff[[nm]], nm)
    out[[nm]] <- if (nm %in% correlated) {
      quantile_from_spec(spec, u[, nm])
    } else {
      draw_from_spec(spec, n_resp)
    }
  }

  out
}


#' Validate a correlation matrix against the coefficients it refers to
#' @noRd
check_correlation <- function(correlation, bcoeff, arg = "correlation") {
  if (!is.matrix(correlation) || !is.numeric(correlation)) {
    stop(
      "`", arg, "` must be a numeric matrix with one row and column per ",
      "correlated coefficient, not ", describe_value(correlation),
      ". Name its rows and columns after the coefficients.",
      call. = FALSE
    )
  }
  if (nrow(correlation) != ncol(correlation)) {
    stop(
      "`", arg, "` must be square, but it is ", nrow(correlation), " by ",
      ncol(correlation), ".",
      call. = FALSE
    )
  }

  nms <- colnames(correlation)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(
      "`", arg, "` needs column names saying which coefficients it refers to. ",
      "Use dimnames, for example ",
      "matrix(c(1, 0.5, 0.5, 1), 2, dimnames = list(c(\"bprice\", \"bqual\"), ",
      "c(\"bprice\", \"bqual\"))).",
      call. = FALSE
    )
  }
  if (!is.null(rownames(correlation)) && !identical(rownames(correlation), nms)) {
    stop("`", arg, "` must have the same row and column names, in the same order.",
      call. = FALSE
    )
  }

  unknown <- setdiff(nms, names(bcoeff))
  if (length(unknown) > 0) {
    stop(
      "`", arg, "` refers to ", and_list(paste0("`", unknown, "`")),
      ", which are not in `bcoeff`.",
      call. = FALSE
    )
  }

  fixed <- nms[vapply(nms, function(nm) {
    identical(as_dist_spec(bcoeff[[nm]], nm)[["dist"]], "fixed")
  }, logical(1))]
  if (length(fixed) > 0) {
    stop(
      "`", arg, "` refers to ", and_list(paste0("`", fixed, "`")),
      ", which are fixed numbers. Only random coefficients can be correlated; ",
      "give them a distribution first.",
      call. = FALSE
    )
  }

  if (any(abs(diag(correlation) - 1) > 1e-8)) {
    stop("`", arg, "` must have 1 on its diagonal.", call. = FALSE)
  }
  if (!isTRUE(all.equal(correlation, t(correlation), tolerance = 1e-8))) {
    stop("`", arg, "` must be symmetric.", call. = FALSE)
  }
  if (any(abs(correlation) > 1 + 1e-8)) {
    stop("`", arg, "` holds a value outside -1 to 1.", call. = FALSE)
  }

  eigen_values <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigen_values) <= 1e-10) {
    stop(
      "`", arg, "` is not a valid correlation matrix: its smallest eigenvalue is ",
      format(min(eigen_values), digits = 3),
      ", so no set of variables can have these correlations. Reduce the ",
      "off-diagonal values.",
      call. = FALSE
    )
  }

  correlation
}


#' Build a correlation matrix from pairwise values
#'
#' @description
#' A convenience wrapper for the `correlation` argument of [sim_all()], so you can
#' write the pairs you care about rather than assembling a matrix by hand.
#'
#' @param ... Named pairs, each `c("first", "second") = value`, or a single named
#'   list of the same.
#' @param parameters Optional character vector fixing the order of the
#'   coefficients. Taken from the pairs when omitted.
#'
#' @return A correlation matrix with dimnames, ready to pass as `correlation`.
#'
#' @export
#'
#' @examples
#' correlate(c("bprice", "bqual") ~ 0.4)
#'
#' # three coefficients, two of the three pairs correlated
#' correlate(
#'   c("bprice", "bqual") ~ 0.4,
#'   c("bprice", "btime") ~ -0.2
#' )
correlate <- function(..., parameters = NULL) {
  pairs <- list(...)
  if (length(pairs) == 1L && is.list(pairs[[1]]) && !inherits(pairs[[1]], "formula")) {
    pairs <- pairs[[1]]
  }
  if (length(pairs) == 0) {
    stop(
      "Give at least one pair, written as c(\"bprice\", \"bqual\") ~ 0.4.",
      call. = FALSE
    )
  }

  parsed <- lapply(seq_along(pairs), function(i) {
    f <- pairs[[i]]
    if (!inherits(f, "formula")) {
      stop(
        "Pair ", i, " must be written as c(\"first\", \"second\") ~ value, not ",
        describe_value(f), ".",
        call. = FALSE
      )
    }
    who <- eval(formula.tools::lhs(f), envir = baseenv())
    value <- eval(formula.tools::rhs(f), envir = baseenv())
    if (!is.character(who) || length(who) != 2L) {
      stop(
        "The left side of pair ", i, " must name exactly two coefficients, ",
        "as c(\"bprice\", \"bqual\").",
        call. = FALSE
      )
    }
    if (!is.numeric(value) || length(value) != 1L) {
      stop("The right side of pair ", i, " must be a single number.", call. = FALSE)
    }
    list(who = who, value = value)
  })

  if (is.null(parameters)) {
    parameters <- unique(unlist(lapply(parsed, `[[`, "who")))
  }

  r <- diag(length(parameters))
  dimnames(r) <- list(parameters, parameters)
  for (p in parsed) {
    if (!all(p$who %in% parameters)) {
      stop(
        "Pair (", paste(p$who, collapse = ", "), ") names a coefficient that is ",
        "not in `parameters`.",
        call. = FALSE
      )
    }
    r[p$who[1], p$who[2]] <- p$value
    r[p$who[2], p$who[1]] <- p$value
  }

  r
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
