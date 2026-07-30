#' How power grows with the sample size
#'
#' @description
#' Runs the same simulation at several sample sizes and returns one tidy table of
#' the results. This is the answer to "how many respondents do I need", which is
#' the question a funder, an ethics board or a preregistration will ask, and the
#' main reason to simulate before collecting data.
#'
#' @param resps A vector of sample sizes to try.
#' @param ... Passed to [sim_all()]. Everything that function takes works here,
#'   except `resps`, which is what varies. `estimate` is forced to `TRUE`, since
#'   power needs models.
#' @param verbose Progress reporting. At level 1 each sample size is announced as
#'   it starts, which is worth having because a full curve takes a while.
#'
#' @details
#' The runs are independent, so if you set a `seed` the same seed is used for every
#' sample size. That makes the curve reproducible and removes one source of
#' wobble between neighbouring points, at the cost of correlating them: a design
#' that happens to suit that seed flatters every point equally. Leave `seed` empty
#' if you would rather each point be an independent draw.
#'
#' Power estimated from `nosim` runs carries a standard error of at most
#' `50 / sqrt(nosim)` percentage points, so 40 runs gives roughly plus or minus 8
#' points and 400 runs roughly plus or minus 2.5. The `se` column reports it, so
#' you can see whether a difference between two sample sizes means anything.
#'
#' @return A data frame with one row per design, sample size and parameter, and the
#'   columns `design`, `resps`, `parameter`, `power`, `se`, `truepar`, `estimate`,
#'   `sd` and `converged`. Joint power, the share of runs in which every
#'   coefficient was significant at once, appears as the parameter `"(all)"`.
#'
#' @seealso [sim_all()] for a single run, and [check_design()] to confirm the
#'   design can identify the model before spending time on a curve.
#'
#' @export
#'
#' @examples
#' design <- data.frame(
#'   Choice.situation = 1:8,
#'   alt1.price = c(2, 4, 6, 8, 2, 4, 6, 8),
#'   alt1.qual  = c(0, 0, 1, 1, 1, 1, 0, 0),
#'   alt2.price = c(6, 2, 8, 4, 8, 6, 2, 4),
#'   alt2.qual  = c(1, 1, 0, 1, 0, 0, 1, 0)
#' )
#' dir <- file.path(tempdir(), "pc_example")
#' dir.create(dir, showWarnings = FALSE)
#' saveRDS(design, file.path(dir, "d.rds"))
#'
#' ul <- list(u1 = list(
#'   v1 = V.1 ~ bprice * alt1.price + bqual * alt1.qual,
#'   v2 = V.2 ~ bprice * alt2.price + bqual * alt2.qual
#' ))
#'
#' # nosim is tiny here so the example runs quickly; the percentages it reports
#' # are meaningless at that size. Use several hundred runs for a real curve.
#' power_curve(
#'   resps = c(50, 100),
#'   nosim = 4,
#'   designpath = dir,
#'   u = ul,
#'   bcoeff = list(bprice = -0.2, bqual = 0.3),
#'   mode = "sequential",
#'   verbose = 0
#' )
power_curve <- function(resps, ..., verbose = 1) {
  ## `resps` has to be tested for being missing before anything looks at it,
  ## otherwise building the message is itself the error.
  if (missing(resps)) {
    stop(
      "`resps` must be a vector of sample sizes to try, for example ",
      "c(100, 200, 400).",
      call. = FALSE
    )
  }
  if (!is.numeric(resps) || length(resps) == 0) {
    stop(
      "`resps` must be a vector of sample sizes to try, for example ",
      "c(100, 200, 400). Got ", describe_value(resps), ".",
      call. = FALSE
    )
  }
  resps <- vapply(resps, check_count, integer(1), arg = "resps")
  resps <- sort(unique(resps))
  verbose <- check_verbose(verbose)

  dots <- list(...)
  if ("estimate" %in% names(dots) && !isTRUE(dots$estimate)) {
    stop(
      "power_curve() needs estimated models, so `estimate` cannot be FALSE. ",
      "Use sim_all() directly if you only want simulated datasets.",
      call. = FALSE
    )
  }
  dots$estimate <- TRUE

  rows <- list()

  for (n in resps) {
    vmsg(verbose, 1, "Sample size ", n, " of ", max(resps), " ...")

    res <- do.call(sim_all, c(dots, list(resps = n, verbose = max(0, verbose - 1))))
    rows[[length(rows) + 1L]] <- power_curve_rows(res, n)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$design, out$parameter, out$resps), ]
}


#' Pull one sample size's results into tidy rows
#' @noRd
power_curve_rows <- function(res, n) {
  designs <- res[["arguments"]][["designname"]]
  sa <- res[["summaryall"]]

  rows <- list()

  for (d in designs) {
    by_par <- res[["powa_by_par"]][[d]]
    if (is.null(by_par)) next

    nsim <- res[["convergence"]][[d]][["converged"]] %||% NA_integer_
    mean_col <- paste0(d, ".mean")
    sd_col <- paste0(d, ".sd")

    for (p in names(by_par)) {
      hit <- sa$parname == p
      rows[[length(rows) + 1L]] <- data.frame(
        design = d,
        resps = n,
        parameter = p,
        power = unname(by_par[[p]]),
        se = power_se(unname(by_par[[p]]), nsim),
        truepar = if (any(hit)) sa$truepar[hit][1] else NA_real_,
        estimate = if (any(hit) && mean_col %in% names(sa)) sa[[mean_col]][hit][1] else NA_real_,
        sd = if (any(hit) && sd_col %in% names(sa)) sa[[sd_col]][hit][1] else NA_real_,
        converged = nsim,
        stringsAsFactors = FALSE
      )
    }

    ## joint power, the share of runs in which everything was significant at once
    joint <- res[["powa"]][[d]]
    if (!is.null(joint) && "TRUE" %in% names(joint)) {
      rows[[length(rows) + 1L]] <- data.frame(
        design = d, resps = n, parameter = "(all)",
        power = unname(joint[["TRUE"]]),
        se = power_se(unname(joint[["TRUE"]]), nsim),
        truepar = NA_real_, estimate = NA_real_, sd = NA_real_,
        converged = nsim,
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

#' Standard error of a share estimated from n runs, in percentage points
#' @noRd
power_se <- function(power, n) {
  if (is.na(n) || n <= 1) {
    return(NA_real_)
  }
  p <- power / 100
  100 * sqrt(p * (1 - p) / n)
}
