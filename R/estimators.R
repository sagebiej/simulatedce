#' Writing your own estimator
#'
#' @description
#' By default the package estimates with `mixl`, which is fast and handles both
#' multinomial and mixed logit. It is not the only thing you might want to fit. The
#' `estimator` argument of [sim_choice()] and [sim_all()] therefore accepts a
#' function of your own, so you can put any model in the loop while keeping the
#' simulation, the aggregation and the power calculations as they are.
#'
#' @section The contract:
#' An estimator is a function of two arguments, `data` and `spec`, returning a list
#' with four elements:
#'
#' \describe{
#'   \item{`coefficients`}{A named numeric vector of estimates.}
#'   \item{`pvalues`}{A named numeric vector of two-sided p values, with the same
#'     names as `coefficients`. Use `NA` where you have none; power will then be
#'     reported as if the coefficient were not significant.}
#'   \item{`se`}{Optional. A named numeric vector of standard errors. Supply it and
#'     you get coverage, the share of runs whose 95% interval contains the true
#'     value, alongside bias and root mean squared error. Leave it out and coverage
#'     is reported as `NA`.}
#'   \item{`converged`}{A single `TRUE` or `FALSE`. A run that reports `FALSE`, or
#'     that returns any non-finite estimate, is left out of the summaries and
#'     counted in `$convergence`.}
#'   \item{`model`}{Anything you like. It is returned untouched in the results when
#'     `keep_models = TRUE`, and discarded otherwise. Put the fitted object here.}
#' }
#'
#' `data` is one simulated dataset: a data frame with `ID`, `task`, `CHOICE`, the
#' design columns, and the coefficient draws when the coefficients are random.
#'
#' `spec` describes the model to fit:
#'
#' \describe{
#'   \item{`utility`}{The utility functions for the first decision group, as a list
#'     of formulas.}
#'   \item{`bcoeff`}{The coefficient specifications, so you can see which are random
#'     and how.}
#'   \item{`coefficient_names`}{The names to estimate, in order.}
#'   \item{`model`}{`"mnl"` or `"mixed"`.}
#'   \item{`n_draws`}{Draws per respondent for a mixed model.}
#'   \item{`n_alt`}{How many alternatives.}
#'   \item{`availabilities`}{A matrix with one row per observation and one column per
#'     alternative, 1 where the alternative was offered. See [availability].}
#'   \item{`script`}{The utility script in mixl's notation, in case that is a useful
#'     starting point.}
#'   \item{`model_matrix`}{A function of no arguments. Call it to get a list with
#'     `x`, an array of dimension observations by alternatives by coefficients
#'     holding each coefficient's regressor, and `terms`, their names. This is
#'     usually all a linear-in-parameters model needs, and it handles interactions
#'     and transformations for you.}
#' }
#'
#' @section A worked example:
#' A conditional logit written from scratch, which doubles as a check on the
#' default backend:
#'
#' ```r
#' plain_mnl <- function(data, spec) {
#'   mm <- spec$model_matrix()
#'   x <- mm$x                      # obs x alternatives x coefficients
#'   av <- spec$availabilities
#'   chosen <- cbind(seq_len(nrow(data)), data$CHOICE)
#'
#'   negll <- function(b) {
#'     v <- apply(x, c(1, 2), function(r) sum(r * b))
#'     v[av == 0] <- -Inf
#'     -sum(v[chosen] - log(rowSums(exp(v))))
#'   }
#'
#'   fit <- stats::optim(rep(0, length(mm$terms)), negll,
#'                       method = "BFGS", hessian = TRUE)
#'   se <- sqrt(diag(solve(fit$hessian)))
#'
#'   list(
#'     coefficients = stats::setNames(fit$par, mm$terms),
#'     pvalues = stats::setNames(2 * stats::pnorm(-abs(fit$par / se)), mm$terms),
#'     se = stats::setNames(se, mm$terms),
#'     converged = fit$convergence == 0,
#'     model = fit
#'   )
#' }
#'
#' sim_all(nosim = 20, resps = 200, designpath = dir, u = ul,
#'         bcoeff = bcoeff, estimator = plain_mnl, mode = "sequential")
#' ```
#'
#' @section Parallel runs:
#' In parallel mode your function is sent to worker processes, so it has to be
#' self-contained: refer to packages with `::`, and do not rely on objects in your
#' global environment unless they travel with the closure. Anything holding an
#' external pointer, as a compiled model does, will not survive the trip and has to
#' be rebuilt inside the function.
#'
#' @name estimators
#' @seealso [sim_all()], [availability]
NULL


#' Turn the estimator argument into a function
#' @noRd
resolve_estimator <- function(estimator) {
  if (is.function(estimator)) {
    if (length(formals(estimator)) < 2L) {
      stop(
        "An `estimator` function must take two arguments, the simulated `data` ",
        "and the model `spec`. Yours takes ", length(formals(estimator)), ". ",
        "See ?estimators.",
        call. = FALSE
      )
    }
    return(estimator)
  }

  if (!is.character(estimator) || length(estimator) != 1L) {
    stop(
      "`estimator` must be \"mixl\" or a function. Got ", describe_value(estimator),
      ". See ?estimators for how to write one.",
      call. = FALSE
    )
  }

  builtin <- c("mixl")
  if (!estimator %in% builtin) {
    stop(
      "Unknown estimator '", estimator, "'. Built in: ", and_list(builtin, "or"),
      ". Pass a function to use your own model; see ?estimators.",
      did_you_mean(estimator, builtin),
      call. = FALSE
    )
  }

  estimator_mixl
}


#' The built-in mixl backend
#'
#' Fits the multinomial logit, and then, for a mixed model, warm-starts the mixed
#' logit from it.
#' @noRd
estimator_mixl <- function(data, spec) {
  bits <- model_spec_for(spec$script, data)
  bits$availabilities <- spec$availabilities
  mnl_fit <- quiet_estimate(bits, data, spec$verbose)

  fit <- mnl_fit
  if (identical(spec$model, "mixed")) {
    mix <- model_spec_for(spec$mixed_script, data)
    mix$availabilities <- spec$availabilities
    mix$start <- mixed_start_values(
      stats::coef(mnl_fit), mix$spec$beta_names, spec$bcoeff, spec$random_params
    )
    fit <- quiet_estimate(mix, data, spec$verbose, n_draws = spec$n_draws)
  }

  table <- tryCatch(summary(fit)[["coefTable"]], error = function(e) NULL)
  if (is.null(table) || nrow(table) == 0) {
    return(list(
      coefficients = numeric(0), pvalues = numeric(0), se = numeric(0),
      converged = FALSE, model = fit
    ))
  }

  list(
    coefficients = stats::setNames(table[["est"]], rownames(table)),
    pvalues = stats::setNames(table[["rob_pval0"]], rownames(table)),
    ## the robust standard error, to match the robust p value above
    se = stats::setNames(table[["robse"]], rownames(table)),
    converged = isTRUE(as.integer(fit[["code"]]) == 0L),
    model = fit
  )
}


#' Assemble the spec handed to an estimator
#' @noRd
build_estimator_spec <- function(u, bcoeff, script, mixed_script, random_params,
                                 model, n_draws, n_alt, verbose) {
  force(u)
  force(bcoeff)

  list(
    utility = u[[1]],
    bcoeff = bcoeff,
    coefficient_names = names(bcoeff),
    model = model,
    n_draws = n_draws,
    n_alt = n_alt,
    script = script,
    mixed_script = mixed_script,
    random_params = random_params,
    verbose = verbose,
    ## availabilities depend on the data, so they are filled in per run
    availabilities = NULL,
    model_matrix = NULL
  )
}

#' Complete the spec for one simulated dataset
#' @noRd
spec_for_data <- function(spec, data) {
  spec$availabilities <- design_availabilities(data, spec$n_alt)
  ## Built on demand: the default backend never needs it, and building it costs a
  ## pass over the data for every coefficient.
  aligned <- align_utility_to_data(spec$utility, data)
  spec$model_matrix <- function() design_model_matrix(data, list(u1 = aligned), spec$bcoeff)
  spec
}


#' Point the utility formulas at the column names the simulated data actually has
#'
#' `simulate_choices()` turns every dot in a column name into an underscore, because
#' that is what the estimation needs, so `alt1.price` in a utility function is
#' `alt1_price` by the time a model is fitted. The rename is done on the syntax tree
#' rather than on the deparsed text, so a number like 0.5 is left alone.
#' @noRd
align_utility_to_data <- function(utility, data) {
  present <- names(data)

  rename <- function(expr) {
    if (is.name(expr)) {
      nm <- as.character(expr)
      if (nm %in% present) {
        return(expr)
      }
      underscored <- gsub(".", "_", nm, fixed = TRUE)
      if (underscored %in% present) {
        return(as.name(underscored))
      }
      return(expr)
    }
    if (is.call(expr)) {
      for (i in seq_along(expr)[-1]) expr[[i]] <- rename(expr[[i]])
    }
    expr
  }

  lapply(utility, function(fm) {
    lhs <- as.character(formula.tools::lhs(fm))
    rhs <- rename(formula.tools::rhs(fm))
    stats::as.formula(
      paste(gsub(".", "_", lhs, fixed = TRUE), "~", paste(deparse(rhs), collapse = " ")),
      env = baseenv()
    )
  })
}


#' Check what an estimator handed back, and say precisely what is wrong
#' @noRd
check_estimator_result <- function(res, run) {
  where <- paste0(" (run ", run, ")")

  if (!is.list(res)) {
    stop(
      "The estimator must return a list, not ", describe_value(res), where,
      ". See ?estimators for the four elements it needs.",
      call. = FALSE
    )
  }

  missing_bits <- setdiff(c("coefficients", "pvalues", "converged"), names(res))
  if (length(missing_bits) > 0) {
    stop(
      "The estimator's result is missing ", and_list(paste0("`", missing_bits, "`")),
      where, ". See ?estimators.",
      call. = FALSE
    )
  }

  if (!is.numeric(res$coefficients) || is.null(names(res$coefficients))) {
    stop(
      "`coefficients` from the estimator must be a named numeric vector, not ",
      describe_value(res$coefficients), where, ".",
      call. = FALSE
    )
  }
  if (!is.numeric(res$pvalues) || is.null(names(res$pvalues))) {
    stop(
      "`pvalues` from the estimator must be a named numeric vector, not ",
      describe_value(res$pvalues), where, ".",
      call. = FALSE
    )
  }
  if (!setequal(names(res$coefficients), names(res$pvalues))) {
    stop(
      "`coefficients` and `pvalues` from the estimator must have the same names",
      where, ". Coefficients: ", and_list(paste0("`", names(res$coefficients), "`")),
      "; p values: ", and_list(paste0("`", names(res$pvalues), "`")), ".",
      call. = FALSE
    )
  }
  if (!is.null(res$se)) {
    if (!is.numeric(res$se) || is.null(names(res$se))) {
      stop(
        "`se` from the estimator must be a named numeric vector, or absent, not ",
        describe_value(res$se), where, ".",
        call. = FALSE
      )
    }
    if (!all(names(res$coefficients) %in% names(res$se))) {
      stop(
        "`se` from the estimator is missing ",
        and_list(paste0("`", setdiff(names(res$coefficients), names(res$se)), "`")),
        where, ". Give one standard error per coefficient, or leave `se` out.",
        call. = FALSE
      )
    }
  }

  if (!is.logical(res$converged) || length(res$converged) != 1L) {
    stop(
      "`converged` from the estimator must be a single TRUE or FALSE, not ",
      describe_value(res$converged), where, ".",
      call. = FALSE
    )
  }

  res
}
