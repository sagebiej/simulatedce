#' Check whether a design can identify the model you mean to estimate
#'
#' @description
#' A choice model is identified by the variation *within* each choice situation.
#' If two attributes always move together across the alternatives on offer, no
#' sample size will separate their coefficients, and the estimates you get back
#' will be meaningless rather than merely imprecise. This is easy to do by
#' accident, and nothing downstream complains: the models converge, the summary
#' table fills in, and the numbers are wrong.
#'
#' `check_design()` builds the matrix of identifying variation and reports whether
#' it has full rank, which terms are collinear if not, and a few other things worth
#' knowing before a long run.
#'
#' @param design A design data frame, as returned by [readdesign()], or a path to a
#'   design file which will be read for you.
#' @param u Optionally the list of utility functions you intend to use, in the same
#'   form as for [sim_all()]. When given, the check uses the exact terms your model
#'   implies, including interactions, transformations and alternative-specific
#'   constants. When omitted, it falls back on treating every `alt<k>.<name>` column
#'   as one attribute shared across alternatives.
#' @param bcoeff Optionally the coefficients. Only used to look for choice
#'   situations in which one alternative dominates the others, which needs to know
#'   the signs.
#' @inheritParams readdesign
#'
#' @details
#' The identifying variation is computed by centring each term within each choice
#' situation. For two alternatives this is equivalent to the usual matrix of
#' attribute differences; centring generalises it to any number of alternatives.
#'
#' When `u` is supplied, the regressor belonging to each coefficient is obtained by
#' evaluating the utility functions with that coefficient set to 1 and the rest to
#' 0. That is exact for any utility that is linear in the coefficients, which is
#' what a logit requires, and it means transformations and interactions are handled
#' without special cases.
#'
#' @return An object of class `dce_design_check`, which prints as a report. As a
#'   list it contains `identified`, `rank`, `n_terms`, `aliased`, `terms`,
#'   `linear_in_coefficients`, `situations`, `blocks`, `sets_per_block`, `alternatives`,
#'   `distinct_patterns`, `max_correlation`, `correlations`, `no_variation`,
#'   `identical_alternatives`, `dominated`, and `problems`.
#'
#' @export
#'
#' @examples
#' # a design whose attributes cannot be told apart: price is exactly
#' # 2 * quality + 4 * origin in differences
#' bad <- data.frame(
#'   Choice.situation = 1:4,
#'   alt1.price = c(2, 4, 6, 8), alt1.quality = c(0, 1, 0, 1), alt1.origin = c(0, 0, 1, 1),
#'   alt2.price = c(8, 6, 4, 2), alt2.quality = c(1, 0, 1, 0), alt2.origin = c(1, 1, 0, 0)
#' )
#' check_design(bad)
#'
#' # the same attributes, paired so they vary independently
#' good <- data.frame(
#'   Choice.situation = 1:4,
#'   alt1.price = c(2, 4, 6, 8), alt1.quality = c(0, 1, 0, 1), alt1.origin = c(0, 0, 1, 1),
#'   alt2.price = c(4, 8, 2, 6), alt2.quality = c(1, 1, 0, 0), alt2.origin = c(1, 0, 1, 0)
#' )
#' check_design(good)
check_design <- function(design, u = NULL, bcoeff = NULL, designtype = NULL) {
  if (is.character(design)) {
    design <- readdesign(design, designtype = designtype, verbose = 0)
  }
  design <- check_design_frame(design)

  ## ---- structure -----------------------------------------------------------

  blocks <- if ("Block" %in% names(design)) {
    normalise_block(design$Block)
  } else {
    rep(1L, nrow(design))
  }
  block_sizes <- as.integer(table(blocks))

  ## ---- the model matrix, one slice per alternative --------------------------

  mm <- design_model_matrix(design, u, bcoeff)
  terms <- mm$terms
  n_alt <- mm$n_alt
  linear <- isTRUE(mm$linear)
  ## x is situations x alternatives x terms
  x <- mm$x

  ## ---- identifying variation ------------------------------------------------
  ##
  ## Centre each term within its choice situation. What survives is what the
  ## likelihood can see.

  centred <- x
  for (j in seq_along(terms)) {
    slice <- x[, , j, drop = FALSE]
    dim(slice) <- c(nrow(design), n_alt)
    centred[, , j] <- slice - rowMeans(slice)
  }

  ident <- matrix(centred, nrow = nrow(design) * n_alt, ncol = length(terms))
  colnames(ident) <- terms

  ## Terms with no identifying variation at all
  spread <- apply(ident, 2, function(v) max(abs(v)))
  no_variation <- terms[spread < .Machine$double.eps^0.5]

  qr_fit <- qr(ident)
  rank <- qr_fit$rank
  aliased <- if (rank < length(terms)) terms[qr_fit$pivot[(rank + 1L):length(terms)]] else character(0)

  ## Correlations, only over terms that vary
  varying <- setdiff(terms, no_variation)
  correlations <- if (length(varying) > 1) {
    stats::cor(ident[, varying, drop = FALSE])
  } else {
    matrix(numeric(0), 0, 0)
  }
  max_correlation <- if (length(varying) > 1) {
    max(abs(correlations[upper.tri(correlations)]))
  } else {
    NA_real_
  }

  ## ---- patterns, ties and dominance ----------------------------------------

  patterns <- matrix(centred, nrow = nrow(design), ncol = n_alt * length(terms))
  distinct_patterns <- nrow(unique(round(patterns, 10)))

  identical_alternatives <- sum(vapply(seq_len(nrow(design)), function(i) {
    slice <- matrix(x[i, , ], nrow = n_alt)
    anyDuplicated(round(slice, 10)) > 0
  }, logical(1)))

  dominated <- NA_integer_
  if (!is.null(bcoeff)) {
    dominated <- count_dominated(x, terms, bcoeff)
  }

  ## ---- plain-language problems ---------------------------------------------

  problems <- character(0)

  if (!linear) {
    problems <- c(problems, paste0(
      "This utility is not linear in its coefficients: at least one term depends ",
      "on the value of another coefficient, as a specification in ",
      "willingness-to-pay space does. Identification cannot be read off the design ",
      "the way it can for a linear-in-parameters model, so the rank reported above ",
      "says nothing about whether your model is estimable. It very well may be. ",
      "Check it by simulating a large sample and seeing whether the coefficients ",
      "come back."
    ))
  }

  if (linear && rank < length(terms)) {
    problems <- c(problems, paste0(
      "Not identified: the identifying variation has rank ", rank, " for ",
      length(terms), " term(s). ",
      if (length(aliased)) {
        paste0(
          and_list(paste0("`", aliased, "`")),
          " cannot be told apart from the others. No sample size will fix this; ",
          "the design has to change."
        )
      } else {
        ""
      }
    ))
  }
  if (linear && length(no_variation) > 0) {
    problems <- c(problems, paste0(
      "No variation within choice situations for ",
      and_list(paste0("`", no_variation, "`")),
      ". These term(s) are constant across the alternatives on offer, so they ",
      "cannot be estimated."
    ))
  }
  if (linear && !is.na(max_correlation) && max_correlation > 0.9) {
    worst <- which(abs(correlations) == max_correlation & upper.tri(correlations),
      arr.ind = TRUE
    )[1, ]
    problems <- c(problems, sprintf(
      paste0(
        "`%s` and `%s` are correlated at %.2f in the identifying variation. ",
        "They are technically separable but their coefficients will be unstable."
      ),
      varying[worst[1]], varying[worst[2]], max_correlation
    ))
  }
  if (identical_alternatives > 0) {
    problems <- c(problems, paste0(
      identical_alternatives, " choice situation(s) offer two identical ",
      "alternatives. Those choices are coin flips and carry no information."
    ))
  }
  ## A few dominated situations are normal and are reported as information.
  ## More than half of them means most choices involve no trade-off.
  if (!is.na(dominated) && dominated > nrow(design) / 2) {
    problems <- c(problems, paste0(
      "More than half the choice situations (", dominated, " of ", nrow(design),
      ") have an alternative that is at least as good as every other on all ",
      "terms, so most choices involve no trade-off."
    ))
  }
  if (length(unique(block_sizes)) > 1L) {
    problems <- c(problems, paste0(
      "The blocks have different sizes (",
      paste(block_sizes, collapse = ", "),
      "), so respondents would see different numbers of choice situations."
    ))
  }

  structure(
    list(
      identified = if (!linear) NA else rank == length(terms) && length(no_variation) == 0,
      linear_in_coefficients = linear,
      rank = rank,
      n_terms = length(terms),
      aliased = aliased,
      terms = terms,
      situations = nrow(design),
      blocks = length(unique(blocks)),
      sets_per_block = block_sizes,
      alternatives = n_alt,
      distinct_patterns = distinct_patterns,
      max_correlation = max_correlation,
      correlations = correlations,
      no_variation = no_variation,
      identical_alternatives = identical_alternatives,
      dominated = dominated,
      problems = problems,
      from_utility = !is.null(u)
    ),
    class = "dce_design_check"
  )
}


#' Build the per-alternative model matrix a design and utility imply
#'
#' Returns an array of dimension situations x alternatives x terms. With a utility
#' list, each term is one coefficient and its regressor is found by evaluating the
#' utility with that coefficient set to 1 and the others to 0, which is exact for
#' any utility linear in the coefficients. Without one, each `alt<k>.<name>` column
#' family is treated as one attribute.
#' @noRd
design_model_matrix <- function(design, u = NULL, bcoeff = NULL) {
  if (is.null(u)) {
    return(design_model_matrix_from_names(design))
  }

  check_utility_list(u, arg = "u")

  ## Only the first decision group is inspected: the others describe the same
  ## alternatives by construction.
  group <- u[[1]]
  rhs_vars <- unique(unlist(lapply(group, function(f) all.vars(formula.tools::rhs(f)))))
  not_in_design <- setdiff(rhs_vars, names(design))

  if (is.null(bcoeff)) {
    ## Without the coefficient list, anything absent from the design has to be
    ## taken for a coefficient. A mistyped column name then shows up as a term
    ## with no variation rather than as a missing column.
    coefs <- not_in_design
  } else {
    coefs <- intersect(rhs_vars, names(bcoeff))
    unknown <- setdiff(not_in_design, names(bcoeff))
    if (length(unknown) > 0) {
      stop(
        "`u` refers to ", and_list(paste0("`", unknown, "`")),
        ", which are not columns of the design and not in `bcoeff` either. ",
        "Check for a typo.",
        call. = FALSE
      )
    }
  }

  if (length(coefs) == 0) {
    stop(
      "None of the variables in `u` look like coefficients: every one of ",
      and_list(paste0("`", rhs_vars, "`")), " is a column of the design.",
      call. = FALSE
    )
  }

  funs <- compile_utility_list(u)[[1]]
  n_alt <- length(funs)

  ## The regressor belonging to coefficient j is the change in utility when j goes
  ## from 0 to 1 with the others held where they are. Taking the difference rather
  ## than the level means a constant in the utility that carries no coefficient
  ## does not leak into every regressor.
  regressors <- function(others) {
    out <- array(0,
      dim = c(nrow(design), n_alt, length(coefs)),
      dimnames = list(NULL, names(funs), coefs)
    )
    for (j in seq_along(coefs)) {
      env_on <- new.env(parent = globalenv())
      env_off <- new.env(parent = globalenv())
      for (i in seq_along(coefs)) {
        assign(coefs[i], others[i], envir = env_on)
        assign(coefs[i], others[i], envir = env_off)
      }
      assign(coefs[j], 1, envir = env_on)
      assign(coefs[j], 0, envir = env_off)

      for (k in seq_len(n_alt)) {
        on <- rep(as.numeric(funs[[k]](design, env_on)), length.out = nrow(design))
        off <- rep(as.numeric(funs[[k]](design, env_off)), length.out = nrow(design))
        out[, k, j] <- on - off
      }
    }
    out
  }

  x <- regressors(rep(0, length(coefs)))

  ## A logit is linear in its coefficients, and the whole idea of reading a term's
  ## regressor off the utility only works if it is. When the utility multiplies two
  ## coefficients together, as a specification in willingness-to-pay space does,
  ## each regressor depends on the other coefficients and none of this applies.
  ## Detect that by asking whether the regressors move when the other coefficients
  ## do.
  probe <- regressors(seq_along(coefs) / length(coefs) + 0.5)
  linear <- isTRUE(all.equal(as.vector(x), as.vector(probe), tolerance = 1e-8))

  list(x = x, terms = coefs, n_alt = n_alt, linear = linear)
}

#' Fall back on the alt<k>.<name> convention when no utility is given
#' @noRd
design_model_matrix_from_names <- function(design) {
  hits <- grep("^alt[0-9]+[._]", names(design), value = TRUE)
  if (length(hits) == 0) {
    stop(
      "Could not work out the alternatives from the column names. Expected ",
      "columns like `alt1.price` and `alt2.price`. Either rename them, or pass ",
      "the utility functions as `u` so the terms can be read from those instead.",
      call. = FALSE
    )
  }

  alt_num <- as.integer(sub("^alt([0-9]+)[._].*$", "\\1", hits))
  attribute <- sub("^alt[0-9]+[._]", "", hits)

  alts <- sort(unique(alt_num))
  attrs <- unique(attribute)

  x <- array(0, dim = c(nrow(design), length(alts), length(attrs)),
             dimnames = list(NULL, paste0("alt", alts), attrs))

  for (j in seq_along(attrs)) {
    for (k in seq_along(alts)) {
      col <- hits[alt_num == alts[k] & attribute == attrs[j]]
      if (length(col) == 1) x[, k, j] <- as.numeric(design[[col]])
      ## a term absent from an alternative stays 0, which is what a utility
      ## function omitting it would imply
    }
  }

  list(x = x, terms = attrs, n_alt = length(alts), linear = TRUE)
}

#' Count choice situations in which one alternative weakly dominates the rest
#' @noRd
count_dominated <- function(x, terms, bcoeff) {
  known <- intersect(terms, names(bcoeff))
  if (length(known) == 0) {
    return(NA_integer_)
  }

  signs <- vapply(known, function(nm) sign(spec_mean(bcoeff[[nm]], nm)), numeric(1))
  keep <- known[signs != 0]
  if (length(keep) == 0) {
    return(NA_integer_)
  }

  idx <- match(keep, terms)
  n_alt <- dim(x)[2]

  sum(vapply(seq_len(dim(x)[1]), function(i) {
    ## orient every term so that more is better
    slice <- matrix(x[i, , idx], nrow = n_alt) *
      matrix(signs[keep], nrow = n_alt, ncol = length(idx), byrow = TRUE)
    any(vapply(seq_len(n_alt), function(k) {
      others <- slice[-k, , drop = FALSE]
      all(apply(others, 1, function(o) all(slice[k, ] >= o)))
    }, logical(1)))
  }, logical(1)))
}


#' @export
print.dce_design_check <- function(x, ...) {
  cat("Design check\n")
  cat(strrep("-", 60), "\n")
  cat(sprintf(
    "  %d choice situation(s), %d alternative(s), %d block(s)%s\n",
    x$situations, x$alternatives, x$blocks,
    if (x$blocks > 1) sprintf(" of %s sets", and_list(unique(x$sets_per_block))) else ""
  ))
  cat(sprintf(
    "  %d term(s) from %s: %s\n",
    x$n_terms,
    if (x$from_utility) "your utility functions" else "the column names",
    paste(x$terms, collapse = ", ")
  ))
  if (isFALSE(x$linear_in_coefficients)) {
    cat("  utility is NOT linear in the coefficients: the rank below does not apply\n")
  }
  cat(sprintf(
    "  identifying variation: rank %d of %d, %d distinct pattern(s)\n",
    x$rank, x$n_terms, x$distinct_patterns
  ))
  if (!is.na(x$max_correlation)) {
    cat(sprintf("  largest correlation between terms: %.2f\n", x$max_correlation))
  }
  if (!is.na(x$dominated)) {
    cat(sprintf(
      "  %d situation(s) with a dominant alternative, %d with identical ones\n",
      x$dominated, x$identical_alternatives
    ))
  } else if (x$identical_alternatives > 0) {
    cat(sprintf("  %d situation(s) offer identical alternatives\n", x$identical_alternatives))
  }

  cat(strrep("-", 60), "\n")
  if (length(x$problems) == 0) {
    cat("  Looks fine: every term is identified.\n")
  } else if (is.na(x$identified)) {
    for (p in x$problems) {
      cat(paste0("  ? ", paste(strwrap(p, width = 72, exdent = 4), collapse = "\n  ")), "\n")
    }
  } else {
    for (p in x$problems) {
      cat(paste0("  ! ", paste(strwrap(p, width = 72, exdent = 4), collapse = "\n  ")), "\n")
    }
  }
  invisible(x)
}
