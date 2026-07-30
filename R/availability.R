#' Availability of alternatives
#'
#' @description
#' Not every alternative is on offer in every choice situation. A design might
#' withhold one option in some tasks, or offer a no-choice only sometimes.
#'
#' Mark this in the design with one column per alternative named `av1`, `av2`, and
#' so on, holding 1 where the alternative is offered and 0 where it is not.
#' `av.1`, `av_1` and `AV1` are recognised too, since the readers rewrite
#' separators. When no such columns exist, every alternative is available
#' everywhere, which is what earlier versions always assumed.
#'
#' Availability is respected in both halves of the package. When simulating, an
#' unavailable alternative cannot be chosen however high its utility comes out.
#' When estimating, the availability matrix is passed to the model rather than a
#' matrix of ones, so the choice probabilities are normalised over the alternatives
#' that were actually offered.
#'
#' @section A no-choice alternative:
#' A no-choice, opt-out or status-quo option is just an alternative whose utility
#' has no attributes, only a constant:
#'
#' ```
#' u1 = list(
#'   v1 = V.1 ~ bprice * alt1.price + bqual * alt1.qual,
#'   v2 = V.2 ~ bprice * alt2.price + bqual * alt2.qual,
#'   v3 = V.3 ~ bnone                      # the opt-out
#' )
#' ```
#'
#' `bnone` is then the utility of choosing nothing, relative to the normalisation
#' that the other alternatives carry no constant. Add an `av3` column if the
#' opt-out is not always offered.
#'
#' @name availability
NULL


#' Find the availability columns in a dataset
#'
#' Returns them ordered by alternative number, or NULL when there are none.
#' @noRd
availability_columns <- function(data) {
  hits <- grep("^av[._]?[0-9]+$", names(data), value = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) {
    return(NULL)
  }
  hits[order(as.integer(sub("^av[._]?", "", hits, ignore.case = TRUE)))]
}

#' Build the availability matrix a dataset implies
#'
#' @param data The simulated dataset.
#' @param n_alt How many alternatives the utility functions describe.
#' @return A numeric matrix with one row per observation and one column per
#'   alternative, or NULL when the design says nothing about availability.
#' @noRd
availability_matrix <- function(data, n_alt) {
  cols <- availability_columns(data)
  if (is.null(cols)) {
    return(NULL)
  }

  numbers <- as.integer(sub("^av[._]?", "", cols, ignore.case = TRUE))

  if (!setequal(numbers, seq_len(n_alt))) {
    stop(
      "The availability columns do not match the alternatives. The utility ",
      "functions describe ", n_alt, " alternative(s), so ",
      and_list(paste0("`av", seq_len(n_alt), "`")),
      " were expected, but the design has ",
      and_list(paste0("`", cols, "`")),
      ". Give one availability column per alternative, or none at all.",
      call. = FALSE
    )
  }

  av <- vapply(cols, function(cl) {
    v <- data[[cl]]
    if (is.logical(v)) v <- as.integer(v)
    if (is.factor(v)) v <- suppressWarnings(as.numeric(as.character(v)))
    if (!is.numeric(v) || anyNA(v) || !all(v %in% c(0, 1))) {
      stop(
        "Availability column `", cl, "` must be 0 or 1 (or FALSE and TRUE) for ",
        "every row. Found ", describe_value(unique(v)), ".",
        call. = FALSE
      )
    }
    as.numeric(v)
  }, numeric(nrow(data)))

  av <- matrix(av, nrow = nrow(data), ncol = n_alt)

  none <- rowSums(av) == 0
  if (any(none)) {
    stop(
      sum(none), " choice situation(s) have no available alternative at all, ",
      "the first at row ", which(none)[1],
      ". A respondent must be offered something to choose.",
      call. = FALSE
    )
  }

  only_one <- rowSums(av) == 1
  if (all(only_one)) {
    stop(
      "Every choice situation offers exactly one alternative, so there is nothing ",
      "to choose. Check the availability columns.",
      call. = FALSE
    )
  }

  av
}

#' The availability matrix to hand to the estimator
#'
#' Falls back on all-available, which is what mixl's own helper produces.
#' @noRd
design_availabilities <- function(data, n_alt) {
  av <- availability_matrix(data, n_alt)
  if (is.null(av)) {
    return(matrix(1, nrow(data), n_alt))
  }
  av
}
