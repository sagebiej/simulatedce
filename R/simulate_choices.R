#' Simulate choices based on a data.frame with a design and respondents
#'
#' @param data a dataframe that includes a design repeated for the number of
#'   observations, as returned by [createDataset()]. It must have an `ID` column.
#' @param utility a list with the utility functions, one utility function for each
#'   alternative. See [sim_choice()] for the conventions.
#' @param setspp  Deprecated. Ignored and retained for backward compatibility.
#'   The number of choice sets per respondent is taken from `data`.
#' @inheritParams make_rand_params
#' @param decisiongroups A vector of cumulative shares showing how respondents are
#'   split between the utility functions in `utility`. It must start at 0 and end
#'   at 1. Whole respondents are assigned to a group, never part of a respondent.
#' @param manipulations A named list of expressions altering columns before the
#'   utilities are computed, for example applying a factor to an attribute or
#'   changing a term for one group only.
#' @param preprocess_function A function that reads in external data (e.g. GIS
#'   coordinates) to be merged with the simulated dataset. It must return either
#'   `NULL` or a data.frame with a column called `ID` used for matching.
#' @param keep_utilities Keep the `V_`, `e_` and `U_` columns in the result.
#'   `TRUE` by default. Setting it to `FALSE` roughly halves the size of the
#'   simulated data, which matters when thousands of runs are held in memory. The
#'   estimation does not need these columns.
#' @param verbose Integer controlling how much progress information is printed
#'   to the console (via \code{message()}). Levels are cumulative: each level
#'   also prints everything emitted by the levels below it.
#'   \describe{
#'     \item{\code{0}}{Silent. No progress messages are emitted.}
#'     \item{\code{1}}{Key results only: the parameter specification summary and
#'       total run time, plus the final summary and power tables when models are
#'       estimated.}
#'     \item{\code{2}}{Adds informational progress: the true and transformed
#'       utility functions, per-chunk progress, notification that a preprocess
#'       function ran, and the paths where results are saved.}
#'     \item{\code{3}}{Adds low-level debugging detail: step-by-step timings and
#'       internal state checks (e.g. dataset creation and decision-group setup).}
#'   }
#'   Defaults to \code{1}.
#' @return a data.frame that includes simulated choices and a design
#' @export
#' @import data.table
#' @examples
#' example_df <- data.frame(
#'   ID = rep(1:100, each = 4),
#'   price = rep(c(10, 10, 20, 20), 100),
#'   quality = rep(c(1, 2, 1, 2), 100)
#' )
#'
#' beta <- list(
#'   bprice   = -0.2,
#'   bquality =  0.8
#' )
#'
#' ut <- list(
#'   u1 = list(
#'     v1 = V.1 ~ bprice * price + bquality * quality,
#'     v2 = V.2 ~ 0
#'   )
#' )
#' simulate_choices(example_df, ut, bcoeff = beta)
#'
simulate_choices <- function(data, utility, setspp, bcoeff, decisiongroups = c(0, 1),
                             manipulations = list(), preprocess_function = NULL,
                             keep_utilities = TRUE, verbose = 1) {
  if (!missing(setspp)) {
    warning(
      "`setspp` is deprecated and ignored. ",
      "The number of choice sets per respondent is inferred from `data`.",
      call. = FALSE
    )
  }

  lap <- new_timer()

  data <- check_sim_data(data)
  check_bcoeff_list(bcoeff)
  check_utility_list(utility)
  check_decisiongroups(decisiongroups, utility)

  prepro_data <- run_preprocess(preprocess_function, verbose)

  ## ---- coefficients ---------------------------------------------------------
  ## Fixed coefficients are put in an environment that the compiled utility
  ## functions are handed explicitly. As soon as one coefficient is random they
  ## all become columns instead, so that a respondent's own draw enters their own
  ## utility. The utility formulas can still reach anything visible from the
  ## global environment, so user-defined helper functions keep working.
  coef_env <- new.env(parent = globalenv())

  if (has_random_params(bcoeff)) {
    clash <- intersect(names(bcoeff), names(data))
    if (length(clash) > 0) {
      stop(
        "Coefficient name(s) ", and_list(paste0("`", clash, "`")),
        " also name column(s) in the design. With random parameters each ",
        "coefficient becomes a column of respondent-level draws, so the names ",
        "would collide. Rename either the coefficient or the design column.",
        call. = FALSE
      )
    }
    respondent_ids <- unique(data$ID)
    rand_params <- make_rand_params(bcoeff,
      n_resp = length(respondent_ids),
      respondent_ids = respondent_ids
    )
    data <- dplyr::left_join(data, rand_params, by = "ID")
  } else {
    for (key in names(bcoeff)) {
      assign(key, bcoeff[[key]], envir = coef_env)
    }
  }
  vmsg(verbose, 3, lap("assign keys for bcoeff"))

  if (!is.null(prepro_data)) {
    data <- dplyr::left_join(data, prepro_data, by = "ID")
  }
  vmsg(verbose, 3, "preprocessed data merged: ", !is.null(prepro_data))

  ## ---- decision groups ------------------------------------------------------
  data$group <- assign_decision_groups(data$ID, decisiongroups)
  vmsg_print(verbose, 3, "\nRespondents per decision group:", table(data$group))

  missing_groups <- setdiff(unique(data$group), seq_along(utility))
  if (length(missing_groups) > 0) {
    stop(
      "The data contains decision group(s) ", and_list(missing_groups),
      " but `utility` only has ", length(utility), " group(s). ",
      "This is an internal inconsistency; please report it.",
      call. = FALSE
    )
  }

  ## ---- user manipulations ---------------------------------------------------
  if (length(manipulations) > 0) {
    data <- data %>%
      dplyr::group_by(.data$ID) %>%
      dplyr::mutate(!!!manipulations) %>%
      dplyr::ungroup()
    vmsg(verbose, 3, lap("user entered manipulations"))
  }

  ## ---- deterministic utility ------------------------------------------------
  ufuns <- compiled_utility(utility)
  varn <- names(ufuns[[1]])

  dt <- data.table::as.data.table(data)

  all_vars <- unique(unlist(lapply(utility, function(fl) {
    unlist(lapply(fl, function(fm) all.vars(formula.tools::rhs(fm))))
  })))
  sdcols <- intersect(all_vars, names(dt))

  ## Index by the group value rather than data.table's group counter, so the
  ## right utility is used no matter what order the groups appear in.
  dt[
    ,
    (varn) := lapply(ufuns[[.BY[["group"]]]], function(f) f(.SD, coef_env)),
    by = "group",
    .SDcols = sdcols
  ]
  vmsg(verbose, 3, lap("for each group calculate utility"))

  ## ---- random component and choice -----------------------------------------
  ## Dots become underscores because that is what mixl needs downstream. This
  ## also turns the utility columns V.1, V.2, ... into V_1, V_2, ...
  data <- dt %>%
    dplyr::rename_with(~ stringr::str_replace_all(., pattern = "\\.", "_"), tidyr::everything()) %>%
    as.data.frame()

  n_alt <- length(varn)
  vcols <- paste0("V_", seq_len(n_alt))
  ecols <- paste0("e_", seq_len(n_alt))
  ucols <- paste0("U_", seq_len(n_alt))

  missing_v <- setdiff(vcols, names(data))
  if (length(missing_v) > 0) {
    stop(
      "Expected the utility function(s) to produce column(s) ",
      and_list(paste0("`", missing_v, "`")),
      " but got ", and_list(paste0("`", varn, "`")),
      ". Name the left hand sides V.1, V.2, ... in order.",
      call. = FALSE
    )
  }

  nr <- nrow(data)
  ## Standard Gumbel via the inverse cdf. This is exactly what
  ## evd::rgumbel(n, 0, 1) does, so old seeds still reproduce.
  for (k in seq_len(n_alt)) data[[ecols[k]]] <- -log(stats::rexp(nr))
  for (k in seq_len(n_alt)) data[[ucols[k]]] <- data[[vcols[k]]] + data[[ecols[k]]]

  ## An alternative that was not offered cannot be chosen, however high its
  ## utility came out. See ?availability.
  total <- as.matrix(data[, ucols, drop = FALSE])
  av <- availability_matrix(data, n_alt)
  if (!is.null(av)) {
    total[av == 0] <- -Inf
    vmsg(verbose, 3, "Availability columns in use: ",
         and_list(availability_columns(data)))
  }

  data$CHOICE <- max.col(total)

  if (!isTRUE(keep_utilities)) {
    data <- data[, setdiff(names(data), c(vcols, ecols, ucols)), drop = FALSE]
  }

  vmsg(verbose, 3, lap("add random component"))
  vmsg_print(verbose, 3, "\nFirst few observations of the dataset:", utils::head(data))

  data
}


#' A stopwatch that reports the time since the previous lap
#'
#' Replaces the tic/toc pairs that used to live here. Those pushed onto a stack
#' in another package and, because the matching toc() sat inside a lazily
#' evaluated message argument, they were never popped below verbose level 3.
#' @noRd
new_timer <- function() {
  last <- proc.time()[["elapsed"]]
  function(label) {
    now <- proc.time()[["elapsed"]]
    out <- sprintf("%s: %.3f sec elapsed", label, now - last)
    last <<- now
    out
  }
}


#' Assign whole respondents to decision groups
#'
#' Cuts on respondents, not on rows. Cutting on rows split a respondent across
#' two decision rules whenever a boundary fell inside their block of choice sets.
#' @noRd
assign_decision_groups <- function(ids, decisiongroups) {
  if (length(decisiongroups) <= 2) {
    return(rep(1L, length(ids)))
  }

  respondent_ids <- unique(ids)
  n_resp <- length(respondent_ids)
  labels <- seq_len(length(decisiongroups) - 1L)

  group_of_resp <- as.integer(cut(
    seq_len(n_resp),
    breaks = decisiongroups * n_resp,
    labels = labels,
    include.lowest = TRUE
  ))

  group_of_resp[match(ids, respondent_ids)]
}


#' Compile the utility functions, reusing an earlier compilation if present
#'
#' `sim_choice()` compiles once and attaches the result to the utility list, so
#' thousands of simulation runs do not each pay for `compiler::cmpfun()`.
#' @noRd
compiled_utility <- function(utility) {
  cached <- attr(utility, "compiled")
  if (!is.null(cached)) {
    return(cached)
  }
  compile_utility_list(utility)
}

#' Turn each utility formula into a compiled function of the data
#'
#' The coefficient environment is passed in rather than captured, so the same
#' compiled function can be reused across simulation runs and shipped to parallel
#' workers. Variables are looked up in the data first, then in that environment,
#' then along the global search path.
#' @noRd
compile_utility_list <- function(utility) {
  compile_one <- function(fm) {
    name <- as.character(formula.tools::lhs(fm))
    rhs <- formula.tools::rhs(fm)
    fn <- function(d, env) eval(rhs, envir = d, enclos = env)
    list(name = name, fun = compiler::cmpfun(fn))
  }

  lapply(utility, function(fl) {
    tmp <- lapply(fl, compile_one)
    stats::setNames(
      lapply(tmp, `[[`, "fun"),
      vapply(tmp, `[[`, "", "name")
    )
  })
}

#' Attach a compilation to a utility list so it is only paid for once
#' @noRd
precompile_utility <- function(utility) {
  attr(utility, "compiled") <- compile_utility_list(utility)
  utility
}


#' Run and validate a user-supplied preprocess function
#' @noRd
run_preprocess <- function(preprocess_function, verbose) {
  if (is.null(preprocess_function)) {
    return(NULL)
  }
  if (!is.function(preprocess_function)) {
    stop(
      "`preprocess_function` must be a function taking no arguments and ",
      "returning a data.frame with an `ID` column, not ",
      describe_value(preprocess_function), ".",
      call. = FALSE
    )
  }

  prepro_data <- preprocess_function()

  if (is.null(prepro_data)) {
    vmsg(verbose, 2, "Preprocess function returned NULL; nothing to merge.")
    return(NULL)
  }
  if (!is.data.frame(prepro_data)) {
    stop(
      "`preprocess_function` returned ", describe_value(prepro_data),
      ". It must return a data.frame with an `ID` column, or NULL.",
      call. = FALSE
    )
  }
  if (!"ID" %in% names(prepro_data)) {
    stop(
      "The data.frame returned by `preprocess_function` has no `ID` column, so ",
      "it cannot be matched to respondents. Columns found: ",
      and_list(paste0("`", names(prepro_data), "`")), ".",
      call. = FALSE
    )
  }

  vmsg(verbose, 2, "Preprocess function has been executed.")
  prepro_data
}


#' Check the simulated dataset before anything is computed from it
#' @noRd
check_sim_data <- function(data) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data.frame holding the design repeated over respondents, ",
      "not ", describe_value(data), ". Build it with createDataset().",
      call. = FALSE
    )
  }
  if (nrow(data) == 0) {
    stop("`data` has no rows, so there is nothing to simulate.", call. = FALSE)
  }
  if (!"ID" %in% names(data)) {
    stop(
      "`data` needs an `ID` column identifying respondents. Columns found: ",
      and_list(paste0("`", names(data), "`")),
      ". createDataset() adds this column for you.",
      call. = FALSE
    )
  }

  reserved <- grep(utility_col_pattern(), names(data), value = TRUE)
  if (length(reserved) > 0) {
    stop(
      "The design contains column(s) ", and_list(paste0("`", reserved, "`")),
      ", but names of the form V_1, e_1, U_1 are reserved for the simulated ",
      "utilities and would be overwritten. Please rename them.",
      call. = FALSE
    )
  }
  if ("CHOICE" %in% names(data)) {
    stop(
      "The design already contains a `CHOICE` column, which is where the ",
      "simulated choice goes. Please rename it.",
      call. = FALSE
    )
  }

  as.data.frame(data)
}


#' Check the list of utility functions
#' @noRd
check_utility_list <- function(utility, arg = "utility") {
  if (!is.list(utility) || length(utility) == 0 || !all(vapply(utility, is.list, logical(1)))) {
    stop(
      "`", arg, "` must be a list of lists: one inner list of utility functions ",
      "per decision group, even when there is only one group. Got ",
      describe_value(utility), ".",
      call. = FALSE
    )
  }

  sizes <- vapply(utility, length, integer(1))
  if (length(unique(sizes)) > 1L) {
    stop(
      "Every decision group must describe the same alternatives, but the groups ",
      "have ", and_list(paste0(names(utility) %||% seq_along(utility), ": ", sizes)),
      " utility functions.",
      call. = FALSE
    )
  }

  for (i in seq_along(utility)) {
    not_formula <- !vapply(utility[[i]], inherits, logical(1), "formula")
    if (any(not_formula)) {
      stop(
        "Element(s) ", and_list(which(not_formula)), " of decision group ",
        names(utility)[i] %||% i,
        " are not formulas. Each utility must be written as V.1 ~ ... .",
        call. = FALSE
      )
    }
  }

  lhs_names <- lapply(utility, function(fl) {
    vapply(fl, function(fm) as.character(formula.tools::lhs(fm)), character(1))
  })
  if (length(unique(lhs_names)) > 1L) {
    stop(
      "The utility functions must have the same left hand sides in every ",
      "decision group. Found ",
      and_list(vapply(lhs_names, function(x) paste0("(", paste(x, collapse = ", "), ")"), character(1))),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#' Check the decisiongroups vector against the utility list
#' @noRd
check_decisiongroups <- function(decisiongroups, utility = NULL, arg = "decisiongroups") {
  if (!is.numeric(decisiongroups) || length(decisiongroups) < 2) {
    stop(
      "`", arg, "` must be a numeric vector of cumulative shares starting at 0 ",
      "and ending at 1, for example c(0, 0.7, 1). Got ",
      describe_value(decisiongroups), ".",
      call. = FALSE
    )
  }
  if (decisiongroups[1] != 0) {
    stop("`", arg, "` must start at 0, not ", decisiongroups[1], ".", call. = FALSE)
  }
  if (utils::tail(decisiongroups, 1) != 1) {
    stop(
      "`", arg, "` must end at 1, not ", utils::tail(decisiongroups, 1),
      ". The values are cumulative shares of the sample.",
      call. = FALSE
    )
  }
  if (any(diff(decisiongroups) <= 0)) {
    stop(
      "`", arg, "` must increase: each value is the cumulative share of ",
      "respondents up to that group. Got ",
      paste(decisiongroups, collapse = ", "), ".",
      call. = FALSE
    )
  }

  if (!is.null(utility) && length(utility) != length(decisiongroups) - 1L) {
    stop(
      "`", arg, "` defines ", length(decisiongroups) - 1L,
      " decision group(s) but `utility` has ", length(utility),
      ". Give one list of utility functions per group.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
