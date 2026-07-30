#' Conditionally emit a message based on verbose level
#'
#' The message is only built when it will be shown, so anything expensive can be
#' passed in freely. Do not rely on side effects inside `...`: they will not run
#' below the requested level.
#' @param verbose Current verbose setting (integer 0-3)
#' @param level Minimum verbose level required to print this message
#' @param ... Arguments passed to message()
#' @noRd
vmsg <- function(verbose, level, ...) {
  if (isTRUE(verbose >= level)) message(...)
}

#' Print a captured object under a heading, at a given verbose level
#' @noRd
vmsg_print <- function(verbose, level, heading, object) {
  if (!isTRUE(verbose >= level)) {
    return(invisible(NULL))
  }
  message(heading, "\n", paste(utils::capture.output(print(object)), collapse = "\n"))
}

#' Describe a value for use in an error message
#'
#' Keeps messages specific without dumping whole objects into the console.
#' @noRd
describe_value <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.function(x)) {
    return("a function")
  }
  if (is.data.frame(x)) {
    return(sprintf("a data frame with %d row(s) and %d column(s)", nrow(x), ncol(x)))
  }
  if (is.list(x)) {
    return(sprintf("a list of length %d", length(x)))
  }
  if (length(x) == 0) {
    return(sprintf("an empty %s vector", class(x)[1]))
  }
  if (length(x) == 1) {
    return(sprintf("%s (%s)", class(x)[1], format_scalar(x)))
  }
  sprintf(
    "a %s vector of length %d (%s%s)",
    class(x)[1], length(x),
    paste(vapply(utils::head(x, 3), format_scalar, character(1)), collapse = ", "),
    if (length(x) > 3) ", ..." else ""
  )
}

#' @noRd
format_scalar <- function(x) {
  if (is.character(x)) {
    return(paste0('"', x, '"'))
  }
  if (is.na(x)) {
    return("NA")
  }
  format(x, digits = 4)
}

#' Join a vector into a readable list for a message
#' @noRd
and_list <- function(x, conjunction = "and") {
  x <- as.character(x)
  if (length(x) == 0) {
    return("")
  }
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    return(paste(x, collapse = paste0(" ", conjunction, " ")))
  }
  paste0(
    paste(x[-length(x)], collapse = ", "),
    ", ", conjunction, " ", x[length(x)]
  )
}

#' Suggest the closest of a set of valid values
#'
#' Turns "unknown value 'ngen'" into a message that points at 'ngene'.
#' @noRd
did_you_mean <- function(value, choices, max_distance = 3) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return("")
  }
  d <- utils::adist(tolower(value), tolower(choices))[1, ]
  close <- choices[d <= max_distance & d == min(d)]
  if (length(close) == 0) {
    return("")
  }
  paste0(" Did you mean ", and_list(paste0("'", close, "'"), "or"), "?")
}

#' Multi-histogram of one coefficient across designs
#'
#' Returns the plot. It deliberately does not print: `aggregateResults()`
#' collects these into a list so the caller decides when and where to draw them.
#' @noRd
plot_multi_histogram <- function(df, feature, label_column, hist = FALSE) {
  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[feature]], fill = .data[[label_column]])
  ) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = mean(.data[[feature]])),
      color = "black", linetype = "dashed", linewidth = 1
    ) +
    ggplot2::labs(x = feature, y = "Density") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = label_column))

  if (isTRUE(hist)) {
    plt <- plt + ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$density)),
      alpha = 0.7, position = "identity", color = "black"
    )
  }

  plt
}


## The function below is intended to find a dataframe called $coef in the output
## regardless of the output data structure, which can vary
find_dataframe <- function(list_object, dataframe_name) {
  # Check if the current object is a list
  if (is.list(list_object)) {
    # Check if the dataframe_name exists in the names of the current list object
    if (dataframe_name %in% names(list_object)) {
      # Check if the object corresponding to dataframe_name is a data frame
      if (is.data.frame(list_object[[dataframe_name]])) {
        return(list_object[[dataframe_name]]) # Return the data frame if found
      }
    }

    # Recursively search through each element of the current list object
    for (element in list_object) {
      result <- find_dataframe(element, dataframe_name)
      if (!is.null(result)) {
        return(result) # Return the data frame if found in any nested list
      }
    }
  }

  return(NULL) # Return NULL if dataframe_name not found
}


#' Modify bcoeff Names
#'
#' This function modifies the names of beta coefficients (`bcoeff`) by removing
#' dots and underscores to ensure consistent naming. If the names are already modified,
#' the function skips processing.
#'
#' @param bcoeff A named list of beta coefficients. The names of this list are
#'   the coefficients used in the utility function.
#' @param verbose Integer verbosity level. The "already cleaned" note is only
#'   emitted at level 3, because on the normal path `sim_all()` and
#'   `sim_choice()` both clean the same list.
#'
#' @return A list containing:
#' \describe{
#'   \item{bcoeff}{The modified `bcoeff` list with updated names.}
#'   \item{bcoeff_lookup}{A tibble mapping the original names to the modified names.}
#' }
#'
#' @keywords internal
modify_bcoeff_names <- function(bcoeff, verbose = 3) {
  # Check if bcoeff already has a lookup table attribute
  if (!is.null(attr(bcoeff, "bcoeff_lookup"))) {
    vmsg(verbose, 3, "Coefficient names were already cleaned; skipping.")
    bcoeff_lookup <- attr(bcoeff, "bcoeff_lookup")
  } else {
    cleaned <- stringr::str_replace_all(names(bcoeff), "[._]", "")

    clash <- cleaned[duplicated(cleaned)]
    if (length(clash) > 0) {
      offenders <- names(bcoeff)[cleaned %in% clash]
      stop(
        "Coefficient names must stay distinct after dots and underscores are ",
        "removed, because that is how they are passed to mixl. These collapse ",
        "onto the same name: ", and_list(paste0("`", offenders, "`")),
        " all become `", clash[1], "`. Rename them.",
        call. = FALSE
      )
    }

    bcoeff_lookup <- tibble::tibble(
      original = names(bcoeff),
      modified = cleaned
    )
    names(bcoeff) <- cleaned
    attr(bcoeff, "bcoeff_lookup") <- bcoeff_lookup
  }

  # Return both modified bcoeff and lookup table
  list(
    bcoeff = bcoeff,
    bcoeff_lookup = bcoeff_lookup
  )
}


#' Regular expression matching the utility columns simulate_choices() creates
#'
#' Anchored on purpose: a design variable called `U_pay` or `V_total` must not be
#' mistaken for an alternative's utility.
#' @noRd
utility_col_pattern <- function(prefixes = c("V", "e", "U")) {
  paste0("^(", paste(prefixes, collapse = "|"), ")_[0-9]+$")
}

#' Names of the utility columns of one kind, in numerical order
#' @noRd
utility_cols <- function(x, prefix) {
  nms <- names(x)
  hits <- grep(paste0("^", prefix, "_[0-9]+$"), nms, value = TRUE)
  hits[order(as.integer(sub(paste0("^", prefix, "_"), "", hits)))]
}
