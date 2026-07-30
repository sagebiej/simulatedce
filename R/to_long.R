#' Reshape a simulated dataset to one row per alternative
#'
#' @description
#' `simulate_choices()` returns wide data: one row per choice occasion, with a
#' column for each alternative's attributes. That is what `mixl` and `apollo`
#' expect, so if those are your estimators you need nothing from this function.
#'
#' `mlogit`, `gmnl` and `survival::clogit` want long data instead: one row per
#' alternative per choice occasion, with a single column per attribute and a
#' logical saying which row was chosen. `to_long()` does that reshape.
#'
#' @param data One simulated dataset, as returned by [simulate_choices()] or found
#'   in the output of [sim_all()] with `estimate = FALSE`.
#' @param keep_utilities Keep the per-alternative `V`, `e` and `U` values as columns
#'   in the long data. `FALSE` by default, since they are the truth rather than
#'   something a model should see. Useful for teaching.
#'
#' @details
#' Attribute columns are recognised by the `alt<k>_<name>` naming that
#' [simulate_choices()] produces, or the `alt<k>.<name>` form before the rename. An
#' attribute that appears for some alternatives but not others gets `NA` where it is
#' absent, which is what a no-choice alternative with no attributes looks like.
#'
#' Everything that is not alternative-specific, such as `ID`, `task`,
#' `Choice.situation`, `Block`, the decision `group` and any respondent-level
#' coefficient draws, is repeated down the alternatives.
#'
#' @return A data frame with `nrow(data) * n_alt` rows and the columns
#'   \describe{
#'     \item{`ID`, `task`}{respondent and choice occasion, repeated per alternative}
#'     \item{`alt`}{which alternative this row is, 1 to the number of alternatives}
#'     \item{`chosen`}{`TRUE` on the row that was chosen}
#'     \item{`available`}{`TRUE` when the alternative was on offer, see [availability]}
#'     \item{one column per attribute}{the value for this alternative}
#'   }
#'   sorted by `ID`, `task` and `alt`.
#'
#' @seealso [availability] for how availability is expressed in the wide data.
#'
#' @export
#'
#' @examples
#' wide <- data.frame(
#'   ID = rep(1:3, each = 2),
#'   task = rep(1:2, 3),
#'   alt1_price = c(2, 4, 6, 8, 2, 4),
#'   alt1_qual = c(0, 1, 0, 1, 1, 0),
#'   alt2_price = c(8, 6, 4, 2, 6, 8),
#'   alt2_qual = c(1, 0, 1, 0, 0, 1),
#'   CHOICE = c(1, 2, 1, 1, 2, 1)
#' )
#'
#' long <- to_long(wide)
#' head(long, 4)
#'
#' # what mlogit or clogit would be given
#' table(long$alt, long$chosen)
to_long <- function(data, keep_utilities = FALSE) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be one simulated dataset, not ", describe_value(data),
      ". With sim_all(estimate = FALSE) that is result[[design]][[run]].",
      call. = FALSE
    )
  }
  if (!"CHOICE" %in% names(data)) {
    stop(
      "`data` has no `CHOICE` column, so there is nothing to mark as chosen. ",
      "Pass a dataset that has been through simulate_choices().",
      call. = FALSE
    )
  }

  wide <- as.data.frame(data)
  parts <- split_alternative_columns(wide)

  if (length(parts$alternatives) == 0) {
    stop(
      "Could not work out how many alternatives there are. Expected columns like ",
      "`alt1_price` and `alt2_price`, or `U_1` and `U_2`. Columns found: ",
      and_list(paste0("`", utils::head(names(wide), 10), "`")),
      if (ncol(wide) > 10) ", ..." else "", ".",
      call. = FALSE
    )
  }

  n_alt <- length(parts$alternatives)
  av <- availability_matrix(wide, n_alt)
  ## The wide availability columns are dropped: their content is the `available`
  ## column, one row at a time.
  shared <- setdiff(
    names(wide),
    c(parts$alt_columns, parts$utility_columns, "CHOICE", availability_columns(wide))
  )

  rows <- lapply(seq_len(n_alt), function(k) {
    out <- wide[, shared, drop = FALSE]
    out$alt <- parts$alternatives[k]
    out$chosen <- wide$CHOICE == parts$alternatives[k]
    out$available <- if (is.null(av)) TRUE else av[, k] == 1

    for (a in parts$attributes) {
      col <- parts$lookup[[paste0(a, "|", parts$alternatives[k])]]
      out[[a]] <- if (is.null(col)) NA_real_ else wide[[col]]
    }

    if (isTRUE(keep_utilities)) {
      for (prefix in c("V", "e", "U")) {
        col <- paste0(prefix, "_", parts$alternatives[k])
        if (col %in% names(wide)) out[[prefix]] <- wide[[col]]
      }
    }

    out
  })

  long <- do.call(rbind, rows)

  ## sort by respondent, then occasion, then alternative
  order_by <- list()
  for (nm in c("ID", "task", "Choice.situation", "Choice_situation")) {
    if (nm %in% names(long)) order_by[[nm]] <- long[[nm]]
  }
  order_by[["alt"]] <- long$alt
  long <- long[do.call(order, order_by), , drop = FALSE]

  rownames(long) <- NULL

  front <- intersect(
    c("ID", "task", "Choice.situation", "Choice_situation", "alt", "chosen", "available"),
    names(long)
  )
  long[, c(front, setdiff(names(long), front)), drop = FALSE]
}


#' Work out which columns belong to which alternative
#' @noRd
split_alternative_columns <- function(wide) {
  nms <- names(wide)
  hits <- grep("^alt[0-9]+[._]", nms, value = TRUE)

  alt_num <- as.integer(sub("^alt([0-9]+)[._].*$", "\\1", hits))
  attribute <- sub("^alt[0-9]+[._]", "", hits)

  ## paste0 recycles to its longest argument, so with no hits at all the separator
  ## alone would produce a name of length 1 against a list of length 0.
  lookup <- if (length(hits) == 0) {
    list()
  } else {
    stats::setNames(as.list(hits), paste0(attribute, "|", alt_num))
  }

  utility_columns <- grep(utility_col_pattern(), nms, value = TRUE)

  ## An alternative need not have any attributes of its own: a no-choice option is
  ## usually nothing but a constant. Count the alternatives from every signal there
  ## is, so such an alternative is not quietly dropped.
  from_utilities <- as.integer(sub("^[VeU]_", "", utility_columns))
  from_availability <- as.integer(sub("^av[._]?", "",
    availability_columns(wide) %||% character(0),
    ignore.case = TRUE
  ))
  from_choice <- if ("CHOICE" %in% nms && any(is.finite(wide$CHOICE))) {
    max(wide$CHOICE, na.rm = TRUE)
  } else {
    0L
  }

  highest <- suppressWarnings(max(
    c(alt_num, from_utilities, from_availability, from_choice),
    na.rm = TRUE
  ))
  alternatives <- if (is.finite(highest) && highest >= 1) seq_len(highest) else integer(0)

  list(
    alternatives = alternatives,
    attributes = unique(attribute),
    alt_columns = hits,
    utility_columns = utility_columns,
    lookup = lookup
  )
}
