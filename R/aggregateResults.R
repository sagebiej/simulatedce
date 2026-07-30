#' Aggregate Simulation Results
#'
#' Processes the simulation results to extract summaries, coefficients, and graphs.
#'
#' @param all_designs A list of simulation results from [sim_choice()]. Can contain
#'   different designs but needs the common structure returned by `sim_choice()`.
#'   Ignored when `fromfolder` is supplied.
#' @param fromfolder A folder from where to read simulations. If provided, the
#'   function reads all `.qs` files from the folder (each one a single design
#'   output saved by [sim_choice()] or [sim_all()] via `savefile`) and merges them.
#'   This is useful to combine results from independent runs, e.g. when you
#'   simulated three designs earlier and now want to add a fourth. Each saved file
#'   stores its own `designname` and `bcoeff`, so the folder is self-describing.
#' @param print_plots Draw the density plots as a side effect. `FALSE` by default:
#'   the plots are returned in `$graphs` so you decide where they go. Printing
#'   them unasked created a stray `Rplots.pdf` in non-interactive sessions.
#' @param reshape_type Deprecated and ignored. Results are no longer reshaped by
#'   splitting column names, so there is nothing left to choose.
#' @return A list with aggregated results including summary, coefficients, graphs, and power.
#' @export
aggregateResults <- function(all_designs, fromfolder = NULL, print_plots = FALSE,
                             reshape_type = NULL) {
  if (!is.null(reshape_type)) {
    message(
      "`reshape_type` is deprecated and ignored. Results are collected directly ",
      "from each design rather than by splitting column names apart, so the ",
      "tidyr and stats reshape paths are both gone."
    )
  }

  if (!is.null(fromfolder)) {
    all_designs <- read_saved_designs(fromfolder)
  }

  if (missing(all_designs) || !is.list(all_designs)) {
    stop(
      "`all_designs` must be the list returned by sim_all() or a list of ",
      "sim_choice() outputs, not ", describe_value(all_designs),
      ". Alternatively pass `fromfolder` to read saved results from disk.",
      call. = FALSE
    )
  }

  args <- all_designs[["arguments"]] %||% all_designs[["arguements"]]
  if (is.null(args)) {
    stop(
      "This list has no `arguments` element, so the designs and their true ",
      "parameter values cannot be identified. Pass the whole object returned by ",
      "sim_all(), or use `fromfolder` to read saved results.",
      call. = FALSE
    )
  }

  designname <- args[["designname"]]
  bcoeff <- args[["Beta values"]]
  ## A mixed logit estimates the distribution's own parameters, so what counts as
  ## the true value depends on which model was fitted.
  model <- args[["Model"]] %||% "mnl"

  designs <- all_designs[intersect(designname, names(all_designs))]
  if (length(designs) == 0) {
    stop(
      "None of the expected designs (", and_list(paste0("'", designname, "'")),
      ") were found in the results. Present: ",
      and_list(paste0("'", names(all_designs), "'")), ".",
      call. = FALSE
    )
  }

  estimated <- purrr::keep(designs, ~ !is.null(.x[["coefs"]]))
  if (length(estimated) == 0) {
    stop(
      "None of the designs hold estimation results. aggregateResults() needs ",
      "models: run the simulation with estimate = TRUE.",
      call. = FALSE
    )
  }

  ## ---- summary table across designs ----------------------------------------

  summaryall <- as.data.frame(purrr::map(estimated, ~ .x$summary)) %>%
    dplyr::select(!dplyr::ends_with("vars")) %>%
    tibble::rownames_to_column("parname") %>%
    ## `quantity` says what each row is, so nothing downstream has to know the
    ## naming scheme in order to pick out the coefficients.
    dplyr::mutate(quantity = quantity_of(.data$parname)) %>%
    dplyr::mutate(parname = stringr::str_remove(.data$parname, "^est_")) %>%
    dplyr::left_join(bcoeff_table(bcoeff, model = model), by = "parname") %>%
    dplyr::relocate("parname", "quantity", dplyr::ends_with(c(
      ".n", "truepar", "truesd", "mean", "median", "bias", "rmse", "coverage",
      "sd", "min", "max", "range", "se"
    )))

  ## ---- one long table of estimates, tagged by design -----------------------
  ##
  ## Built by stacking each design's own coefficient table rather than by taking
  ## wide column names apart on "_". That name splitting broke whenever a design
  ## name was a prefix of another one, and it is why design and coefficient names
  ## were not allowed to contain underscores.

  s <- purrr::list_rbind(
    purrr::map(estimated, function(d) {
      d[["coefs"]] %>%
        dplyr::select(dplyr::starts_with("est_")) %>%
        dplyr::rename_with(~ sub("^est_", "", .x))
    }),
    names_to = "design"
  ) %>%
    dplyr::relocate("design")

  ## ---- one density plot per coefficient ------------------------------------

  p <- list()
  for (att in setdiff(names(s), "design")) {
    p[[att]] <- plot_multi_histogram(s, att, "design")
    if (isTRUE(print_plots)) print(p[[att]])
  }

  all_designs[["summaryall"]] <- summaryall
  all_designs[["estimates"]] <- s
  all_designs[["graphs"]] <- p
  all_designs[["powa"]] <- purrr::map(estimated, ~ .x$power)
  all_designs[["powa_by_par"]] <- purrr::map(estimated, ~ .x$power_by_par)
  all_designs[["convergence"]] <- purrr::map(estimated, ~ .x$convergence)

  all_designs
}


#' Read the design outputs saved by sim_choice() or sim_all() from a folder
#'
#' Each file describes itself, so results produced at different times can be
#' merged later.
#' @noRd
read_saved_designs <- function(fromfolder) {
  if (!is.character(fromfolder) || length(fromfolder) != 1L) {
    stop(
      "`fromfolder` must be a single folder path, not ", describe_value(fromfolder), ".",
      call. = FALSE
    )
  }
  if (!dir.exists(fromfolder)) {
    stop(
      "Folder from where to read simulations does not exist: '", fromfolder, "'. ",
      "Working directory is '", getwd(), "'.",
      call. = FALSE
    )
  }

  files <- list.files(path = fromfolder, pattern = "\\.qs$", full.names = TRUE)
  if (length(files) == 0) {
    present <- list.files(fromfolder)
    stop(
      "No '.qs' files found in '", fromfolder, "'. ",
      if (length(present)) {
        paste0(
          "The folder holds ", and_list(paste0("'", utils::head(present, 8), "'")),
          if (length(present) > 8) ", ..." else "", "."
        )
      } else {
        "The folder is empty."
      },
      " Saved results are written by sim_all(savefile = ).",
      call. = FALSE
    )
  }

  designs <- purrr::map(files, function(f) {
    tryCatch(
      qs2::qs_read(f),
      error = function(e) {
        stop(
          "Could not read '", basename(f), "' as a saved simulation: ",
          conditionMessage(e),
          ". Keep only files written by sim_all(savefile = ) in this folder.",
          call. = FALSE
        )
      }
    )
  })

  ## Fall back to the file name for designs saved before designname was stored.
  designname <- purrr::imap_chr(designs, function(d, i) {
    d[["designname"]] %||% stringr::str_remove(basename(files[[i]]), "\\.qs$")
  })

  if (anyDuplicated(designname)) {
    dupes <- unique(designname[duplicated(designname)])
    stop(
      "Two saved files describe the same design: ",
      and_list(paste0("'", dupes, "'")),
      ". Keep one run per design in a folder, or rename them.",
      call. = FALSE
    )
  }

  designs <- stats::setNames(designs, designname)

  with_bcoeff <- purrr::detect(designs, ~ !is.null(.x[["bcoeff"]]))
  bcoeff <- with_bcoeff[["bcoeff"]]
  if (is.null(bcoeff)) {
    warning(
      "None of the saved files contain `bcoeff`; the `truepar` column will be NA. ",
      "Re-run the simulation with a newer version of the package to store it.",
      call. = FALSE
    )
  }

  ## Rebuild the metadata aggregateResults() expects further down so that the
  ## fromfolder path behaves like the in-memory path used by sim_all().
  model <- with_bcoeff[["model"]] %||% "mnl"

  designs[["arguments"]] <- list(
    "Beta values" = bcoeff,
    "designname"  = unname(designname),
    "Model"       = model
  )
  designs[["arguements"]] <- designs[["arguments"]]

  designs
}


#' Say what each row of a summary table holds
#'
#' The rows of a summary carry a prefix saying whether they describe an estimate,
#' its p value or its standard error. Turning that into a column means no caller
#' needs to know the prefixes, and adding another kind of row later breaks nothing.
#' @noRd
quantity_of <- function(parname) {
  ifelse(startsWith(parname, "rob_pval0_"), "pvalue",
    ifelse(startsWith(parname, "se_"), "se", "estimate")
  )
}
