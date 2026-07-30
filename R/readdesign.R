#'  Creates a dataframe with the design.
#'
#' @param design The path to a design file.
#' @param designtype Which tool produced the design. One of `"ngene"`,
#'   `"spdesign"`, `"idefix"` or `"matrix"`. Ngene designs should be stored as
#'   the standard `.ngd` output. `spdesign` should be the spdesign object stored
#'   as an RDS file. Idefix objects should also be stored as an RDS file. Use
#'   `"matrix"` for a design you built yourself and saved as a plain data frame,
#'   one row per choice situation. If `designtype` is not given it is detected
#'   from the file, which is helpful when a folder mixes designs from different
#'   tools.
#' @param destype Deprecated. Use `designtype` instead.
#' @param verbose Set to 0 to suppress the note saying which kind of design was
#'   detected. [sim_all()] passes its own `verbose` down, so that note follows the
#'   verbosity of the whole simulation.
#' @return A data frame with one row per choice situation, a `Choice.situation`
#'   column, and a `Block` column when the design is blocked.
#' @export
#'
#' @examples library(simulateDCE)
#' mydesign <- readdesign(
#'   system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE"),
#'   "ngene"
#' )
#'
#' print(mydesign)
#'
readdesign <- function(design, designtype = NULL, destype = NULL, verbose = 1) {
  if (missing(design)) {
    stop("`design` must be the path to a design file.", call. = FALSE)
  }
  if (!is.character(design) || length(design) != 1L) {
    stop(
      "`design` must be a single file path, not ", describe_value(design),
      ". To read several designs at once use `sim_all(designpath = )`.",
      call. = FALSE
    )
  }

  designfile <- design

  if (!is.null(designtype) && !is.null(destype)) {
    stop(
      "Use `designtype` only. `destype` is the old name for the same argument, ",
      "so giving both is ambiguous.",
      call. = FALSE
    )
  }

  if (!is.null(destype)) {
    message("`destype` is deprecated. Use `designtype` instead.")
    designtype <- destype
  }

  ## Cheap argument checks first, so a typo is reported even when the path is also
  ## wrong.
  valid_types <- c("ngene", "spdesign", "idefix", "matrix")
  if (!is.null(designtype)) {
    if (!is.character(designtype) || length(designtype) != 1L ||
      !designtype %in% valid_types) {
      stop(
        "`designtype` must be one of ", and_list(paste0("'", valid_types, "'"), "or"),
        ", or left empty so it can be detected from the file. Got ",
        describe_value(designtype), ".",
        did_you_mean(designtype, valid_types),
        call. = FALSE
      )
    }
  }

  if (!file.exists(designfile)) {
    stop(
      "Design file not found: '", designfile, "'.",
      if (dir.exists(designfile)) {
        " That path is a directory. Point `design` at a file, or use `sim_all(designpath = )` to read a whole folder."
      } else {
        paste0(" Working directory is '", getwd(), "'.")
      },
      call. = FALSE
    )
  }

  ## Read an RDS at most once, however many times the guesser and the reader
  ## need to look at it.
  rds_cache <- NULL
  rds_loaded <- FALSE
  load_rds <- function() {
    if (!rds_loaded) {
      rds_cache <<- tryCatch(
        readRDS(designfile),
        error = function(e) {
          stop(
            "Could not read '", basename(designfile), "' as an R object: ",
            conditionMessage(e),
            ". Ngene designs must keep the .ngd extension; designs from ",
            "spdesign or idefix must be saved with saveRDS().",
            call. = FALSE
          )
        }
      )
      rds_loaded <<- TRUE
    }
    rds_cache
  }

  if (is.null(designtype)) {
    designtype <- guess_designtype(designfile, load_rds)
    vmsg(verbose, 1, "Detected ", designtype, " design in '", basename(designfile), "'.")
  }

  out <- switch(designtype,
    "ngene"    = read_ngene(designfile),
    "spdesign" = read_design_matrix(as_design_frame(load_rds(), designfile)),
    "matrix"   = read_design_matrix(as_design_frame(load_rds(), designfile)),
    "idefix"   = read_idefix(as_design_frame(load_rds(), designfile))
  )

  validate_design(out, designfile, designtype)
}


#' Work out which tool produced a design file
#'
#' Every branch tests for something positive. A plain data frame is a design
#' matrix the user built themselves, which is common enough that guessing
#' "idefix" for it, as earlier versions did, silently returned nonsense.
#' @noRd
guess_designtype <- function(designfile, load_rds) {
  if (grepl("\\.ngd$", designfile, ignore.case = TRUE)) {
    return("ngene")
  }

  obj <- load_rds()

  if (inherits(obj, "spdesign")) {
    return("spdesign")
  }

  if (is.data.frame(obj)) {
    return("matrix")
  }

  if (is.list(obj)) {
    if (looks_like_idefix(obj)) {
      return("idefix")
    }
    if ("design" %in% names(obj)) {
      return("spdesign")
    }
    stop(
      "Could not tell what kind of design '", basename(designfile), "' holds. ",
      "It is a list with the element(s) ", and_list(paste0("`", names(obj), "`")),
      ", but a design is expected to be a data frame, an spdesign object, or an ",
      "idefix object. Set `designtype` explicitly if you know which it is.",
      call. = FALSE
    )
  }

  if (is.matrix(obj)) {
    return("matrix")
  }

  stop(
    "Could not tell what kind of design '", basename(designfile), "' holds: it is ",
    describe_value(obj), ". Save your design as a data frame, an spdesign object, ",
    "or an idefix object, or set `designtype` explicitly.",
    call. = FALSE
  )
}

#' Does this list look like the output of idefix?
#'
#' idefix returns a list whose `design` matrix has row names like `set1.alt1`,
#' alongside the error and probability elements.
#' @noRd
looks_like_idefix <- function(obj) {
  if (!"design" %in% names(obj)) {
    return(FALSE)
  }
  if (any(c("error", "inf.error", "probs") %in% names(obj))) {
    return(TRUE)
  }
  rn <- rownames(obj[["design"]])
  !is.null(rn) && any(grepl("^set[0-9]+\\.", rn))
}

#' Pull the design table out of whatever container it arrived in
#' @noRd
as_design_frame <- function(obj, designfile) {
  if (is.list(obj) && !is.data.frame(obj)) {
    if (!"design" %in% names(obj)) {
      stop(
        "The 'design' list element is missing from '", basename(designfile),
        "'. It holds ",
        if (length(names(obj))) and_list(paste0("`", names(obj), "`")) else "no named elements",
        ". Make sure to provide a proper spdesign or idefix object.",
        call. = FALSE
      )
    }
    obj <- obj[["design"]]
  }
  as.data.frame(obj)
}

#' Read an Ngene .ngd file
#' @noRd
read_ngene <- function(designfile) {
  raw <- suppressWarnings(readr::read_delim(
    designfile,
    delim = "\t",
    escape_double = FALSE,
    trim_ws = TRUE,
    col_select = c(-"Design", -tidyr::starts_with("...")),
    name_repair = "universal", show_col_types = FALSE, guess_max = Inf
  ))

  if (!"Choice.situation" %in% names(raw)) {
    stop(
      "'", basename(designfile), "' does not look like an Ngene design: no ",
      "'Choice situation' column was found after reading it as tab separated ",
      "text. Columns found: ", and_list(paste0("`", names(raw), "`")), ".",
      call. = FALSE
    )
  }

  dplyr::filter(raw, !is.na(.data$Choice.situation))
}

#' Tidy a plain design matrix, from spdesign or built by hand
#'
#' Only the first underscore in a name becomes a dot, so `alt2_farm2` matches the
#' `alt2.farm2` convention while a name like `alt2_farm_new` keeps its second
#' underscore.
#' @noRd
read_design_matrix <- function(design) {
  design %>%
    dplyr::rename_with(~ stringr::str_replace(., pattern = "_", "\\."), tidyr::everything()) %>%
    dplyr::rename_with(~ dplyr::case_when(
      tolower(.) == "block" ~ "Block",
      tolower(.) == "choice.situation" ~ "Choice.situation",
      TRUE ~ .
    ), tidyr::everything()) %>%
    add_choice_situation()
}

#' Reshape an idefix design from long to one row per choice situation
#' @noRd
read_idefix <- function(design) {
  design %>%
    tibble::rownames_to_column(var = "row_id") %>%
    dplyr::filter(!grepl("no.choice", .data$row_id)) %>%
    dplyr::select(!dplyr::contains("cte")) %>%
    dplyr::mutate(
      Choice.situation = as.integer(sub("^set(\\d+).*", "\\1", .data$row_id)),
      alt = sub(".*\\.", "", .data$row_id)
    ) %>%
    dplyr::select(-"row_id") %>%
    tidyr::pivot_wider(
      id_cols = "Choice.situation",
      names_from = "alt",
      values_from = -c("Choice.situation", "alt"),
      names_glue = "{alt}.{.value}"
    )
}

#' Number the choice situations 1..n unless the design already numbers them
#' @noRd
add_choice_situation <- function(design) {
  if ("Choice.situation" %in% names(design)) {
    return(design)
  }
  dplyr::mutate(design, Choice.situation = seq_len(dplyr::n()))
}

#' Final sanity checks shared by every reader
#' @noRd
validate_design <- function(design, designfile, designtype) {
  design <- as.data.frame(design)

  if (nrow(design) == 0) {
    stop(
      "'", basename(designfile), "' was read as a ", designtype,
      " design but contains no choice situations.",
      call. = FALSE
    )
  }
  if (!"Choice.situation" %in% names(design)) {
    stop(
      "'", basename(designfile), "' was read as a ", designtype,
      " design but has no 'Choice.situation' column. This is an internal ",
      "inconsistency; please report it.",
      call. = FALSE
    )
  }
  if (anyDuplicated(design$Choice.situation)) {
    dupes <- unique(design$Choice.situation[duplicated(design$Choice.situation)])
    stop(
      "'", basename(designfile), "' has repeated choice situation number(s): ",
      and_list(utils::head(dupes, 5)),
      if (length(dupes) > 5) ", ..." else "",
      ". Each row of a design must be one distinct choice situation. If the file ",
      "is in long format with one row per alternative, reshape it to wide first.",
      call. = FALSE
    )
  }
  if (ncol(design) < 2) {
    stop(
      "'", basename(designfile), "' was read as a ", designtype,
      " design with no attribute columns, only 'Choice.situation'.",
      call. = FALSE
    )
  }

  design
}
