#' Run a simulation over every design in a folder
#'
#' Wrapper around [sim_choice()] that reads every design file in `designpath`,
#' simulates and optionally estimates choices for each, and aggregates the
#' results so the designs can be compared.
#'
#' @param nosim Number of runs or simulations. For testing use 2 but once you go serious, use at least 200, for better results use 2000.
#' @param resps Number of respondents you want to simulate
#' @inheritParams readdesign
#' @inheritParams sim_choice
#' @inheritParams simulate_choices
#' @inheritParams createDataset
#' @param designpath The path to the folder where the designs are stored. For example "c:/myfancydec/Designs"
#' @param pattern Regular expression picking the design files out of `designpath`,
#'   matched without regard to case. By default any `.ngd` or `.rds` file. Widen it
#'   if your designs use a different extension. Anything in the folder that does
#'   not match is ignored, so a stray note or plot no longer breaks the run.
#' @param reshape_type Must be "auto", "stats" to use the reshape from the stats package or tidyr to use pivot longer. Default is auto and should not bother you. Only change it once you face an error at this position and you may be lucky that it works then.
#' @return A list, with all information on the simulation. This list an be easily processed by the user and in the rmarkdown template.
#' @export
#'
#' @examples
#' library(rlang)
#' designpath <- system.file("extdata", "SE_DRIVE", package = "simulateDCE")
#' resps <- 120 # number of respondents
#' nosim <- 2 # number of simulations to run (about 500 is minimum)
#'
#' decisiongroups <- c(0, 0.7, 1)
#'
#' # pass beta coefficients as a list
#' bcoeff <- list(
#'   b.preis = -0.01,
#'   b.lade = -0.07,
#'   b.warte = 0.02
#' )
#'
#' manipulations <- list(
#'   alt1.x2 = expr(alt1.x2 / 10),
#'   alt1.x3 = expr(alt1.x3 / 10),
#'   alt2.x2 = expr(alt2.x2 / 10),
#'   alt2.x3 = expr(alt2.x3 / 10)
#' )
#'
#'
#' # place your utility functions here
#' ul <- list(
#'   u1 =
#'
#'     list(
#'       v1 = V.1 ~ b.preis * alt1.x1 + b.lade * alt1.x2 + b.warte * alt1.x3,
#'       v2 = V.2 ~ b.preis * alt2.x1 + b.lade * alt2.x2 + b.warte * alt2.x3
#'     ),
#'   u2 = list(
#'     v1 = V.1 ~ b.preis * alt1.x1,
#'     v2 = V.2 ~ b.preis * alt2.x1
#'   )
#' )
#'
#'
#' sedrive <- sim_all(
#'   nosim = nosim,
#'   resps = resps,
#'   designpath = designpath,
#'   u = ul,
#'   bcoeff = bcoeff,
#'   decisiongroups = decisiongroups,
#'   manipulations = manipulations,
#'   mode = "sequential",
#'   estimate = FALSE
#' )
#'
sim_all <- function(nosim = 2,
                    resps,
                    designtype = NULL,
                    destype = NULL,
                    designpath,
                    pattern = "\\.(ngd|rds)$",
                    u,
                    bcoeff,
                    decisiongroups = c(0, 1),
                    manipulations = list(),
                    estimate = TRUE,
                    chunks = 1,
                    model = c("mnl", "mixed"),
                    correlation = NULL,
                    estimator = "mixl",
                    n_draws = 200,
                    sets_per_resp = NULL,
                    sample_sets = c("balanced", "random", "with_replacement"),
                    resample = TRUE,
                    utility_transform_type = c("exact", "simple"),
                    reshape_type = "auto",
                    mode = c("parallel", "sequential"),
                    preprocess_function = NULL,
                    savefile = NULL,
                    keep_models = TRUE,
                    keep_utilities = TRUE,
                    workers = NULL,
                    seed = NULL,
                    verbose = 1) {
  #################################################
  ########## Input Validation Test ###############
  #################################################
  mode <- match.arg(mode)
  sample_sets <- match.arg(sample_sets)
  utility_transform_type <- match.arg(utility_transform_type)
  model <- match.arg(model)
  verbose <- check_verbose(verbose)

  ## Set the seed here rather than asking the user to remember it outside, so a
  ## script records how it was run. This also makes the parallel path
  ## reproducible, since furrr derives its streams from the current RNG state.
  if (!is.null(seed)) {
    set.seed(check_count(seed, "seed", min = -.Machine$integer.max))
  }

  ########### validate the utility function ########
  if (missing(u)) {
    stop(
      "`u` must be provided and must be a list containing at least one list ",
      "element: one inner list of utility functions per decision group. For ",
      "example u = list(u1 = list(v1 = V.1 ~ b1 * alt1.x, v2 = V.2 ~ 0)).",
      call. = FALSE
    )
  }
  if (!(is.list(u) && any(vapply(u, is.list, logical(1))))) {
    stop(
      "`u` must be provided and must be a list containing at least one list ",
      "element, not ", describe_value(u),
      ". Even with a single decision group the utility functions go in an inner ",
      "list: u = list(u1 = list(v1 = V.1 ~ ..., v2 = V.2 ~ ...)).",
      call. = FALSE
    )
  }
  check_utility_list(u, arg = "u")

  ########## validate the bcoeff list ################
  if (missing(bcoeff)) {
    stop(
      "`bcoeff` is required: one entry per coefficient used in `u`, either a ",
      "number for a fixed coefficient or a list like ",
      "list(dist = \"normal\", mean = -0.2, sd = 0.1) for a random one.",
      call. = FALSE
    )
  }
  check_bcoeff_list(bcoeff)

  nosim <- check_count(nosim, "nosim")
  chunks <- check_count(chunks, "chunks")

  if (nosim < chunks) {
    stop(
      "`chunks` is ", chunks, " but `nosim` is ", nosim,
      ". The number of chunks says how often results are written to disk, which ",
      "can happen at most once per run.",
      call. = FALSE
    )
  }

  check_decisiongroups(decisiongroups, u, arg = "decisiongroups")

  ## Validate each coefficient specification, so a typo is reported with the name
  ## of the parameter it belongs to rather than failing later inside the draw.
  for (nm in names(bcoeff)) as_dist_spec(bcoeff[[nm]], nm)

  #### check that all the coefficients in utility function have a corresponding value in bcoeff ####
  coeff_names_ul <- unique(unlist(lapply(u, function(group) {
    unlist(lapply(group, function(f) {
      all_vars <- all.vars(stats::as.formula(f))
      all_vars[grep("^b", all_vars)]
    }))
  })))

  missing_coeffs <- setdiff(coeff_names_ul, names(bcoeff))
  if (length(missing_coeffs) > 0) {
    stop(
      "These coefficients appear in `u` but not in `bcoeff`: ",
      and_list(paste0("`", missing_coeffs, "`")),
      ". Add them to `bcoeff`, or check for a typo.",
      if (length(names(bcoeff))) {
        paste0(" `bcoeff` currently has ", and_list(paste0("`", names(bcoeff), "`")), ".")
      } else {
        ""
      },
      call. = FALSE
    )
  }

  ## Only report the specification once everything about it checks out.
  vmsg(verbose, 1,
    "\nParameter specification:\n",
    paste(
      vapply(names(bcoeff), function(nm) describe_spec(bcoeff[[nm]], nm), character(1)),
      collapse = "\n"
    ),
    "\n"
  )

  unused_coeffs <- setdiff(names(bcoeff), coeff_names_ul)
  if (length(unused_coeffs) > 0) {
    warning(
      "These coefficients are in `bcoeff` but never used in `u`: ",
      and_list(paste0("`", unused_coeffs, "`")),
      ". They will have no effect. Note that only names starting with \"b\" are ",
      "recognised as coefficients.",
      call. = FALSE
    )
  }

  ########## validate resps #####################
  if (missing(resps)) {
    stop("`resps` is required: the number of respondents to simulate per run.",
      call. = FALSE
    )
  }
  resps <- check_count(resps, "resps")

  ########## validate designpath ################
  if (missing(designpath)) {
    stop("`designpath` is required: the folder holding your design file(s).", call. = FALSE)
  }
  if (!is.character(designpath) || length(designpath) != 1L) {
    stop(
      "`designpath` must be a single folder path, not ", describe_value(designpath), ".",
      call. = FALSE
    )
  }
  if (!dir.exists(designpath)) {
    stop(
      "The folder where your designs are stored does not exist: '", designpath, "'. ",
      if (file.exists(designpath)) {
        "That path is a file. Use readdesign() and sim_choice() for a single design."
      } else {
        paste0("Working directory is '", getwd(), "'.")
      },
      call. = FALSE
    )
  }

  #################################################
  ########## End Validation Tests #################
  #################################################

  bcoeff_result <- modify_bcoeff_names(bcoeff, verbose = verbose)
  bcoeff <- bcoeff_result$bcoeff

  designfile <- list.files(designpath, full.names = TRUE, pattern = pattern, ignore.case = TRUE)
  designfile <- designfile[!dir.exists(designfile)]

  if (length(designfile) == 0) {
    present <- list.files(designpath)
    stop(
      "No design files matching '", pattern, "' in '", designpath, "'. ",
      if (length(present)) {
        paste0(
          "The folder holds ", and_list(paste0("'", utils::head(present, 8), "'")),
          if (length(present) > 8) ", ..." else "",
          ". Adjust `pattern` if your designs use another extension."
        )
      } else {
        "The folder is empty."
      },
      call. = FALSE
    )
  }

  designname <- vapply(designfile, clean_design_name, character(1), USE.NAMES = FALSE)

  if (anyDuplicated(designname)) {
    dupes <- unique(designname[duplicated(designname)])
    stop(
      "Two or more designs end up with the same name once the file extension is ",
      "dropped: ", and_list(paste0("'", dupes, "'")),
      ". Results are labelled by that name, so please rename the files.",
      call. = FALSE
    )
  }

  ## Start the workers once for all designs rather than once per design, and hand
  ## the caller's plan back untouched afterwards.
  if (identical(mode, "parallel")) {
    oldplan <- future::plan()
    on.exit(future::plan(oldplan), add = TRUE)
    if (inherits(oldplan, "sequential")) {
      if (!is.null(workers)) {
        future::plan("multisession", workers = workers)
      } else {
        future::plan("multisession")
      }
    }
    vmsg(verbose, 2, "Running in parallel on ", future::nbrOfWorkers(), " worker(s).")
  }

  tictoc::tic("total time for simulation and estimation")

  run_design <- function(file) {
    sim_choice(
      designfile = file,
      no_sim = nosim,
      respondents = resps,
      designtype = designtype,
      destype = destype,
      u = u,
      bcoeff = bcoeff,
      decisiongroups = decisiongroups,
      manipulations = manipulations,
      estimate = estimate,
      chunks = chunks,
      model = model,
      correlation = correlation,
      estimator = estimator,
      n_draws = n_draws,
      sets_per_resp = sets_per_resp,
      sample_sets = sample_sets,
      resample = resample,
      utility_transform_type = utility_transform_type,
      mode = mode,
      preprocess_function = preprocess_function,
      savefile = savefile,
      keep_models = keep_models,
      keep_utilities = keep_utilities,
      workers = workers,
      verbose = verbose
    )
  }

  if (is.null(savefile)) {
    all_designs <- stats::setNames(purrr::map(designfile, run_design), designname)
  } else {
    purrr::walk(designfile, run_design)
    gc()

    ## Read back exactly the files this run wrote, matched to their design by
    ## name. Reading whatever happened to be in the folder broke as soon as it
    ## held anything else.
    saved <- file.path(
      dirname(savefile),
      paste0(basename(savefile), "_", designname, ".qs")
    )
    found <- file.exists(saved)
    if (!all(found)) {
      stop(
        "Expected saved result(s) missing after the run: ",
        and_list(paste0("'", basename(saved[!found]), "'")),
        " in '", dirname(savefile), "'.",
        call. = FALSE
      )
    }
    all_designs <- stats::setNames(purrr::map(saved, qs2::qs_read), designname)
  }

  time <- tictoc::toc(quiet = TRUE)
  vmsg(verbose, 1, time[["callback_msg"]])

  all_designs[["time"]] <- time
  all_designs[["arguments"]] <- list(
    "Beta values" = bcoeff,
    "Utility functions" = strip_compiled(u),
    "Decision groups" = decisiongroups,
    "Manipulation of vars" = manipulations,
    "Number Simulations" = nosim,
    "Respondents" = resps,
    "Designpath" = designpath,
    "Reshape Type" = reshape_type,
    "mode" = mode,
    "designname" = designname,
    "Sets per respondent" = sets_per_resp,
    "Set sampling" = if (is.null(sets_per_resp)) "blocks" else sample_sets,
    "Model" = model,
    "Correlation" = correlation,
    "Estimator" = if (is.function(estimator)) "custom" else estimator,
    "Draws" = if (identical(model, "mixed")) n_draws else NA_integer_,
    "Seed" = if (is.null(seed)) NA_integer_ else seed
  )
  ## Kept for backward compatibility: this element used to be misspelled.
  all_designs[["arguements"]] <- all_designs[["arguments"]]

  if (isTRUE(estimate)) {
    all_designs <- aggregateResults(all_designs = all_designs)
  }

  all_designs
}
