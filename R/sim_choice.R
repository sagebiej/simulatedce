#' Simulate and estimate choices
#'
#' @param designfile path to a file containing a design.
#' @param no_sim Number of runs i.e. how often do you want the simulation to be repeated
#' @param respondents Number of respondents. How many respondents do you want to simulate in each run.
#' @param u A list with utility functions. The list can incorporate as many decision rule groups as you want. However, each group must be in a list in this list. If you just use one group (the normal),  this  group still  has to be in a list in  the u list. As a convention name beta coefficients starting with a lower case "b"
#' @param estimate If TRUE models will be estimated. If FALSE only a dataset will be simulated. Default is TRUE
#' @inheritParams readdesign
#' @inheritParams simulate_choices
#' @inheritParams createDataset
#' @param chunks How often results should be written to disk while estimating, as
#'   a safety measure against losing a long run. With `no_sim = 100` and
#'   `chunks = 2` the results so far are written after run 50 and again after run
#'   100. The scratch files go to the session's temporary directory and are
#'   removed once everything has been read back.
#' @param model Which model to estimate. `"mnl"`, the default, fits a multinomial
#'   logit, which recovers the mean of each coefficient's distribution and says
#'   nothing about its spread. `"mixed"` fits a mixed logit in which every random
#'   entry of `bcoeff` gets its own respondent-level draw, so both the location and
#'   the spread are estimated. A mixed logit is warm-started from the multinomial
#'   fit, which is why it is noticeably slower than one model per run.
#'
#'   `"mixed"` needs at least one random coefficient, and only the shapes a mixed
#'   logit can express are supported: `normal`, `lognormal` and `neg_lognormal`.
#'   Note that the estimated parameters are then the distribution's own parameters
#'   rather than its moments, so a `lognormal` reports `meanlog` and `sdlog`.
#'   `truepar` follows suit.
#' @param correlation Optional correlation matrix over the random coefficients. See
#'   [make_rand_params()] and [correlate()]. The correlation enters the data
#'   generating process; estimating it is a separate matter, and `model = "mixed"`
#'   fits independent random parameters whatever `correlation` says, so the gap
#'   between the two is exactly what you would be measuring.
#' @param estimator Which model fitter to use. `"mixl"` by default, which handles
#'   both `model` settings. Pass a function of your own to fit something else and
#'   still get the simulation, aggregation and power machinery around it. See
#'   [estimators] for the contract and a worked example.
#' @param n_draws Number of draws per respondent for a mixed logit. Ignored for
#'   `model = "mnl"`. 200 is enough to see whether a design works; use more for
#'   results you intend to publish.
#' @param seed Optional integer. Sets the random seed before anything is
#'   simulated, so a script records how it was run. This also makes the parallel
#'   path reproducible, because `furrr` derives its per-run streams from the
#'   current state of the generator. Note that it changes the session's generator
#'   state, exactly as calling [set.seed()] yourself would.
#' @param resample Redraw which choice sets each respondent sees for every
#'   simulation run. Only relevant with `sets_per_resp`. `TRUE` by default,
#'   because the allocation is part of the data generating process: fix it and you
#'   measure the performance of one particular allocation rather than of the
#'   design. Ignored for blocked designs, where the blocks are fixed anyway.
#' @param utility_transform_type How the utility function you entered is
#'   transformed into the utility function mixl needs. `"exact"`, the default,
#'   matches parameter and variable names exactly as they appear in the dataset
#'   and in `bcoeff`. `"simple"` is the older approach, where parameters have to
#'   start with "b" and variables with "alt"; it is deprecated and will be
#'   removed.
#' @param mode `"parallel"` to spread the runs over several R processes,
#'   `"sequential"` to run them one after another in this process. Starting eight
#'   worker processes and shipping the data to them costs on the order of ten
#'   seconds, so parallel only pays off once the work itself takes longer than
#'   that. As a rule of thumb, use it when a sequential run would take more than
#'   about half a minute. On a small three-coefficient model, 8 workers were 1.35
#'   times faster at 200 runs of 1000 respondents and 1.6 times faster at 500 runs
#'   of 300 respondents, but slower than sequential at 200 runs of 300. Bigger
#'   models shift the balance further towards parallel, since the fixed startup
#'   cost stays the same. Time your own case rather than assuming.
#'
#'   Both modes are reproducible from a seed, but they do not produce the *same*
#'   numbers: parallel runs draw from separate L'Ecuyer streams rather than from
#'   this session's stream. Parallel results do not depend on how many workers you
#'   use, so a rerun on a different machine still reproduces. Do not compare a
#'   sequential run against a parallel one and expect them to agree run for run;
#'   compare their summaries, which converge as `no_sim` grows.
#' @param workers Number of parallel workers. Defaults to whatever `future`
#'   picks for this machine, which respects `SLURM_CPUS_PER_TASK` on a cluster.
#'   Ignored in sequential mode.
#' @param keep_models Keep the full estimated model object for every run. `TRUE`
#'   by default, so `result[[design]][[1]]$data` still gets you the first run's
#'   data. Each object carries its own copy of that data, which is what dominates
#'   memory in a long run: 60 runs of 300 respondents came to 23 MB with the models
#'   kept and 0.08 MB without. Set it to `FALSE` to keep only the coefficients,
#'   summary, power and convergence results, which is all `aggregateResults()`
#'   needs.
#' @param savefile Indicate a path if you want to store the results after each design simulation locally. This is useful in case you fear that your computer crashes
#' @return a list with all information on the run
#' @export
#'
#' @examples bcoeff <- list(
#'   basc = -1.2,
#'   basc2 = -1.4,
#'   baction = 0.1,
#'   badvisory = 0.4,
#'   bpartnertest = 0.3,
#'   bcomp = 0.02
#' )
#' ul <- list(
#'   u1 =
#'     list(
#'       v1 = V.1 ~ basc + baction * alt1.b + badvisory * alt1.c +
#'         bpartnertest * alt1.d + bcomp * alt1.p,
#'       v2 = V.2 ~ basc2 + baction * alt2.b + badvisory * alt2.c +
#'         bpartnertest * alt2.d + bcomp * alt2.p,
#'       v3 = V.3 ~ 0
#'     )
#' )
#'
#' sim_choice(
#'   designfile = system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE"),
#'   no_sim = 2,
#'   respondents = 144,
#'   u = ul,
#'   bcoeff = bcoeff,
#'   estimate = FALSE
#' )
#'
sim_choice <- function(designfile, no_sim = 10, respondents = 330, u,
                       designtype = NULL, destype = NULL, bcoeff,
                       decisiongroups = c(0, 1), manipulations = list(),
                       estimate = TRUE, chunks = 1,
                       model = c("mnl", "mixed"),
                       correlation = NULL,
                       estimator = "mixl",
                       n_draws = 200,
                       sets_per_resp = NULL,
                       sample_sets = c("balanced", "random", "with_replacement"),
                       resample = TRUE,
                       utility_transform_type = c("exact", "simple"),
                       mode = c("parallel", "sequential"),
                       preprocess_function = NULL,
                       savefile = NULL,
                       keep_models = TRUE,
                       keep_utilities = TRUE,
                       workers = NULL,
                       seed = NULL,
                       verbose = 1) {
  mode <- match.arg(mode)
  sample_sets <- match.arg(sample_sets)
  utility_transform_type <- match.arg(utility_transform_type)
  model <- match.arg(model)

  #################################################
  ########## Input Validation Test ###############
  #################################################

  no_sim <- check_count(no_sim, "no_sim")
  respondents <- check_count(respondents, "respondents")
  chunks <- check_count(chunks, "chunks")
  n_draws <- check_count(n_draws, "n_draws")
  verbose <- check_verbose(verbose)
  estimator_fun <- resolve_estimator(estimator)

  if (!is.null(seed)) {
    set.seed(check_count(seed, "seed", min = -.Machine$integer.max))
  }

  if (chunks > no_sim) {
    stop(
      "`chunks` is ", chunks, " but there are only ", no_sim, " run(s). Results ",
      "can be written to disk at most once per run, so `chunks` must not exceed ",
      "`no_sim`.",
      call. = FALSE
    )
  }

  if (utility_transform_type == "simple") {
    message(
      "utility_transform_type = \"simple\" is deprecated and will be removed. ",
      "Use \"exact\", which matches parameter and variable names exactly instead ",
      "of requiring them to start with \"b\" and \"alt\"."
    )
  }

  ## make bcoeff clean
  check_bcoeff_list(bcoeff)
  bcoeff_result <- modify_bcoeff_names(bcoeff, verbose = verbose)
  bcoeff <- bcoeff_result$bcoeff
  bcoeff_lookup <- bcoeff_result$bcoeff_lookup

  ### make utility function clean
  check_utility_list(u)
  u <- rename_utility_coefficients(u, bcoeff_lookup)
  check_decisiongroups(decisiongroups, u)

  ## Compile once here rather than once per run. The compiled functions take the
  ## coefficient environment as an argument, so they stay valid across runs and
  ## across parallel workers.
  u <- precompile_utility(u)

  vmsg_print(verbose, 2, "\nUtility function used in simulation (true utility):", strip_compiled(u))

  #### Read in the design file and set core variables ####

  design <- readdesign(
    design = designfile, designtype = designtype, destype = destype,
    verbose = verbose
  )

  designname <- clean_design_name(designfile)

  ## Say so before spending an hour on a design that cannot identify the model.
  ## A collinear design still converges and still fills in the summary table, so
  ## nothing later would tell you.
  diagnosis <- tryCatch(
    check_design(design, u = strip_compiled(u), bcoeff = bcoeff),
    error = function(e) NULL
  )
  if (!is.null(diagnosis) && isFALSE(diagnosis$identified)) {
    warning(
      "Design '", designname, "' cannot identify the model you asked for. ",
      paste(diagnosis$problems, collapse = " "),
      " Run check_design() on it for the details.",
      call. = FALSE
    )
  }

  ## Whether the choice sets shown to a respondent are redrawn every run. With a
  ## blocked design there is nothing to redraw.
  redraw_each_run <- !is.null(sets_per_resp) && isTRUE(resample)

  build_data <- function() {
    createDataset(design, respondents,
      sets_per_resp = sets_per_resp,
      sample_sets = sample_sets
    )
  }
  fixed_data <- if (redraw_each_run) NULL else build_data()

  simulate_one <- function(i) {
    dat <- if (redraw_each_run) build_data() else fixed_data
    simulate_choices(
      data = dat, utility = u, bcoeff = bcoeff,
      decisiongroups = decisiongroups, manipulations = manipulations,
      preprocess_function = preprocess_function, correlation = correlation,
      keep_utilities = keep_utilities,
      verbose = if (i == 1L) verbose else min(verbose, 2)
    )
  }

  ### function to store results
  savef <- function(object) {
    if (is.null(savefile)) {
      return(invisible(NULL))
    }
    save_dir <- dirname(savefile)
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
      vmsg(verbose, 2, "Directory created: ", save_dir)
    }
    target <- paste0(savefile, "_", designname, ".qs")
    qs2::qs_save(object, target)
    vmsg(verbose, 2, "Output saved to: ", target)
  }

  ## ---- simulate only -------------------------------------------------------

  if (!isTRUE(estimate)) {
    sim_data <- switchmap(seq_len(no_sim), simulate_one, mode = mode, workers = workers)
    savef(sim_data)
    return(sim_data)
  }

  ## ---- simulate and estimate -----------------------------------------------

  ## The first run doubles as the template mixl needs to check the utility script
  ## against the data, so no simulation is wasted.
  first_data <- simulate_one(1L)

  mnl_U <- transform_utility(u, bcoeff, first_data, utility_transform_type)
  vmsg(verbose, 2, "\nTransformed utility function (type: ", utility_transform_type, "):\n", mnl_U)

  mixed_U <- NULL
  random_params <- character(0)
  if (identical(model, "mixed")) {
    random_params <- check_mixed_supported(bcoeff)
    mixed_U <- build_mixed_script(mnl_U, bcoeff, random_params)
    vmsg(verbose, 2, "\nMixed logit utility function:\n", mixed_U)
    vmsg(verbose, 1, "Estimating a mixed logit with ", n_draws, " draws, random: ",
      and_list(random_params), ".")
  }

  ## Everything the estimator needs, except the parts that depend on the run.
  base_spec <- build_estimator_spec(
    u = u, bcoeff = bcoeff, script = mnl_U, mixed_script = mixed_U,
    random_params = random_params, model = model, n_draws = n_draws,
    n_alt = length(u[[1]]), verbose = verbose
  )

  ## Nothing holding an external pointer, such as a compiled mixl model, survives
  ## being sent to another process, so each worker builds its own the first time it
  ## is asked and caches it after that. See ?estimators.
  estimate_one <- function(data, run = 1L) {
    res <- estimator_fun(data, spec_for_data(base_spec, data))
    check_estimator_result(res, run)
  }

  run_one <- function(i) estimate_one(simulate_one(i), i)

  timer <- new_timer()

  if (chunks > 1) {
    output <- run_in_chunks(
      no_sim = no_sim, chunks = chunks, first = estimate_one(first_data, 1L),
      run_one = run_one, mode = mode, workers = workers,
      designname = designname, verbose = verbose
    )
  } else {
    output <- c(
      list(estimate_one(first_data, 1L)),
      if (no_sim > 1) {
        switchmap(seq_len(no_sim)[-1], run_one, mode = mode, workers = workers)
      } else {
        list()
      }
    )
  }

  vmsg(verbose, 3, timer("estimation"))

  ## ---- collect results -----------------------------------------------------

  results <- collect_results(
    output = output, designfile = designfile, designname = designname,
    no_sim = no_sim, respondents = respondents, bcoeff = bcoeff,
    keep_models = keep_models, model = model,
    scale_params = if (length(random_params)) paste0("sigma_", random_params) else character(0)
  )

  vmsg_print(verbose, 1, "\nSummary table:", format_table(results[["summary"]]))
  vmsg_print(verbose, 1, "\nPower results:", results[["power"]])
  if (results[["convergence"]][["failed"]] > 0) {
    warning(
      results[["convergence"]][["failed"]], " of ", no_sim,
      " model(s) did not converge for design '", designname,
      "'. They are excluded from the summary and power results. Check ",
      "`$convergence` in the output, and consider more respondents or ",
      "different starting values.",
      call. = FALSE
    )
  }

  savef(results)
  results
}


#' Rewrite the coefficient names inside the utility formulas
#'
#' `bcoeff` names lose their dots and underscores so mixl can use them. The same
#' substitution has to happen inside the formulas.
#' @noRd
rename_utility_coefficients <- function(u, bcoeff_lookup) {
  if (identical(bcoeff_lookup$original, bcoeff_lookup$modified)) {
    return(u)
  }

  replacements <- stats::setNames(
    bcoeff_lookup$modified,
    paste0("(?<![a-zA-Z0-9._])", bcoeff_lookup$original, "(?![a-zA-Z0-9._])")
  )

  purrr::map(u, function(utility_group) {
    purrr::map(utility_group, function(utility) {
      rhs_string <- paste(deparse(formula.tools::rhs(utility)), collapse = " ")
      modified_rhs <- stringr::str_replace_all(rhs_string, replacements)
      stats::formula(paste(
        as.character(formula.tools::lhs(utility)), "~", modified_rhs
      ))
    })
  })
}

#' Drop the compiled attribute so printing a utility list stays readable
#' @noRd
strip_compiled <- function(u) {
  attr(u, "compiled") <- NULL
  u
}

#' Turn a design file path into a short, safe name
#'
#' Only the extension is removed, from the file name only. Earlier versions
#' stripped underscores from the whole path, which invented directories that did
#' not exist, and left unescaped dots in the pattern so `xngd_1.ngd` became `1`.
#' Underscores are kept now that results are no longer keyed by splitting names on
#' them.
#' @noRd
clean_design_name <- function(designfile) {
  sub("\\.(ngd|rds|RDS|Rds|ngd)$", "", basename(designfile))
}


#' Map over runs, in parallel or not, restoring the caller's future plan
#'
#' The old version reset the plan to sequential on exit, which silently threw
#' away a plan the user had set up, for instance a cluster plan on an HPC.
#' @noRd
switchmap <- function(.x, .f, mode, workers = NULL, ..., .progress = FALSE) {
  if (length(.x) == 0) {
    return(list())
  }

  if (identical(mode, "sequential")) {
    return(purrr::map(.x, .f, ..., .progress = .progress))
  }

  oldplan <- future::plan()
  on.exit(future::plan(oldplan), add = TRUE)

  if (!is.null(workers)) {
    future::plan("multisession", workers = workers)
  } else if (inherits(oldplan, "sequential")) {
    future::plan("multisession")
  }

  furrr::future_map(.x, .f, ..., .options = furrr::furrr_options(seed = TRUE))
}


#' Per-process cache of compiled mixl models
#'
#' `mixl::specify_model()` compiles C++ and returns an external pointer, which
#' cannot be serialised to another process. Each worker therefore builds the model
#' the first time it needs it and reuses it for every later run.
#' @noRd
.mixl_cache <- new.env(parent = emptyenv())

#' How many compiled models to keep per process
#'
#' A mixed logit run needs two, the multinomial warm start and the mixed model
#' itself. The objects are large, so nothing older is kept.
#' @noRd
.mixl_cache_size <- 2L

#' @noRd
model_spec_for <- function(script, data) {
  key <- paste(script, nrow(data), paste(names(data), collapse = ","), sep = "|")
  hit <- .mixl_cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }

  spec <- mixl::specify_model(utility_script = script, dataset = data, disable_multicore = TRUE)
  bits <- list(
    spec = spec,
    start = stats::setNames(rep(0, length(spec$beta_names)), spec$beta_names),
    availabilities = design_availabilities(data, spec$num_utility_functions)
  )

  ## Evict the oldest entries: designs and sample sizes change between calls and
  ## the cached objects are large.
  existing <- ls(.mixl_cache)
  if (length(existing) >= .mixl_cache_size) {
    ages <- vapply(existing, function(k) .mixl_cache[[k]]$stamp %||% 0, numeric(1))
    rm(list = existing[order(ages)][seq_len(length(existing) - .mixl_cache_size + 1L)],
       envir = .mixl_cache)
  }
  bits$stamp <- as.numeric(Sys.time())
  assign(key, bits, envir = .mixl_cache)
  bits
}

#' Estimate one model, hiding the optimiser trace unless asked for it
#'
#' mixl calls maxLik with print.level = 4 hard-coded, so the only way to keep the
#' console usable is to capture the output.
#' @noRd
quiet_estimate <- function(bits, data, verbose, n_draws = NULL) {
  run <- function() {
    args <- list(
      model_spec = bits$spec,
      start_values = bits$start,
      availabilities = bits$availabilities,
      data = data
    )
    ## Only a mixed model takes draws, and passing nDraws to a model without any
    ## draw dimensions is an error in mixl.
    if (!is.null(n_draws) && isTRUE(bits$spec$is_mixed)) args$nDraws <- n_draws
    do.call(mixl::estimate, args)
  }
  if (verbose >= 3) {
    return(suppressMessages(run()))
  }
  fitted <- NULL
  utils::capture.output(suppressMessages(fitted <- run()))
  fitted
}


#' Estimate the runs in batches, writing each batch to disk as it finishes
#' @noRd
run_in_chunks <- function(no_sim, chunks, first, run_one, mode, workers,
                          designname, verbose) {
  remaining <- seq_len(no_sim)[-1]
  batches <- if (length(remaining) == 0) {
    list()
  } else {
    split(remaining, cut(seq_along(remaining), chunks, labels = FALSE))
  }

  scratch <- file.path(tempdir(), paste0("simulateDCE_", designname, "_chunk_"))
  files <- character(0)
  on.exit(unlink(files), add = TRUE)

  ## the first run is already done and belongs to batch 1
  carry <- list(first)

  for (i in seq_along(batches)) {
    timer <- new_timer()
    part <- c(carry, switchmap(batches[[i]], run_one, mode = mode, workers = workers))
    carry <- list()

    f <- paste0(scratch, i, ".qs")
    qs2::qs_save(part, f)
    files <- c(files, f)
    rm(part)
    gc()

    vmsg(verbose, 2, sprintf(
      "Chunk %d of %d finished (runs %d to %d). %s",
      i, length(batches), min(batches[[i]]), max(batches[[i]]), timer("chunk")
    ))
  }

  if (length(files) == 0) {
    return(carry)
  }

  out <- list()
  for (f in files) {
    out <- c(out, qs2::qs_read(f))
    unlink(f)
  }
  out
}


#' Pull coefficients, summaries, power and convergence out of the estimated models
#' @noRd
collect_results <- function(output, designfile, designname, no_sim, respondents,
                            bcoeff, keep_models = TRUE, model = "mnl",
                            scale_params = character(0)) {
  reported_ok <- vapply(output, function(r) isTRUE(r$converged), logical(1))

  tables <- lapply(output, function(r) {
    if (length(r$coefficients) == 0) {
      return(NULL)
    }
    nms <- names(r$coefficients)
    data.frame(
      est = unname(r$coefficients),
      rob_pval0 = unname(r$pvalues[nms]),
      ## an estimator need not report standard errors; without them coverage is NA
      se = if (is.null(r$se)) NA_real_ else unname(r$se[nms]),
      row.names = nms
    )
  })

  ## A separated logit walks off to a huge coefficient with a huge standard error
  ## and still reports success, so the convergence flag alone is not enough. Such a
  ## run would otherwise drag the mean of every summary with it.
  usable <- vapply(tables, function(tab) {
    !is.null(tab) && nrow(tab) > 0 && all(is.finite(tab$est))
  }, logical(1))

  ok <- reported_ok & usable

  if (!any(ok)) {
    stop(
      "None of the ", no_sim, " model(s) for design '", designname,
      "' produced usable estimates",
      if (any(!reported_ok)) {
        paste0(
          " (", sum(!reported_ok), " did not converge",
          if (any(reported_ok & !usable)) {
            paste0(", ", sum(reported_ok & !usable), " returned non-finite values")
          } else {
            ""
          },
          ")"
        )
      } else {
        " (all returned non-finite values)"
      },
      ". The usual cause is a design that cannot identify the model: with two ",
      "alternatives, check that the matrix of attribute differences has full ",
      "column rank, and that no attribute is a linear function of the others. ",
      "More respondents will not help if that is the problem.",
      call. = FALSE
    )
  }

  coefs <- purrr::map(which(ok), function(i) {
    tables[[i]] %>%
      tibble::rownames_to_column() %>%
      tidyr::pivot_wider(
        names_from = "rowname",
        values_from = c("est", "rob_pval0", "se")
      )
  }) %>%
    dplyr::bind_rows(.id = "run")

  ## A mixed logit's scale parameters have no identified sign: the likelihood sees
  ## only sigma * draw, and the draws are symmetric about zero, so +sigma and
  ## -sigma fit identically. The optimiser lands on either. Reporting the absolute
  ## value is what every mixed logit package does; leaving the sign in would drag
  ## the mean of the summary towards zero for no reason.
  for (sp in intersect(paste0("est_", scale_params), names(coefs))) {
    coefs[[sp]] <- abs(coefs[[sp]])
  }

  ## The numbered slots hold the backend's own fitted object, so
  ## result[[design]][[1]]$data still works with the default estimator.
  results <- if (isTRUE(keep_models)) lapply(output, function(r) r$model) else list()

  results[["coefs"]] <- coefs
  results[["summary"]] <- add_accuracy(
    describe_coefs(coefs[, -1, drop = FALSE]),
    coefs, bcoeff_table(bcoeff, model)
  )

  pvals <- coefs %>% dplyr::select(dplyr::starts_with("rob_pval0"))
  results[["power"]] <- joint_power(pvals)
  results[["power_by_par"]] <- power_by_parameter(pvals)

  results[["convergence"]] <- list(
    runs = no_sim,
    converged = sum(ok),
    failed = sum(!ok),
    not_converged = sum(!reported_ok),
    unusable_estimates = sum(reported_ok & !usable)
  )

  results[["metainfo"]] <- c(Path = designfile, NoSim = no_sim, NoResp = respondents)

  ## Persist the information aggregateResults() needs so that saved files are
  ## self-describing. This allows aggregateResults(fromfolder = ) to merge
  ## results from independent runs (e.g. designs simulated at different times).
  results[["bcoeff"]] <- bcoeff
  results[["designname"]] <- designname
  results[["model"]] <- model

  results
}

#' Share of runs in which every coefficient is significant at 5 percent
#'
#' Always reports both outcomes, so reading `power["FALSE"]` does not give NA when
#' every run happened to be significant.
#' @noRd
joint_power <- function(pvals) {
  if (nrow(pvals) == 0) {
    return(stats::setNames(c(0, 0), c("FALSE", "TRUE")) %>% as.table())
  }
  all_sig <- apply(pvals, 1, function(x) all(x < 0.05))
  all_sig[is.na(all_sig)] <- FALSE
  out <- table(factor(all_sig, levels = c(FALSE, TRUE)))
  100 * out / nrow(pvals)
}

#' Share of runs in which each single coefficient is significant at 5 percent
#' @noRd
power_by_parameter <- function(pvals) {
  if (ncol(pvals) == 0 || nrow(pvals) == 0) {
    return(numeric(0))
  }
  out <- vapply(pvals, function(x) 100 * mean(x < 0.05, na.rm = TRUE), numeric(1))
  stats::setNames(out, sub("^rob_pval0_", "", names(out)))
}

#' Describe the estimated coefficients across runs
#'
#' Replaces psych::describe(fast = TRUE), which was the only reason for that
#' dependency. Same columns, same row names.
#' @noRd
describe_coefs <- function(x) {
  x <- as.data.frame(x)
  if (ncol(x) == 0) {
    return(data.frame())
  }
  stats_mat <- t(vapply(x, function(v) {
    v <- v[is.finite(v)]
    n <- length(v)
    sdv <- if (n > 1) stats::sd(v) else NA_real_
    c(
      n = n,
      mean = if (n > 0) mean(v) else NA_real_,
      ## The median is here because one badly behaved run can move the mean a long
      ## way. If mean and median disagree, look at min and max.
      median = if (n > 0) stats::median(v) else NA_real_,
      sd = sdv,
      min = if (n > 0) min(v) else NA_real_,
      max = if (n > 0) max(v) else NA_real_,
      range = if (n > 0) max(v) - min(v) else NA_real_,
      se = if (n > 0 && !is.na(sdv)) sdv / sqrt(n) else NA_real_
    )
  }, numeric(8)))
  as.data.frame(stats_mat)
}

#' Render a small numeric table for the console
#'
#' Replaces kableExtra::kable(), which was the only reason for that dependency.
#' @noRd
format_table <- function(x, digits = 3) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(x)
  }
  out <- x
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(v) round(v, digits))
  out
}


#' Translate the user's utility functions into a mixl utility script
#' @noRd
transform_utility <- function(u, bcoeff, database, type) {
  utility_strings <- paste(
    purrr::map_chr(u[[1]], as.character, keep.source.attr = TRUE),
    collapse = "", ";"
  )

  if (identical(type, "simple")) {
    return(stringr::str_replace_all(
      utility_strings,
      c(
        "priors\\[\"" = "", "\"\\]" = "", "~" = "=", "\\." = "_",
        " b" = " @b", "V_" = "U_", " alt" = " $alt"
      )
    ))
  }

  ## Everything in the dataset that is a genuine variable. Coefficient names are
  ## excluded because with random parameters they are columns of draws too, and
  ## marking them as data would produce "@$bcost" and leave mixl with no free
  ## parameters at all.
  relevant_database_vars <- setdiff(
    names(database),
    c(
      grep(utility_col_pattern(), names(database), value = TRUE),
      "CHOICE", names(bcoeff)
    )
  )

  utility_strings %>%
    stringr::str_replace_all(stats::setNames(
      paste0("@", names(bcoeff)),
      paste0("(?<![._a-zA-Z0-9])", names(bcoeff), "(?![._a-zA-Z0-9-])")
    )) %>%
    stringr::str_replace_all(c(
      `priors\\["` = "",
      `"\\]` = "",
      `~` = "=",
      `\\.` = "_",
      `V_` = "U_"
    )) %>%
    stringr::str_replace_all(stats::setNames(
      paste0("$", relevant_database_vars),
      paste0("(?<![._a-zA-Z0-9])", relevant_database_vars, "(?![._a-zA-Z0-9-])")
    )) %>%
    stringr::str_replace_all(c(`@@` = "@", "\\$\\$" = "$"))
}


#' Validate the verbose argument
#' @noRd
check_verbose <- function(verbose) {
  if (!is.numeric(verbose) || length(verbose) != 1L || is.na(verbose) ||
    verbose < 0 || verbose > 3 || verbose != trunc(verbose)) {
    stop(
      "`verbose` must be one of 0, 1, 2 or 3, not ", describe_value(verbose),
      ". 0 is silent, 1 reports key results, 2 adds progress, 3 adds timings.",
      call. = FALSE
    )
  }
  as.integer(verbose)
}


#' Attach bias, root mean squared error and coverage to a summary table
#'
#' Bias and RMSE need only the estimates and the truth. Coverage, the share of runs
#' whose 95 percent interval contains the true value, needs the standard errors too,
#' and is `NA` when the estimator did not report any. Coverage is the diagnostic
#' that catches a design whose intervals are too narrow, which power alone will not
#' show.
#' @noRd
add_accuracy <- function(summary_tab, coefs, truth) {
  n <- nrow(summary_tab)
  summary_tab$bias <- NA_real_
  summary_tab$rmse <- NA_real_
  summary_tab$coverage <- NA_real_

  if (n == 0 || nrow(truth) == 0) {
    return(summary_tab)
  }

  for (i in seq_len(n)) {
    row <- rownames(summary_tab)[i]
    if (!startsWith(row, "est_")) next

    par <- sub("^est_", "", row)
    hit <- truth$parname == par
    if (!any(hit)) next

    true_value <- truth$truepar[hit][1]
    est <- coefs[[row]]
    est <- est[is.finite(est)]
    if (length(est) == 0) next

    summary_tab$bias[i] <- mean(est) - true_value
    summary_tab$rmse[i] <- sqrt(mean((est - true_value)^2))

    se_col <- paste0("se_", par)
    if (se_col %in% names(coefs)) {
      se <- coefs[[se_col]]
      keep <- is.finite(coefs[[row]]) & is.finite(se) & se > 0
      if (any(keep)) {
        half <- stats::qnorm(0.975) * se[keep]
        inside <- abs(coefs[[row]][keep] - true_value) <= half
        summary_tab$coverage[i] <- 100 * mean(inside)
      }
    }
  }

  summary_tab
}
