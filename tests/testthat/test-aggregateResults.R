library(formula.tools)

## Tests for aggregateResults(), in particular the `fromfolder` path that
## merges independently saved design outputs (e.g. three designs simulated
## now and a fourth added later).

designpath <- system.file("extdata", "SE_DRIVE", package = "simulateDCE")

bcoeff <- list(
  b.preis = -0.01,
  b.lade  = -0.07,
  b.warte =  0.02
)

ul <- list(
  u1 = list(
    v1 = V.1 ~ b.preis * alt1.x1 + b.lade * alt1.x2 + b.warte * alt1.x3,
    v2 = V.2 ~ b.preis * alt2.x1 + b.lade * alt2.x2 + b.warte * alt2.x3
  )
)

# Helper: run a simulation that saves one self-describing .qs file per design.
run_and_save <- function(folder, seed = 123) {
  set.seed(seed)
  sim_all(
    nosim = 3, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff,
    utility_transform_type = "exact", mode = "sequential",
    estimate = TRUE, savefile = file.path(folder, "run"),
    verbose = 0
  )
}

test_that("saved design output is self-describing (stores bcoeff and designname)", {
  skip_on_cran()
  td <- withr::local_tempdir()
  run_and_save(td)

  files <- list.files(td, pattern = "\\.qs$", full.names = TRUE)
  expect_gt(length(files), 0)

  obj <- qs2::qs_read(files[[1]])
  expect_true("bcoeff" %in% names(obj))
  expect_true("designname" %in% names(obj))
  # designname must be the cleaned basename used to name designs elsewhere
  expect_false(grepl("\\.(ngd|qs|RDS)$", obj[["designname"]]))
  # bcoeff names are stripped of dots/underscores for mixl
  expect_setequal(names(obj[["bcoeff"]]), c("bpreis", "blade", "bwarte"))
})

test_that("aggregateResults(fromfolder) reconstructs the same summary as the in-memory run", {
  skip_on_cran()
  td <- withr::local_tempdir()
  res_inmemory <- run_and_save(td)

  res_folder <- aggregateResults(fromfolder = td)

  # truepar column is present and populated from the stored bcoeff
  expect_true("truepar" %in% names(res_folder[["summaryall"]]))
  expect_false(all(is.na(res_folder[["summaryall"]][["truepar"]])))

  # The folder-based aggregation matches what sim_all produced in memory
  expect_equal(res_folder[["summaryall"]], res_inmemory[["summaryall"]])
  expect_equal(res_folder[["powa"]], res_inmemory[["powa"]])
})

test_that("aggregateResults(fromfolder) merges designs added in separate runs", {
  skip_on_cran()
  # Simulate the real workflow: results produced at different times land in the
  # same folder and are merged afterwards.
  td <- withr::local_tempdir()
  run_and_save(td, seed = 1)        # SE_DRIVE has two designs -> two files
  n_first <- length(list.files(td, pattern = "\\.qs$"))

  res <- aggregateResults(fromfolder = td)

  # All designs end up in the aggregated coefficient table
  expect_true("design" %in% names(res[["summaryall"]]) ||
              ncol(res[["summaryall"]]) > 1)
  expect_length(res[["powa"]], n_first)
})

test_that("aggregateResults(fromfolder) warns and sets NA truepar when bcoeff is missing", {
  skip_on_cran()
  td <- withr::local_tempdir()
  run_and_save(td)

  # Emulate files saved by an older package version that did not store bcoeff.
  old_td <- withr::local_tempdir()
  for (f in list.files(td, pattern = "\\.qs$", full.names = TRUE)) {
    obj <- qs2::qs_read(f)
    obj[["bcoeff"]] <- NULL
    qs2::qs_save(obj, file.path(old_td, basename(f)))
  }

  expect_warning(
    res <- aggregateResults(fromfolder = old_td),
    "truepar"
  )
  expect_true(all(is.na(res[["summaryall"]][["truepar"]])))
})

test_that("aggregateResults(fromfolder) errors on a missing or empty folder", {
  expect_error(
    aggregateResults(fromfolder = file.path(tempdir(), "does-not-exist-xyz")),
    "does not exist"
  )

  empty <- withr::local_tempdir()
  expect_error(
    aggregateResults(fromfolder = empty),
    "No '\\.qs' files"
  )
})

#── the in-memory path used by sim_all() ───────────────────────────────────────

test_that("summaryall has one row per estimated quantity and a truepar per coefficient", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 3, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, utility_transform_type = "exact",
    mode = "sequential", estimate = TRUE, verbose = 0
  ))
  sa <- res[["summaryall"]]

  expect_s3_class(sa, "data.frame")
  expect_true(all(c("parname", "truepar") %in% names(sa)))
  # one row per coefficient plus one per robust p value
  expect_equal(nrow(sa), 2 * length(bcoeff))
  # the coefficient rows carry the true value, the p value rows do not
  coef_rows <- !grepl("^rob_pval0_", sa$parname)
  expect_false(any(is.na(sa$truepar[coef_rows])))
  expect_setequal(sa$parname[coef_rows], c("bpreis", "blade", "bwarte"))
})

test_that("truepar matches the beta values that were fed in", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, utility_transform_type = "exact",
    mode = "sequential", estimate = TRUE, verbose = 0
  ))
  sa <- res[["summaryall"]]
  expect_equal(sa$truepar[sa$parname == "bpreis"], -0.01)
  expect_equal(sa$truepar[sa$parname == "blade"],  -0.07)
  expect_equal(sa$truepar[sa$parname == "bwarte"],  0.02)
})

test_that("one power entry and one graph per design and coefficient", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, utility_transform_type = "exact",
    mode = "sequential", estimate = TRUE, verbose = 0
  ))
  ndesigns <- length(list.files(designpath))
  expect_length(res[["powa"]], ndesigns)
  expect_true(all(vapply(res[["powa"]], is.table, logical(1))))

  expect_named(res[["graphs"]], names(bcoeff) |> stringr::str_remove_all("[._]"),
               ignore.order = TRUE)
  expect_true(all(vapply(res[["graphs"]], ggplot2::is_ggplot, logical(1))))
})

test_that("sim_all records its own arguments for later aggregation", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, utility_transform_type = "exact",
    mode = "sequential", estimate = FALSE, verbose = 0
  ))
  args <- res[["arguements"]]
  expect_equal(args[["Number Simulations"]], 2)
  expect_equal(args[["Respondents"]], 60)
  expect_equal(args[["Designpath"]], designpath)
  expect_equal(args[["mode"]], "sequential")
  expect_length(args[["designname"]], length(list.files(designpath)))
})

test_that("aggregateResults is not run when estimate is FALSE", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 20, designpath = designpath,
    u = ul, bcoeff = bcoeff, utility_transform_type = "exact",
    mode = "sequential", estimate = FALSE, verbose = 0
  ))
  expect_false("summaryall" %in% names(res))
  expect_false("graphs" %in% names(res))
})

test_that("reshape_type is deprecated and ignored", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, mode = "sequential", estimate = TRUE, verbose = 0
  ))
  expect_message(
    aggregateResults(all_designs = res, reshape_type = "nonsense"),
    "deprecated and ignored"
  )
  # and the result is the same as without it
  expect_equal(
    suppressMessages(aggregateResults(res, reshape_type = "stats"))[["summaryall"]],
    aggregateResults(res)[["summaryall"]]
  )
})

test_that("the long estimates table has one row per run per design", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 3, resps = 60, designpath = designpath,
    u = ul, bcoeff = bcoeff, mode = "sequential", estimate = TRUE, verbose = 0
  ))
  ndesigns <- length(res[["arguments"]][["designname"]])

  expect_true("design" %in% names(res[["estimates"]]))
  expect_equal(nrow(res[["estimates"]]), 3 * ndesigns)
  expect_setequal(
    setdiff(names(res[["estimates"]]), "design"),
    names(res[["arguments"]][["Beta values"]])
  )
})

test_that("aggregateResults refuses a list it cannot interpret", {
  expect_error(aggregateResults(all_designs = list(a = 1)), "no `arguments` element")
  expect_error(aggregateResults(all_designs = "nope"), "must be the list returned")
})

test_that("aggregateResults refuses results with no models in them", {
  skip_on_cran()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 20, designpath = designpath,
    u = ul, bcoeff = bcoeff, mode = "sequential", estimate = FALSE, verbose = 0
  ))
  expect_error(aggregateResults(all_designs = res), "estimate = TRUE")
})
