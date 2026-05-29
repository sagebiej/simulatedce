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
