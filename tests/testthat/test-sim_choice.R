## sim_choice() had no direct coverage: it was only reached through sim_all().
## These tests exercise it on its own, for both the simulate-only and the
## simulate-and-estimate paths.

design_rbook <- system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE")

ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bcost * alt2.cost + bredkite * alt2.redkite,
  v3 = V.3 ~ bcost * alt3.cost + bredkite * alt3.redkite
))

bcoeff <- list(bsq = 0.1, bcost = -0.05, bredkite = -0.05)

sc <- function(...) {
  suppressMessages(sim_choice(
    designfile = design_rbook, designtype = "spdesign",
    u = ul, bcoeff = bcoeff, mode = "sequential",
    utility_transform_type = "exact", verbose = 0, ...
  ))
}

# ── simulate only ─────────────────────────────────────────────────────────────

test_that("estimate = FALSE returns one data frame per run", {
  res <- sc(no_sim = 3, respondents = 20, estimate = FALSE)
  expect_length(res, 3)
  expect_true(all(vapply(res, is.data.frame, logical(1))))
})

test_that("each simulated run has respondents x sets-per-respondent rows", {
  design <- suppressMessages(readdesign(design_rbook, designtype = "spdesign"))
  setpp <- nrow(design) / length(unique(design$Block))

  res <- sc(no_sim = 2, respondents = 20, estimate = FALSE)
  expect_equal(nrow(res[[1]]), 20 * setpp)
  expect_equal(nrow(res[[2]]), 20 * setpp)
})

test_that("runs differ from each other", {
  set.seed(101)
  res <- sc(no_sim = 2, respondents = 20, estimate = FALSE)
  expect_false(identical(res[[1]]$CHOICE, res[[2]]$CHOICE))
})

test_that("set.seed makes sim_choice reproducible in sequential mode", {
  set.seed(202)
  a <- sc(no_sim = 2, respondents = 20, estimate = FALSE)
  set.seed(202)
  b <- sc(no_sim = 2, respondents = 20, estimate = FALSE)
  expect_identical(a, b)
})

# ── simulate and estimate ─────────────────────────────────────────────────────

test_that("estimate = TRUE returns the documented summary elements", {
  skip_on_cran()
  res <- sc(no_sim = 2, respondents = 60, estimate = TRUE)
  expect_true(all(c("summary", "coefs", "power", "metainfo", "bcoeff", "designname")
                  %in% names(res)))
})

test_that("coefs has one row per run and est_ columns per coefficient", {
  skip_on_cran()
  res <- sc(no_sim = 3, respondents = 60, estimate = TRUE)
  expect_equal(nrow(res[["coefs"]]), 3)
  expect_equal(
    sum(grepl("^est_", names(res[["coefs"]]))),
    length(bcoeff)
  )
})

test_that("metainfo records the design path, number of runs and respondents", {
  skip_on_cran()
  res <- sc(no_sim = 2, respondents = 60, estimate = TRUE)
  expect_equal(unname(res[["metainfo"]]["Path"]), design_rbook)
  expect_equal(unname(res[["metainfo"]]["NoSim"]), "2")
  expect_equal(unname(res[["metainfo"]]["NoResp"]), "60")
})

test_that("saved output is self-describing", {
  skip_on_cran()
  res <- sc(no_sim = 2, respondents = 60, estimate = TRUE)
  expect_equal(res[["designname"]], "design1")
  expect_setequal(names(res[["bcoeff"]]), names(bcoeff))
})

test_that("power is a proportion table over the runs", {
  skip_on_cran()
  res <- sc(no_sim = 3, respondents = 60, estimate = TRUE)
  expect_true(is.table(res[["power"]]))
  expect_equal(sum(res[["power"]]), 100)
})

# ── chunking ──────────────────────────────────────────────────────────────────

test_that("chunks > 1 yields the same number of estimated models as chunks = 1", {
  skip_on_cran()
  # copy the design into a temp dir so chunk scratch files never touch the
  # installed package directory
  td <- withr::local_tempdir()
  local_design <- file.path(td, "design1.RDS")
  file.copy(design_rbook, local_design)

  run <- function(chunks) {
    suppressMessages(sim_choice(
      designfile = local_design, designtype = "spdesign",
      no_sim = 4, respondents = 60, u = ul, bcoeff = bcoeff,
      estimate = TRUE, chunks = chunks, mode = "sequential",
      utility_transform_type = "exact", verbose = 0
    ))
  }

  set.seed(303)
  one <- run(1)
  set.seed(303)
  two <- run(2)

  expect_equal(nrow(one[["coefs"]]), nrow(two[["coefs"]]))
  expect_equal(one[["coefs"]], two[["coefs"]])
  # no scratch files left behind
  expect_length(list.files(td, pattern = "_tmp_"), 0)
})

test_that("chunks must be a positive integer", {
  expect_error(sc(no_sim = 2, respondents = 20, estimate = FALSE, chunks = 0),
               "`chunks` must be at least 1")
  expect_error(sc(no_sim = 2, respondents = 20, estimate = FALSE, chunks = 1.5),
               "whole number")
  expect_error(sc(no_sim = 2, respondents = 20, estimate = FALSE, chunks = "2"),
               "whole number")
})

# ── savefile ──────────────────────────────────────────────────────────────────

test_that("savefile writes one qs file named after the design", {
  td <- withr::local_tempdir()
  sc(no_sim = 2, respondents = 20, estimate = FALSE,
     savefile = file.path(td, "run"))

  files <- list.files(td, pattern = "\\.qs$")
  expect_equal(files, "run_design1.qs")
  expect_length(qs2::qs_read(file.path(td, files)), 2)
})

test_that("savefile creates the target directory if it does not exist", {
  td <- withr::local_tempdir()
  nested <- file.path(td, "a", "b")
  sc(no_sim = 2, respondents = 20, estimate = FALSE,
     savefile = file.path(nested, "run"))
  expect_true(dir.exists(nested))
  expect_length(list.files(nested, pattern = "\\.qs$"), 1)
})

# ── argument handling ─────────────────────────────────────────────────────────

test_that("utility_transform_type simple is deprecated but still works", {
  expect_message(
    sim_choice(designfile = design_rbook, designtype = "spdesign",
               no_sim = 1, respondents = 20, u = ul, bcoeff = bcoeff,
               estimate = FALSE, mode = "sequential",
               utility_transform_type = "simple", verbose = 0),
    "deprecated"
  )
})

test_that("an unknown utility_transform_type is rejected up front", {
  expect_error(
    suppressMessages(sim_choice(
      designfile = design_rbook, designtype = "spdesign",
      no_sim = 1, respondents = 60, u = ul, bcoeff = bcoeff,
      estimate = FALSE, mode = "sequential",
      utility_transform_type = "nonsense", verbose = 0
    )),
    "'arg' should be one of"
  )
})

test_that("mode must be parallel or sequential", {
  expect_error(sc(no_sim = 1, respondents = 20, estimate = FALSE, mode = "turbo"))
})

# ── verbose levels ────────────────────────────────────────────────────────────

test_that("verbose = 0 is silent", {
  expect_silent(suppressWarnings(sim_choice(
    designfile = design_rbook, designtype = "spdesign",
    no_sim = 1, respondents = 20, u = ul, bcoeff = bcoeff,
    estimate = FALSE, mode = "sequential",
    utility_transform_type = "exact", verbose = 0
  )))
})

test_that("verbose = 2 reports the true utility function", {
  expect_message(
    sim_choice(designfile = design_rbook, designtype = "spdesign",
               no_sim = 1, respondents = 20, u = ul, bcoeff = bcoeff,
               estimate = FALSE, mode = "sequential",
               utility_transform_type = "exact", verbose = 2),
    "true utility"
  )
})

test_that("verbose levels are cumulative", {
  say <- function(v) {
    length(suppressWarnings(testthat::capture_messages(
      sim_choice(designfile = design_rbook, designtype = "spdesign",
                 no_sim = 1, respondents = 20, u = ul, bcoeff = bcoeff,
                 estimate = FALSE, mode = "sequential",
                 utility_transform_type = "exact", verbose = v)
    )))
  }
  expect_equal(say(0), 0)
  expect_gt(say(2), say(1))
  expect_gt(say(3), say(2))
})

test_that("verbose = 3 reports the first rows of the simulated dataset", {
  expect_message(
    sim_choice(designfile = design_rbook, designtype = "spdesign",
               no_sim = 1, respondents = 20, u = ul, bcoeff = bcoeff,
               estimate = FALSE, mode = "sequential",
               utility_transform_type = "exact", verbose = 3),
    "First few observations"
  )
})
