## One test per defect that has been fixed, asserting the behaviour we now want.
## The numbering matches the audit these came from, so a regression points
## straight back at what it broke.

design_rbook <- system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE")

ul_rbook <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bcost * alt2.cost,
  v3 = V.3 ~ bcost * alt3.cost
))

sim_rbook <- function(...) {
  suppressMessages(sim_all(
    designtype = "spdesign", designpath = dirname(design_rbook),
    u = ul_rbook, mode = "sequential", ...
  ))
}

# ── 1. a triangular parameter no longer breaks the startup summary ────────────

test_that("a triangular bcoeff spec runs and is reported with its bounds", {
  bc <- list(bsq = 0.1, bcost = list(dist = "triangular", min = -0.2, max = 0, mode = -0.05))
  msgs <- testthat::capture_messages(
    sim_all(
      nosim = 1, resps = 20, designtype = "spdesign",
      designpath = dirname(design_rbook), u = ul_rbook, bcoeff = bc,
      estimate = FALSE, mode = "sequential", verbose = 1
    )
  )
  spec_line <- grep("triangular", msgs, value = TRUE)
  expect_length(spec_line, 1)
  expect_match(spec_line, "min = -0.2")
  expect_match(spec_line, "max = 0")
  expect_match(spec_line, "mode = -0.05")
})

test_that("every supported distribution survives the startup summary", {
  bc <- list(
    bsq = 0.1,
    bcost = list(dist = "triangular", min = -0.2, max = 0, mode = -0.05)
  )
  for (spec in list(
    list(dist = "normal", mean = -1, sd = 0.5),
    list(dist = "lognormal", meanlog = 0, sdlog = 0.3),
    list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3),
    list(dist = "uniform", min = -1, max = 1),
    list(dist = "triangular", min = -1, max = 1, mode = 0),
    list(dist = "truncated_normal", mean = 0, sd = 1, min = -1, max = 1)
  )) {
    bc$bcost <- spec
    expect_no_error(
      suppressMessages(sim_all(
        nosim = 1, resps = 20, designtype = "spdesign",
        designpath = dirname(design_rbook), u = ul_rbook, bcoeff = bc,
        estimate = FALSE, mode = "sequential", verbose = 1
      ))
    )
  }
})

# ── 2. random parameters can be estimated ────────────────────────────────────

test_that("mixed bcoeff estimates without error and mixl sees free parameters", {
  skip_on_cran()
  bc <- list(bsq = 0.1, bcost = list(dist = "normal", mean = -0.05, sd = 0.02))
  res <- sim_rbook(nosim = 2, resps = 80, bcoeff = bc, estimate = TRUE, verbose = 0)

  expect_true("summaryall" %in% names(res))
  expect_equal(res[["convergence"]][["design1"]][["failed"]], 0)
})

test_that("the mixl utility script keeps coefficients as parameters, not variables", {
  bc <- list(bsq = 0.1, bcost = list(dist = "normal", mean = -0.05, sd = 0.02))
  dsn <- suppressMessages(readdesign(design_rbook, designtype = "spdesign"))
  db <- suppressMessages(simulate_choices(createDataset(dsn, 20), ul_rbook,
    bcoeff = bc, verbose = 0
  ))
  script <- transform_utility(ul_rbook, bc, db, "exact")

  expect_false(grepl("@$", script, fixed = TRUE))
  expect_match(script, "@bsq")
  expect_match(script, "@bcost")
  expect_match(script, "$alt1_sq", fixed = TRUE)
})

# ── 3. truepar reports the mean of the mixing distribution ───────────────────

test_that("truepar and truesd come from the distribution moments", {
  bc <- list(
    bfix = 0.4,
    bnorm = list(dist = "normal", mean = -0.5, sd = 0.2),
    btri = list(dist = "triangular", min = 0, max = 9, mode = 3)
  )
  tab <- bcoeff_table(bc)

  expect_equal(tab$parname, c("bfix", "bnorm", "btri"))
  expect_equal(tab$truepar, c(0.4, -0.5, 4))
  expect_equal(tab$truesd[1:2], c(0, 0.2))
  expect_type(tab$truepar, "double")
})

test_that("summaryall carries a numeric truepar for random parameters", {
  skip_on_cran()
  bc <- list(bsq = 0.1, bcost = list(dist = "neg_lognormal", meanlog = -3, sdlog = 0.3))
  res <- sim_rbook(nosim = 2, resps = 80, bcoeff = bc, estimate = TRUE, verbose = 0)
  sa <- res[["summaryall"]]

  expect_type(sa$truepar, "double")
  expect_equal(sa$truepar[sa$parname == "bcost"], spec_mean(bc$bcost))
  expect_equal(sa$truesd[sa$parname == "bcost"], spec_sd(bc$bcost))
})

# ── 4/5/6. createDataset and blocks ──────────────────────────────────────────

test_that("respondents need not be a multiple of the block count", {
  design <- data.frame(Choice.situation = rep(1:5, 2), Block = rep(1:2, each = 5), x = 1:10)
  res <- createDataset(design, respondents = 7)

  expect_equal(nrow(res), 7 * 5)
  expect_equal(as.vector(table(res$ID)), rep(5, 7))
  # the remainder lands on the first block
  expect_equal(as.vector(tapply(res$Block, res$ID, unique)), c(1, 2, 1, 2, 1, 2, 1))
})

test_that("blocks not numbered from 1 are still counted correctly", {
  design <- data.frame(Choice.situation = rep(1:4, 2), Block = rep(0:1, each = 4), x = 1:8)
  res <- createDataset(design, respondents = 10)

  expect_equal(as.vector(table(res$ID)), rep(4, 10))
  expect_true(all(tapply(res$Block, res$ID, function(x) length(unique(x))) == 1))
})

test_that("blocks labelled as a factor or as text work", {
  as_factor <- data.frame(
    Choice.situation = rep(1:4, 2),
    Block = factor(rep(1:2, each = 4)), x = 1:8
  )
  as_text <- data.frame(
    Choice.situation = rep(1:4, 2),
    Block = rep(c("a", "b"), each = 4), x = 1:8
  )
  expect_equal(as.vector(table(createDataset(as_factor, 4)$ID)), rep(4, 4))
  expect_equal(as.vector(table(createDataset(as_text, 4)$ID)), rep(4, 4))
})

test_that("unequal block sizes are rejected with an explanation", {
  design <- data.frame(Choice.situation = 1:7, Block = c(1, 1, 1, 1, 2, 2, 2), x = 1:7)
  expect_error(createDataset(design, 4), "different sizes")
})

test_that("a design without Choice.situation is rejected by name", {
  expect_error(
    createDataset(data.frame(cs = 1:4, a = 1:4), respondents = 2),
    "needs a `Choice.situation` column"
  )
})

# ── 7. design names come from the file name only ─────────────────────────────

test_that("the design name keeps the directory out of it and the extension off", {
  expect_equal(clean_design_name("/data/SE_DRIVE/effconstrsmall.ngd"), "effconstrsmall")
  expect_equal(clean_design_name("/data/SE_DRIVE/my_design.RDS"), "my_design")
  expect_equal(clean_design_name("dir/d1.rds"), "d1")
  # an unescaped dot used to eat the preceding characters
  expect_equal(clean_design_name("dir/xngd_1.ngd"), "xngd_1")
})

test_that("chunk scratch files go to the temp directory and are cleaned up", {
  skip_on_cran()
  before <- list.files(tempdir(), pattern = "simulateDCE_.*chunk")
  res <- sim_rbook(
    nosim = 4, resps = 60, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = TRUE, chunks = 2, verbose = 0
  )
  expect_equal(nrow(res[["design1"]][["coefs"]]), 4)
  expect_equal(list.files(tempdir(), pattern = "simulateDCE_.*chunk"), before)
  # nothing written next to the design
  expect_length(list.files(dirname(design_rbook), pattern = "_tmp_"), 0)
})

# ── 8. savefile reads back only its own files ───────────────────────────────

test_that("an unrelated file in the save folder is ignored", {
  skip_on_cran()
  td <- withr::local_tempdir()
  writeLines("notes", file.path(td, "aaa_readme.txt"))

  res <- sim_rbook(
    nosim = 1, resps = 20, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = FALSE, savefile = file.path(td, "run"), verbose = 0
  )
  expect_true("design1" %in% names(res))
  expect_true(file.exists(file.path(td, "run_design1.qs")))
})

# ── 9. a plain data frame design is recognised ──────────────────────────────

test_that("guessing a plain design matrix gives the same result as saying so", {
  guessed <- suppressMessages(readdesign(design_rbook))
  explicit <- suppressMessages(readdesign(design_rbook, designtype = "spdesign"))

  expect_identical(guessed, explicit)
  expect_equal(ncol(guessed), 17L)
  expect_message(readdesign(design_rbook), "matrix design")
})

test_that("an spdesign object and an idefix object are each detected", {
  sp <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")
  id <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")
  expect_message(readdesign(sp), "spdesign design")
  expect_message(readdesign(id), "idefix design")
  expect_identical(
    suppressMessages(readdesign(id)),
    suppressMessages(readdesign(id, designtype = "idefix"))
  )
})

# ── 10. design variables that look like utility columns ─────────────────────

test_that("a design variable containing 'U_' leaves CHOICE alone", {
  d <- data.frame(ID = rep(1:4, each = 2), price = rep(c(10, 20), 4), U_pay = 1:8)
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))
  res <- suppressMessages(simulate_choices(d, ut, bcoeff = list(bprice = -1), verbose = 0))

  expect_true(all(res$CHOICE %in% c(1L, 2L)))
  expect_equal(res$CHOICE, max.col(as.matrix(res[, c("U_1", "U_2")])))
  expect_equal(res$U_pay, 1:8)
})

test_that("a design column that is exactly a utility column name is rejected", {
  d <- data.frame(ID = rep(1:4, each = 2), price = rep(c(10, 20), 4), U_1 = 1:8)
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))
  expect_error(
    simulate_choices(d, ut, bcoeff = list(bprice = -1), verbose = 0),
    "reserved for the simulated"
  )
})

# ── 11/12. timings ─────────────────────────────────────────────────────────

tic_stack_size <- function() {
  pe <- get("tictoc_pkg_env", envir = asNamespace("tictoc"))
  get("size", envir = get(".tictoc", envir = pe))()
}

test_that("simulate_choices leaves no timers behind at any verbose level", {
  d <- data.frame(ID = rep(1:5, each = 2), price = rep(c(10, 20), 5))
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))

  for (v in 0:3) {
    tictoc::tic.clear()
    for (i in 1:3) {
      invisible(suppressMessages(
        simulate_choices(d, ut, bcoeff = list(bprice = -1), verbose = v)
      ))
    }
    expect_equal(tic_stack_size(), 0, info = paste("verbose =", v))
  }
})

test_that("sim_all reports its own total time", {
  skip_on_cran()
  tictoc::tic.clear()
  res <- sim_rbook(
    nosim = 2, resps = 20, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = FALSE, verbose = 1
  )
  expect_equal(res[["time"]][["msg"]], "total time for simulation and estimation")
  expect_equal(tic_stack_size(), 0)
})

test_that("verbose = 3 prints timings with a duration in them", {
  d <- data.frame(ID = rep(1:5, each = 2), price = rep(c(10, 20), 5))
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))
  msgs <- testthat::capture_messages(
    simulate_choices(d, ut, bcoeff = list(bprice = -1), verbose = 3)
  )
  expect_true(any(grepl("[0-9]+\\.[0-9]+ sec elapsed", msgs)))
})

# ── 13. a fixed spec without a value is an error ────────────────────────────

test_that("dist = 'fixed' without a value is refused by name", {
  expect_error(
    make_rand_params(list(bprice = list(dist = "fixed")), 5),
    "bprice"
  )
  expect_error(
    make_rand_params(list(bprice = list(dist = "fixed")), 5),
    "missing required argument\\(s\\): value"
  )
})

# ── 14. preprocess_function returning NULL ─────────────────────────────────

test_that("a preprocess_function returning NULL is simply skipped", {
  d <- data.frame(ID = rep(1:4, each = 2), price = rep(c(10, 20), 4))
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))
  res <- suppressMessages(
    simulate_choices(d, ut, bcoeff = list(bprice = -1),
      preprocess_function = function() NULL, verbose = 0
    )
  )
  expect_equal(nrow(res), nrow(d))
})

test_that("a preprocess_function returning the wrong thing says what is wrong", {
  d <- data.frame(ID = rep(1:4, each = 2), price = rep(c(10, 20), 4))
  ut <- list(u1 = list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0))

  expect_error(
    simulate_choices(d, ut, bcoeff = list(bprice = -1),
      preprocess_function = function() 42, verbose = 0
    ),
    "must return a data.frame"
  )
  expect_error(
    simulate_choices(d, ut, bcoeff = list(bprice = -1),
      preprocess_function = function() data.frame(x = 1), verbose = 0
    ),
    "no `ID` column"
  )
})

# ── 15. decision groups follow respondents ─────────────────────────────────

test_that("no respondent is split across two decision groups", {
  dsn <- suppressMessages(readdesign(design_rbook, designtype = "spdesign"))
  dd <- createDataset(dsn, 40) # 40 respondents x 10 sets
  ut <- list(
    g1 = list(v1 = V.1 ~ bsq * alt1.sq, v2 = V.2 ~ 0, v3 = V.3 ~ 0),
    g2 = list(v1 = V.1 ~ 2 * bsq * alt1.sq, v2 = V.2 ~ 0, v3 = V.3 ~ 0)
  )

  for (cut_at in c(0.5, 0.7, 0.75, 0.77, 0.9)) {
    res <- suppressMessages(simulate_choices(dd, ut,
      bcoeff = list(bsq = 0.1),
      decisiongroups = c(0, cut_at, 1), verbose = 0
    ))
    per_resp <- tapply(res$group, res$ID, function(x) length(unique(x)))
    expect_true(all(per_resp == 1), info = paste("cut at", cut_at))
  }
})

test_that("the share of respondents in each group follows decisiongroups", {
  d <- data.frame(ID = rep(1:100, each = 4), price = rep(c(10, 20), 200))
  ut <- rep(list(list(v1 = V.1 ~ bprice * price, v2 = V.2 ~ 0)), 3)
  names(ut) <- c("g1", "g2", "g3")

  res <- suppressMessages(simulate_choices(d, ut,
    bcoeff = list(bprice = -1),
    decisiongroups = c(0, 0.2, 0.5, 1), verbose = 0
  ))
  first_row <- res$group[match(unique(res$ID), res$ID)]
  expect_equal(as.vector(table(first_row)), c(20, 30, 50))
})

# ── 16. design names that prefix one another ───────────────────────────────

test_that("designs whose names prefix one another aggregate correctly", {
  skip_on_cran()
  td <- withr::local_tempdir()
  d <- toy_design(nsets = 8, blocks = 2)
  saveRDS(d, file.path(td, "eff.rds"))
  saveRDS(d, file.path(td, "effconstr.rds"))

  res <- suppressMessages(sim_all(
    nosim = 3, resps = 40, designtype = "spdesign", designpath = td,
    u = toy_utility(), bcoeff = toy_bcoeff(),
    estimate = TRUE, mode = "sequential", verbose = 0
  ))

  expect_setequal(unique(res[["estimates"]]$design), c("eff", "effconstr"))
  expect_setequal(names(res[["powa"]]), c("eff", "effconstr"))
  expect_false(anyNA(res[["summaryall"]]$truepar[
    !grepl("^rob_pval0_", res[["summaryall"]]$parname)
  ]))
})

test_that("design and coefficient names may contain underscores", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(toy_design(nsets = 8, blocks = 2), file.path(td, "my_design_v2.rds"))

  res <- suppressMessages(sim_all(
    nosim = 3, resps = 40, designtype = "spdesign", designpath = td,
    u = toy_utility(), bcoeff = toy_bcoeff(),
    estimate = TRUE, mode = "sequential", verbose = 0
  ))
  expect_equal(unique(res[["estimates"]]$design), "my_design_v2")
})

# ── 17. the caller's future plan survives ──────────────────────────────────

test_that("sim_all hands back the future plan it was given", {
  skip_on_cran()
  withr::defer(future::plan("sequential"))
  future::plan(future::multisession, workers = 2)
  expect_equal(future::nbrOfWorkers(), 2L)

  invisible(sim_rbook(
    nosim = 1, resps = 20, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = FALSE, verbose = 0
  ))
  expect_equal(future::nbrOfWorkers(), 2L)
})

test_that("sequential mode does not start any workers", {
  expect_true(inherits(future::plan(), "sequential"))
  invisible(sim_rbook(
    nosim = 1, resps = 20, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = FALSE, verbose = 0
  ))
  expect_true(inherits(future::plan(), "sequential"))
})

# ── 18. power keeps an honest denominator ──────────────────────────────────

test_that("the power table always carries both outcomes and sums to 100", {
  all_sig <- data.frame(a = c(0.01, 0.02), b = c(0.001, 0.04))
  none_sig <- data.frame(a = c(0.5, 0.6), b = c(0.7, 0.8))

  for (pv in list(all_sig, none_sig)) {
    power <- joint_power(pv)
    expect_named(power, c("FALSE", "TRUE"))
    expect_equal(sum(power), 100)
  }
  expect_equal(unname(joint_power(all_sig)["TRUE"]), 100)
  expect_equal(unname(joint_power(none_sig)["TRUE"]), 0)
})

test_that("an NA p-value counts as not significant rather than disappearing", {
  pv <- data.frame(a = c(0.01, 0.02, NA), b = c(0.001, 0.04, 0.01))
  power <- joint_power(pv)
  expect_equal(sum(power), 100)
  expect_equal(unname(power["TRUE"]), 200 / 3)
})

test_that("power is also reported per parameter", {
  pv <- data.frame(rob_pval0_a = c(0.01, 0.2), rob_pval0_b = c(0.01, 0.01))
  pbp <- power_by_parameter(pv)
  expect_named(pbp, c("a", "b"))
  expect_equal(unname(pbp), c(50, 100))
})

test_that("non-convergence is counted and reported", {
  skip_on_cran()
  res <- sim_rbook(
    nosim = 2, resps = 60, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = TRUE, verbose = 0
  )
  conv <- res[["design1"]][["convergence"]]
  expect_equal(conv$runs, 2)
  expect_equal(conv$converged + conv$failed, 2)
})

# ── 19. a folder that holds more than designs ─────────────────────────────

test_that("only files matching the pattern are treated as designs", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(toy_design(nsets = 8, blocks = 2), file.path(td, "toy.rds"))
  dir.create(file.path(td, "subfolder"))
  writeLines("hello", file.path(td, "README.md"))

  res <- suppressMessages(sim_all(
    nosim = 1, resps = 20, designtype = "spdesign", designpath = td,
    u = toy_utility(), bcoeff = toy_bcoeff(),
    estimate = FALSE, mode = "sequential", verbose = 0
  ))
  expect_equal(res[["arguments"]][["designname"]], "toy")
})

test_that("a folder with no matching designs says what is in it", {
  td <- withr::local_tempdir()
  writeLines("hello", file.path(td, "README.md"))
  expect_error(
    sim_all(
      nosim = 1, resps = 20, designpath = td,
      u = toy_utility(), bcoeff = toy_bcoeff(),
      estimate = FALSE, mode = "sequential", verbose = 0
    ),
    "No design files matching"
  )
  expect_error(
    sim_all(
      nosim = 1, resps = 20, designpath = td,
      u = toy_utility(), bcoeff = toy_bcoeff(),
      estimate = FALSE, mode = "sequential", verbose = 0
    ),
    "README.md"
  )
})

# ── plots are no longer drawn as a side effect ─────────────────────────────

test_that("aggregateResults returns plots instead of printing them", {
  skip_on_cran()
  pdf_before <- file.exists("Rplots.pdf")
  res <- sim_rbook(
    nosim = 2, resps = 60, bcoeff = list(bsq = 0.1, bcost = -0.05),
    estimate = TRUE, verbose = 0
  )
  expect_true(all(vapply(res[["graphs"]], ggplot2::is_ggplot, logical(1))))
  expect_equal(file.exists("Rplots.pdf"), pdf_before)
})
