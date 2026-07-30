library(formula.tools)

## All the argument-validation and behaviour tests are wrapped in one function so
## they can be run against several designs. Everything runs sequentially: see
## setup.R for why.

comprehensive_design_test <- function(nosim, resps, designtype, designpath, ul, bcoeff,
                                     decisiongroups = c(0, 1)) {
  ## defaults that any single test may override, without tripping over a
  ## duplicated argument name
  sa <- function(...) {
    args <- utils::modifyList(
      list(
        nosim = nosim, resps = resps, designtype = designtype,
        designpath = designpath, mode = "sequential", verbose = 0
      ),
      list(...)
    )
    do.call(sim_all, args)
  }

  test_that("u is not a list of lists", {
    expect_error(
      sa(u = data.frame(u = " alp"), bcoeff = bcoeff),
      "must be provided and must be a list containing at least one list"
    )
  })

  test_that("no value provided for  utility", {
    expect_error(
      sa(bcoeff = bcoeff),
      "must be provided and must be a list containing at least one list"
    )
  })

  test_that("wrong designtype", {
    expect_error(
      sim_all(
        nosim = nosim, resps = resps, designtype = "ng",
        designpath = designpath, u = ul, bcoeff = bcoeff,
        decisiongroups = decisiongroups, mode = "sequential", verbose = 0
      ),
      "must be one of"
    )
  })

  test_that("folder does not exist", {
    expect_error(
      sa(
        designpath = system.file("da/bullshit", package = "simulateDCE"),
        u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups
      ),
      "The folder where your designs are stored does not exist"
    )
  })

  test_that("seed setting makes code reproducible", {
    skip_on_cran()
    set.seed(3333)
    result1 <- sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)
    set.seed(3333)
    result2 <- sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    expect_identical(result1[["summaryall"]], result2[["summaryall"]])
    expect_identical(result1[["estimates"]], result2[["estimates"]])
  })

  test_that("No seed setting makes code results different", {
    skip_on_cran()
    result1 <- sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)
    result2 <- sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    expect_failure(expect_identical(result1[["summaryall"]], result2[["summaryall"]]))
  })

  test_that("exact and simple produce same results", {
    skip_on_cran()
    set.seed(3333)
    result1 <- suppressMessages(sa(
      u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups,
      utility_transform_type = "simple"
    ))
    set.seed(3333)
    result2 <- sa(
      u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups,
      utility_transform_type = "exact"
    )

    expect_identical(result1[["summaryall"]], result2[["summaryall"]])
  })

  test_that("Length of utility functions matches number of decision groups", {
    badbcoeff <- list(
      basc = 0.2, bcow = 0.3, badv = 0.3, bvet = 0.3, bfar = 0.3,
      bmet = 0.3, bbon = 0.3, bbon2 = 1.9, basc2 = 2
    )

    badlist <- list(
      u1 = list(
        v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u2 = list(
        v1 = V.1 ~ bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bbon * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u3 = list(
        v1 = V.1 ~ bbon2 * alt1.bon,
        v2 = V.2 ~ bbon2 * alt2.bon,
        v3 = V.3 ~ basc
      ),
      u4 = list(
        v1 = V.1 ~ basc2 + bcow * alt1.cow + badv * alt1.adv + bvet * alt1.vet + bfar * alt1.far + bmet * alt1.met + bbon * alt1.bon,
        v2 = V.2 ~ bcow * alt2.cow + badv * alt2.adv + bvet * alt2.vet + bfar * alt2.far + bmet * alt2.met + bbon * alt2.bon,
        v3 = V.3 ~ basc
      )
    )
    baddecisiongroups <- c(0, 0.3, 0.6, 1)

    # 4 utility groups but only 3 decision groups
    expect_error(
      sa(u = badlist, bcoeff = badbcoeff, decisiongroups = baddecisiongroups),
      "decision group\\(s\\) but `utility` has 4"
    )

    expect_no_error(sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups))
  })

  ########### Additional Tests ##############

  test_that("bcoeff is provided", {
    expect_error(sa(u = ul), "`bcoeff` is required")
  })

  test_that("bcoeff contains valid values", {
    expect_error(sa(u = ul, bcoeff = list(bsq = "invalid")))
  })

  test_that("bcoeff is a list", {
    expect_error(sa(u = ul, bcoeff = "not a list"), "must be a list")
  })

  test_that("B coefficients in the utility functions dont match those in the bcoeff list", {
    expect_error(
      suppressWarnings(sa(u = ul, bcoeff = list(bWRONG = 0.00))),
      "appear in `u` but not in `bcoeff`"
    )
  })

  test_that("Utility functions are valid", {
    expect_no_error(eval(ul$u1$v1))
    expect_no_error(eval(ul$u1$v2))
  })

  test_that("Design path must be a valid directory", {
    expect_error(sa(designpath = 123, u = ul, bcoeff = bcoeff), "single folder path")
    expect_error(sa(designpath = "/nonexistent/path", u = ul, bcoeff = bcoeff), "does not exist")
    expect_error(sa(designpath = "path/to/a/file.txt", u = ul, bcoeff = bcoeff), "does not exist")
  })

  test_that("Resps must be an integer", {
    expect_error(
      sim_all(
        nosim = nosim, designtype = designtype, designpath = designpath,
        u = ul, bcoeff = bcoeff, mode = "sequential", verbose = 0
      ),
      "`resps` is required"
    )
    expect_error(sa(resps = "abc", u = ul, bcoeff = bcoeff), "whole number")
    expect_error(sa(resps = 1.5, u = ul, bcoeff = bcoeff), "whole number")
  })

  test_that("Function exists in simulateDCE", {
    expect_true("sim_all" %in% ls("package:simulateDCE"))
  })

  test_that("Simulation results are reasonable", {
    skip_on_cran()
    result1 <- sa(u = ul, bcoeff = bcoeff, decisiongroups = decisiongroups)

    designs <- tools::file_path_sans_ext(list.files(designpath, full.names = FALSE))

    ## A different summary table is produced for each design, so loop over them
    for (design in designs) {
      summaryTable <- result1[[design]][["summary"]]
      expect_false(is.null(summaryTable), info = design)

      ## look at each row holding an estimated coefficient
      for (row_name in rownames(summaryTable)) {
        if (!startsWith(row_name, "est_")) next

        betaCoeff <- sub("^est_", "", row_name)
        meanBeta <- summaryTable[row_name, "mean"]
        inputBeta <- spec_mean(bcoeff[[betaCoeff]], betaCoeff)

        expect_true(
          betaCoeff %in% names(bcoeff),
          info = sprintf("est_%s is not a coefficient in bcoeff", betaCoeff)
        )
        expect_gt(meanBeta, inputBeta - 1)
        expect_lt(meanBeta, inputBeta + 1)
      }
    }
  })
}

###################
## FROM RBOOK #####
###################

designpath <- system.file("extdata", "Rbook", package = "simulateDCE")

notes <- "No Heuristics"

resps <- 40 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"

bcoeff <- list(
  bsq = 0.00,
  bredkite = -0.05,
  bdistance = 0.50,
  bcost = -0.05,
  bfarm2 = 0.25,
  bfarm3 = 0.50,
  bheight2 = 0.25,
  bheight3 = 0.50
)

destype <- "spdesign"


# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 + bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 = V.3 ~ bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 + bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
))

comprehensive_design_test(
  nosim = nosim, resps = resps, designtype = destype,
  designpath = designpath, ul = ul, bcoeff = bcoeff
)

#── random parameter tests for sim_all ────────────────────────────────────────

designpath_rbook <- system.file("extdata", "Rbook", package = "simulateDCE")

bcoeff_mixed <- list(
  bsq       = 0.00,
  bredkite  = list(dist = "normal", mean = -0.05, sd = 0.1),
  bdistance = 0.50,
  bcost     = list(dist = "neg_lognormal", meanlog = -3, sdlog = 0.3),
  bfarm2    = 0.25,
  bfarm3    = 0.50,
  bheight2  = 0.25,
  bheight3  = 0.50
)

ul_rbook <- ul

sa_mixed <- function(...) {
  sim_all(
    nosim = 2, resps = 20, designtype = "spdesign",
    designpath = designpath_rbook, u = ul_rbook,
    mode = "sequential", ...
  )
}

test_that("sim_all accepts mixed bcoeff without error", {
  expect_no_error(sa_mixed(bcoeff = bcoeff_mixed, estimate = FALSE, verbose = 0))
})

test_that("sim_all rejects bcoeff element that is a list without dist", {
  bad_bcoeff <- bcoeff_mixed
  bad_bcoeff$bredkite <- list(mean = -0.05, sd = 0.1) # missing dist
  expect_error(
    sa_mixed(bcoeff = bad_bcoeff, estimate = FALSE, verbose = 0),
    "dist"
  )
})

test_that("sim_all emits parameter summary message", {
  expect_message(
    sa_mixed(bcoeff = bcoeff_mixed, estimate = FALSE, verbose = 1),
    "Parameter specification"
  )
})

test_that("sim_all parameter message labels fixed and random correctly", {
  msgs <- testthat::capture_messages(
    sa_mixed(bcoeff = bcoeff_mixed, estimate = FALSE, verbose = 1)
  )
  spec <- grep("Parameter specification", msgs, value = TRUE)
  expect_length(spec, 1)
  expect_match(spec, "fixed")
  expect_match(spec, "normal")
  expect_match(spec, "neg_lognormal")
  # the implied moments are shown for the random ones
  expect_match(spec, "mean = ")
})

test_that("sim_all warns about coefficients that are never used", {
  extra <- c(bcoeff, list(bunused = 0.5))
  expect_warning(
    sim_all(
      nosim = 1, resps = 20, designtype = "spdesign",
      designpath = designpath_rbook, u = ul_rbook, bcoeff = extra,
      estimate = FALSE, mode = "sequential", verbose = 0
    ),
    "never used in `u`"
  )
})

#── argument plumbing ─────────────────────────────────────────────────────────

test_that("verbose must be 0, 1, 2 or 3", {
  expect_error(
    sa_mixed(bcoeff = bcoeff_mixed, estimate = FALSE, verbose = 9),
    "must be one of 0, 1, 2 or 3"
  )
})

test_that("chunks may not exceed nosim", {
  expect_error(
    sim_all(
      nosim = 2, resps = 20, designtype = "spdesign",
      designpath = designpath_rbook, u = ul_rbook, bcoeff = bcoeff,
      estimate = FALSE, chunks = 5, mode = "sequential", verbose = 0
    ),
    "at most once per run"
  )
})

test_that("arguments records the simulation settings, with the old misspelling kept", {
  res <- sim_all(
    nosim = 2, resps = 20, designtype = "spdesign",
    designpath = designpath_rbook, u = ul_rbook, bcoeff = bcoeff,
    estimate = FALSE, mode = "sequential", verbose = 0
  )
  expect_identical(res[["arguments"]], res[["arguements"]])
  expect_equal(res[["arguments"]][["Number Simulations"]], 2)
  expect_equal(res[["arguments"]][["Set sampling"]], "blocks")
})
