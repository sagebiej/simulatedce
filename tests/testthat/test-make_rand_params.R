# ── output structure ──────────────────────────────────────────────────────────

test_that("output is a data frame with n_resp rows", {
  bcoef <- list(b1 = list(dist = "normal", mean = 0, sd = 1))
  out <- make_rand_params(bcoef, 100)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 100)
})

test_that("ID defaults to 1:n_resp", {
  bcoef <- list(b1 = list(dist = "fixed", value = 1))
  out <- make_rand_params(bcoef, 50)
  expect_equal(out$ID, 1:50)
})

test_that("custom respondent_ids are used", {
  bcoef <- list(b1 = list(dist = "fixed", value = 1))
  ids <- c("A", "B", "C")
  out <- make_rand_params(bcoef, 3, respondent_ids = ids)
  expect_equal(out$ID, ids)
})

test_that("columns are ID plus parameter names", {
  bcoef <- list(bprice = list(dist = "normal", mean = 0, sd = 1),
                bqual  = list(dist = "fixed", value = 2))
  out <- make_rand_params(bcoef, 10)
  expect_equal(names(out), c("ID", "bprice", "bqual"))
})

# ── distributions ────────────────────────────────────────────────────────────

test_that("normal draws recover mean and sd (large n)", {
  set.seed(314)
  bcoef <- list(b = list(dist = "normal", mean = -0.5, sd = 0.2))
  out <- make_rand_params(bcoef, 10000)
  expect_equal(mean(out$b), -0.5, tolerance = 0.05)
  expect_equal(sd(out$b),    0.2, tolerance = 0.05)
})

test_that("lognormal draws are all positive", {
  set.seed(628)
  bcoef <- list(b = list(dist = "lognormal", meanlog = 0, sdlog = 0.5))
  out <- make_rand_params(bcoef, 1000)
  expect_true(all(out$b > 0))
})

test_that("neg_lognormal draws are all negative", {
  set.seed(971)
  bcoef <- list(b = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3))
  out <- make_rand_params(bcoef, 1000)
  expect_true(all(out$b < 0))
})

test_that("uniform draws lie within [min, max]", {
  set.seed(159)
  bcoef <- list(b = list(dist = "uniform", min = 2, max = 5))
  out <- make_rand_params(bcoef, 1000)
  expect_true(all(out$b >= 2))
  expect_true(all(out$b <= 5))
})

test_that("triangular draws lie within [min, max]", {
  set.seed(265)
  bcoef <- list(b = list(dist = "triangular", min = 0, max = 10, mode = 3))
  out <- make_rand_params(bcoef, 1000)
  expect_true(all(out$b >= 0))
  expect_true(all(out$b <= 10))
})

test_that("fixed gives identical values for all respondents", {
  bcoef <- list(b = list(dist = "fixed", value = 42))
  out <- make_rand_params(bcoef, 100)
  expect_true(all(out$b == 42))
})

# ── input validation ─────────────────────────────────────────────────────────

test_that("error when bcoef is unnamed", {
  expect_error(make_rand_params(list(list(dist = "normal", mean = 0, sd = 1)), 10),
               "fully named")
})

test_that("error for unknown distribution", {
  bcoef <- list(b = list(dist = "cauchy", location = 0, scale = 1))
  expect_error(make_rand_params(bcoef, 10), "Unknown distribution")
})

test_that("error when required moments are missing", {
  bcoef <- list(b = list(dist = "normal", mean = 0))
  expect_error(make_rand_params(bcoef, 10), "missing required")
})

test_that("error when respondent_ids length mismatches", {
  bcoef <- list(b = list(dist = "fixed", value = 1))
  expect_error(make_rand_params(bcoef, 5, respondent_ids = 1:3),
               "must have length")
})
