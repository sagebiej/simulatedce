# ── output structure ──────────────────────────────────────────────────────────

test_that("output is a data frame with n_resp rows", {
  bcoeff <- list(b1 = list(dist = "normal", mean = 0, sd = 1))
  out <- make_rand_params(bcoeff, 100)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 100)
})

test_that("ID defaults to 1:n_resp", {
  bcoeff <- list(b1 = list(dist = "normal", mean = 0, sd = 1))
  out <- make_rand_params(bcoeff, 50)
  expect_equal(out$ID, 1:50)
})

test_that("custom respondent_ids are used", {
  bcoeff <- list(b1 = list(dist = "normal", mean = 0, sd = 1))
  ids <- c("A", "B", "C")
  out <- make_rand_params(bcoeff, 3, respondent_ids = ids)
  expect_equal(out$ID, ids)
})

test_that("columns are ID plus parameter names", {
  bcoeff <- list(bprice = list(dist = "normal", mean = 0, sd = 1),
                bqual  = 2)
  out <- make_rand_params(bcoeff, 10)
  expect_equal(names(out), c("ID", "bprice", "bqual"))
})

# ── distributions ────────────────────────────────────────────────────────────

test_that("normal draws recover mean and sd (large n)", {
  set.seed(314)
  bcoeff <- list(b = list(dist = "normal", mean = -0.5, sd = 0.2))
  out <- make_rand_params(bcoeff, 10000)
  expect_equal(mean(out$b), -0.5, tolerance = 0.05)
  expect_equal(sd(out$b),    0.2, tolerance = 0.05)
})

test_that("lognormal draws are all positive", {
  set.seed(628)
  bcoeff <- list(b = list(dist = "lognormal", meanlog = 0, sdlog = 0.5))
  out <- make_rand_params(bcoeff, 1000)
  expect_true(all(out$b > 0))
})

test_that("neg_lognormal draws are all negative", {
  set.seed(971)
  bcoeff <- list(b = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3))
  out <- make_rand_params(bcoeff, 1000)
  expect_true(all(out$b < 0))
})

test_that("uniform draws lie within [min, max]", {
  set.seed(159)
  bcoeff <- list(b = list(dist = "uniform", min = 2, max = 5))
  out <- make_rand_params(bcoeff, 1000)
  expect_true(all(out$b >= 2))
  expect_true(all(out$b <= 5))
})

test_that("triangular draws lie within [min, max]", {
  set.seed(265)
  bcoeff <- list(b = list(dist = "triangular", min = 0, max = 10, mode = 3))
  out <- make_rand_params(bcoeff, 1000)
  expect_true(all(out$b >= 0))
  expect_true(all(out$b <= 10))
})

test_that("fixed (numeric) gives identical values for all respondents", {
  bcoeff <- list(b = 42)
  out <- make_rand_params(bcoeff, 100)
  expect_true(all(out$b == 42))
})

# ── input validation ─────────────────────────────────────────────────────────

test_that("error when bcoeff is unnamed", {
  expect_error(make_rand_params(list(list(dist = "normal", mean = 0, sd = 1)), 10),
               "needs a name")
})

test_that("error for unknown distribution", {
  bcoeff <- list(b = list(dist = "cauchy", location = 0, scale = 1))
  expect_error(make_rand_params(bcoeff, 10), "Unknown distribution")
})

test_that("error when required moments are missing", {
  bcoeff <- list(b = list(dist = "normal", mean = 0))
  expect_error(make_rand_params(bcoeff, 10), "missing required")
})

test_that("error when parameter is list but lacks dist", {
  bcoeff <- list(b = list(mean = 0, sd = 1))
  expect_error(make_rand_params(bcoeff, 10), "must be a numeric scalar or a list with a `dist` element")
})

test_that("error when respondent_ids length mismatches", {
  bcoeff <- list(b = 1)
  expect_error(make_rand_params(bcoeff, 5, respondent_ids = 1:3),
               "one id per respondent")
})

# ── additional distribution and structure tests ───────────────────────────────

test_that("fixed dist spec (list form) gives identical values", {
  bcoeff <- list(b = list(dist = "fixed", value = 3.14))
  out <- make_rand_params(bcoeff, 50)
  expect_true(all(out$b == 3.14))
})

test_that("multiple parameters produce correct column count", {
  bcoeff <- list(
    b1 = list(dist = "normal", mean = 0, sd = 1),
    b2 = 0.5,
    b3 = list(dist = "uniform", min = 0, max = 1)
  )
  out <- make_rand_params(bcoeff, 20)
  expect_equal(ncol(out), 4)  # ID + 3 params
  expect_equal(nrow(out), 20)
})

test_that("lognormal draws recover meanlog and sdlog (large n)", {
  set.seed(7291)
  bcoeff <- list(b = list(dist = "lognormal", meanlog = 0.5, sdlog = 0.3))
  out <- make_rand_params(bcoeff, 10000)
  expect_equal(mean(log(out$b)), 0.5, tolerance = 0.05)
  expect_equal(sd(log(out$b)),   0.3, tolerance = 0.05)
})

test_that("neg_lognormal is the negative of lognormal with same seed", {
  set.seed(4812)
  pos <- make_rand_params(list(b = list(dist = "lognormal",     meanlog = 0, sdlog = 0.5)), 200)
  set.seed(4812)
  neg <- make_rand_params(list(b = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.5)), 200)
  expect_equal(pos$b, -neg$b)
})

test_that("uniform draws have correct empirical mean (large n)", {
  set.seed(1357)
  bcoeff <- list(b = list(dist = "uniform", min = -2, max = 4))
  out <- make_rand_params(bcoeff, 10000)
  expect_equal(mean(out$b), 1, tolerance = 0.05)  # mean of U(-2, 4) = 1
})

test_that("triangular draws are skewed toward mode (large n)", {
  set.seed(2468)
  bcoeff <- list(b = list(dist = "triangular", min = 0, max = 10, mode = 2))
  out <- make_rand_params(bcoeff, 10000)
  expect_true(all(out$b >= 0 & out$b <= 10))
  # with mode=2 far below midpoint=5, median should be below 5
  expect_lt(median(out$b), 5)
})

test_that("n_resp = 1 works", {
  bcoeff <- list(b = list(dist = "normal", mean = 0, sd = 1))
  out <- make_rand_params(bcoeff, 1)
  expect_equal(nrow(out), 1)
})

test_that("error for non-positive n_resp", {
  bcoeff <- list(b = 1)
  expect_error(make_rand_params(bcoeff, 0),  "n_resp")
  expect_error(make_rand_params(bcoeff, -5), "n_resp")
})

# ── triangular distribution ───────────────────────────────────────────────────

test_that("triangular rejects min >= max and a mode outside the bounds", {
  expect_error(make_rand_params(list(b = list(dist = "triangular", min = 5, max = 5, mode = 5)), 3),
               "min < max")
  expect_error(make_rand_params(list(b = list(dist = "triangular", min = 0, max = 1, mode = 2)), 3),
               "min <= mode <= max")
  expect_error(make_rand_params(list(b = list(dist = "triangular", min = 0, max = 1, mode = -1)), 3),
               "min <= mode <= max")
})

test_that("triangular accepts a mode on the boundary", {
  set.seed(515)
  left  <- make_rand_params(list(b = list(dist = "triangular", min = 0, max = 10, mode = 0)), 2000)
  right <- make_rand_params(list(b = list(dist = "triangular", min = 0, max = 10, mode = 10)), 2000)
  expect_true(all(left$b >= 0 & left$b <= 10))
  expect_true(all(right$b >= 0 & right$b <= 10))
  # mode at the lower bound skews low, at the upper bound skews high
  expect_lt(mean(left$b), mean(right$b))
})

test_that("triangular mean matches (min + max + mode) / 3", {
  set.seed(616)
  out <- make_rand_params(list(b = list(dist = "triangular", min = 0, max = 9, mode = 3)), 20000)
  expect_equal(mean(out$b), (0 + 9 + 3) / 3, tolerance = 0.05)
})

# ── moment recovery for the remaining distributions ──────────────────────────

test_that("neg_lognormal recovers the moments of the underlying normal", {
  set.seed(717)
  out <- make_rand_params(list(b = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4)), 20000)
  expect_equal(mean(log(-out$b)), -1,  tolerance = 0.05)
  expect_equal(sd(log(-out$b)),    0.4, tolerance = 0.05)
})

test_that("normal with sd = 0 degenerates to the mean", {
  out <- make_rand_params(list(b = list(dist = "normal", mean = 1.5, sd = 0)), 20)
  expect_equal(unique(out$b), 1.5)
})

# ── independence and column layout ───────────────────────────────────────────

test_that("draws are independent across parameters", {
  # documents the current design: no correlation structure between parameters
  set.seed(818)
  out <- make_rand_params(list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1)
  ), 5000)
  expect_equal(cor(out$a, out$b), 0, tolerance = 0.05)
})

test_that("column order follows the order of bcoeff, not alphabetical", {
  out <- make_rand_params(list(z = 1, a = 2, m = list(dist = "normal", mean = 0, sd = 1)), 3)
  expect_equal(names(out), c("ID", "z", "a", "m"))
})

test_that("non-sequential respondent ids are carried through unchanged", {
  ids <- c(7L, 3L, 11L)
  out <- make_rand_params(list(b = list(dist = "normal", mean = 0, sd = 1)), 3,
                          respondent_ids = ids)
  expect_identical(out$ID, ids)
})

test_that("each respondent gets exactly one draw", {
  set.seed(919)
  out <- make_rand_params(list(b = list(dist = "normal", mean = 0, sd = 1)), 500)
  expect_equal(nrow(out), 500)
  expect_equal(anyDuplicated(out$ID), 0L)
})

test_that("an unnamed element in an otherwise named list is rejected", {
  expect_error(make_rand_params(list(a = 1, 2), 5), "needs a name")
})

test_that("respondent_ids longer or shorter than n_resp is rejected", {
  expect_error(make_rand_params(list(b = 1), 3, respondent_ids = 1:5), "one id per respondent")
  expect_error(make_rand_params(list(b = 1), 5, respondent_ids = 1:3), "one id per respondent")
})
