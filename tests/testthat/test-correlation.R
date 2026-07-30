## Correlated random parameters, imposed with a Gaussian copula so that any
## marginal can be correlated with any other.

# ── correlate() ──────────────────────────────────────────────────────────────

test_that("correlate builds a named correlation matrix", {
  r <- correlate(c("bprice", "bqual") ~ 0.4)
  expect_equal(dim(r), c(2L, 2L))
  expect_equal(dimnames(r), list(c("bprice", "bqual"), c("bprice", "bqual")))
  expect_equal(diag(r), c(bprice = 1, bqual = 1))
  expect_equal(r["bprice", "bqual"], 0.4)
  expect_equal(r["bqual", "bprice"], 0.4)
})

test_that("correlate handles several pairs and an explicit ordering", {
  r <- correlate(
    c("bprice", "bqual") ~ 0.4,
    c("bprice", "btime") ~ -0.2
  )
  expect_equal(nrow(r), 3L)
  expect_equal(r["bprice", "btime"], -0.2)
  expect_equal(r["bqual", "btime"], 0) # not named, so uncorrelated

  ordered <- correlate(c("bqual", "bprice") ~ 0.4, parameters = c("bprice", "bqual"))
  expect_equal(colnames(ordered), c("bprice", "bqual"))
})

test_that("correlate refuses malformed input", {
  expect_error(correlate(), "at least one pair")
  expect_error(correlate("bprice" ~ 0.4), "exactly two coefficients")
  expect_error(correlate(c("a", "b") ~ "half"), "single number")
  expect_error(correlate(c("a", "b") ~ 0.4, parameters = c("a")), "not in `parameters`")
})

# ── the draws ────────────────────────────────────────────────────────────────

test_that("normal marginals reproduce the requested correlation exactly", {
  set.seed(1)
  bc <- list(
    bprice = list(dist = "normal", mean = -0.4, sd = 0.2),
    bqual  = list(dist = "normal", mean = 0.5, sd = 0.3)
  )
  d <- make_rand_params(bc, 20000, correlation = correlate(c("bprice", "bqual") ~ 0.6))

  expect_equal(stats::cor(d$bprice, d$bqual), 0.6, tolerance = 0.02)
  # and the marginals are untouched
  expect_equal(mean(d$bprice), -0.4, tolerance = 0.01)
  expect_equal(stats::sd(d$bprice), 0.2, tolerance = 0.01)
  expect_equal(mean(d$bqual), 0.5, tolerance = 0.01)
  expect_equal(stats::sd(d$bqual), 0.3, tolerance = 0.01)
})

test_that("a negative correlation works", {
  set.seed(2)
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1)
  )
  d <- make_rand_params(bc, 20000, correlation = correlate(c("a", "b") ~ -0.5))
  expect_equal(stats::cor(d$a, d$b), -0.5, tolerance = 0.02)
})

test_that("non-normal marginals keep their support and come out close to the target", {
  set.seed(3)
  bc <- list(
    bprice = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4),
    bqual  = list(dist = "triangular", min = 0, max = 1, mode = 0.3)
  )
  d <- make_rand_params(bc, 20000, correlation = correlate(c("bprice", "bqual") ~ 0.6))

  expect_true(all(d$bprice < 0))
  expect_true(all(d$bqual >= 0 & d$bqual <= 1))
  # the rank correlation is what a Gaussian copula preserves
  expect_equal(stats::cor(d$bprice, d$bqual, method = "spearman"), 0.6, tolerance = 0.05)
  # Pearson is close but need not be exact, since the marginals are not linear
  expect_equal(stats::cor(d$bprice, d$bqual), 0.6, tolerance = 0.1)
})

test_that("a lognormal can be correlated with a normal", {
  set.seed(4)
  bc <- list(
    a = list(dist = "lognormal", meanlog = 0, sdlog = 0.5),
    b = list(dist = "normal", mean = 1, sd = 0.4)
  )
  d <- make_rand_params(bc, 10000, correlation = correlate(c("a", "b") ~ 0.5))
  expect_true(all(d$a > 0))
  expect_gt(stats::cor(d$a, d$b, method = "spearman"), 0.4)
})

test_that("coefficients the matrix does not mention stay independent", {
  set.seed(5)
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1),
    cc = list(dist = "normal", mean = 0, sd = 1)
  )
  d <- make_rand_params(bc, 10000, correlation = correlate(c("a", "b") ~ 0.7))

  expect_equal(stats::cor(d$a, d$b), 0.7, tolerance = 0.03)
  expect_equal(stats::cor(d$a, d$cc), 0, tolerance = 0.04)
  expect_equal(stats::cor(d$b, d$cc), 0, tolerance = 0.04)
})

test_that("fixed coefficients still come out constant alongside correlated ones", {
  set.seed(6)
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1),
    fixed = 0.25
  )
  d <- make_rand_params(bc, 500, correlation = correlate(c("a", "b") ~ 0.5))
  expect_true(all(d$fixed == 0.25))
})

test_that("three correlated coefficients work", {
  set.seed(7)
  bc <- stats::setNames(
    rep(list(list(dist = "normal", mean = 0, sd = 1)), 3),
    c("a", "b", "cc")
  )
  r <- correlate(
    c("a", "b") ~ 0.5,
    c("a", "cc") ~ 0.3,
    c("b", "cc") ~ 0.2
  )
  d <- make_rand_params(bc, 20000, correlation = r)
  realised <- stats::cor(d[, c("a", "b", "cc")])
  expect_equal(realised[upper.tri(realised)], r[upper.tri(r)], tolerance = 0.03)
})

test_that("the draws are reproducible", {
  r <- correlate(c("a", "b") ~ 0.5)
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1)
  )
  set.seed(8)
  x <- make_rand_params(bc, 100, correlation = r)
  set.seed(8)
  y <- make_rand_params(bc, 100, correlation = r)
  expect_identical(x, y)
})

test_that("omitting correlation reproduces the independent draws exactly", {
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1)
  )
  set.seed(9)
  x <- make_rand_params(bc, 100)
  set.seed(9)
  y <- make_rand_params(bc, 100, correlation = NULL)
  expect_identical(x, y)
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("a matrix without names is refused", {
  bc <- list(
    a = list(dist = "normal", mean = 0, sd = 1),
    b = list(dist = "normal", mean = 0, sd = 1)
  )
  expect_error(
    make_rand_params(bc, 10, correlation = matrix(c(1, .5, .5, 1), 2)),
    "needs column names"
  )
})

test_that("a coefficient not in bcoeff is refused", {
  bc <- list(a = list(dist = "normal", mean = 0, sd = 1))
  expect_error(
    make_rand_params(bc, 10, correlation = correlate(c("a", "zzz") ~ 0.5)),
    "`zzz`, which are not in `bcoeff`"
  )
})

test_that("correlating a fixed coefficient is refused", {
  bc <- list(a = 1, b = list(dist = "normal", mean = 0, sd = 1))
  expect_error(
    make_rand_params(bc, 10, correlation = correlate(c("a", "b") ~ 0.5)),
    "which are fixed numbers"
  )
})

test_that("an impossible correlation is refused", {
  bc <- stats::setNames(
    rep(list(list(dist = "normal", mean = 0, sd = 1)), 3),
    c("a", "b", "cc")
  )
  expect_error(
    make_rand_params(bc, 10, correlation = correlate(c("a", "b") ~ 1.5)),
    "outside -1 to 1"
  )
  # a matrix that is symmetric and in range but not positive definite
  r <- correlate(c("a", "b") ~ 0.99, c("a", "cc") ~ 0.99, c("b", "cc") ~ -0.99)
  expect_error(make_rand_params(bc, 10, correlation = r), "not a valid correlation matrix")
})

test_that("a non-square or non-matrix argument is refused", {
  bc <- list(a = list(dist = "normal", mean = 0, sd = 1))
  expect_error(make_rand_params(bc, 10, correlation = "high"), "must be a numeric matrix")
  expect_error(
    make_rand_params(bc, 10, correlation = matrix(1, 2, 3, dimnames = list(NULL, NULL))),
    "must be square"
  )
})

test_that("correlation without any random coefficient is refused", {
  d <- data.frame(ID = rep(1:4, each = 2), price = rep(c(10, 20), 4))
  ut <- list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0))
  expect_error(
    simulate_choices(d, ut,
      bcoeff = list(bp = -1),
      correlation = correlate(c("bp", "bp") ~ 0.5), verbose = 0
    ),
    "every entry of `bcoeff` is a fixed number"
  )
})

# ── through the simulation ───────────────────────────────────────────────────

corr_design <- local({
  set.seed(4)
  prof <- expand.grid(price = c(2, 4, 6, 8), quality = c(0, 1))
  a1 <- prof[rep(seq_len(nrow(prof)), 2), ]
  n <- nrow(a1)
  repeat {
    pr <- sample(n)
    if (all(rowSums(abs(a1 - a1[pr, ])) > 0)) break
  }
  data.frame(
    Choice.situation = seq_len(n), Block = rep(1:2, each = n / 2),
    alt1.price = a1$price, alt1.quality = a1$quality,
    alt2.price = a1$price[pr], alt2.quality = a1$quality[pr]
  )
})

ul_corr <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bqual * alt1.quality,
  v2 = V.2 ~ bprice * alt2.price + bqual * alt2.quality
))

bc_corr <- list(
  bprice = list(dist = "normal", mean = -0.4, sd = 0.2),
  bqual  = list(dist = "normal", mean = 0.5, sd = 0.3)
)

test_that("the correlation shows up in the simulated respondent draws", {
  td <- withr::local_tempdir()
  saveRDS(corr_design, file.path(td, "d.rds"))

  d <- suppressMessages(sim_all(
    nosim = 1, resps = 2000, designpath = td, u = ul_corr, bcoeff = bc_corr,
    correlation = correlate(c("bprice", "bqual") ~ 0.7),
    estimate = FALSE, mode = "sequential", seed = 5, verbose = 0
  ))$d[[1]]

  per_resp <- d[match(unique(d$ID), d$ID), c("bprice", "bqual")]
  expect_equal(stats::cor(per_resp$bprice, per_resp$bqual), 0.7, tolerance = 0.05)
})

test_that("without correlation the draws are independent", {
  td <- withr::local_tempdir()
  saveRDS(corr_design, file.path(td, "d.rds"))

  d <- suppressMessages(sim_all(
    nosim = 1, resps = 2000, designpath = td, u = ul_corr, bcoeff = bc_corr,
    estimate = FALSE, mode = "sequential", seed = 5, verbose = 0
  ))$d[[1]]

  per_resp <- d[match(unique(d$ID), d$ID), c("bprice", "bqual")]
  expect_equal(stats::cor(per_resp$bprice, per_resp$bqual), 0, tolerance = 0.06)
})

test_that("the correlation is recorded in the arguments", {
  td <- withr::local_tempdir()
  saveRDS(corr_design, file.path(td, "d.rds"))
  r <- correlate(c("bprice", "bqual") ~ 0.7)

  res <- suppressMessages(sim_all(
    nosim = 1, resps = 100, designpath = td, u = ul_corr, bcoeff = bc_corr,
    correlation = r, estimate = FALSE, mode = "sequential", verbose = 0
  ))
  expect_equal(res$arguments$Correlation, r)
})

test_that("a correlated data generating process still estimates", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(corr_design, file.path(td, "d.rds"))

  res <- suppressMessages(sim_all(
    nosim = 10, resps = 400, designpath = td, u = ul_corr, bcoeff = bc_corr,
    correlation = correlate(c("bprice", "bqual") ~ 0.7),
    mode = "sequential", seed = 6, verbose = 0
  ))
  sa <- res$summaryall
  # a multinomial logit still recovers roughly the means
  expect_lt(abs(sa$d.mean[sa$parname == "bprice"] - (-0.4)), 0.1)
  expect_lt(abs(sa$d.mean[sa$parname == "bqual"] - 0.5), 0.15)
})
