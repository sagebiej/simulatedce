## The distribution registry is the single place that knows which distributions
## exist, what they need, and what their moments are. These tests keep the three
## in step, so adding a distribution cannot half-work.

test_that("every registered distribution declares args, draw, mean and sd", {
  for (nm in supported_dists()) {
    d <- dce_distributions[[nm]]
    expect_type(d$args, "character")
    expect_true(is.function(d$draw), info = nm)
    expect_true(is.function(d$mean), info = nm)
    expect_true(is.function(d$sd), info = nm)
  }
})

## A valid specification for each distribution, used by the loops below.
example_specs <- list(
  fixed            = list(dist = "fixed", value = 0.4),
  normal           = list(dist = "normal", mean = -0.5, sd = 0.2),
  lognormal        = list(dist = "lognormal", meanlog = -1, sdlog = 0.4),
  neg_lognormal    = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4),
  uniform          = list(dist = "uniform", min = -2, max = 4),
  triangular       = list(dist = "triangular", min = 0, max = 9, mode = 3),
  truncated_normal = list(dist = "truncated_normal", mean = 0, sd = 1, min = -1, max = 2)
)

test_that("there is an example spec for every registered distribution", {
  expect_setequal(names(example_specs), supported_dists())
})

test_that("the analytic mean and sd match large samples", {
  set.seed(20240101)
  for (nm in names(example_specs)) {
    spec <- example_specs[[nm]]
    draws <- draw_from_spec(spec, 50000)

    expect_equal(mean(draws), spec_mean(spec), tolerance = 0.05, info = nm)
    if (nm != "fixed") {
      expect_equal(stats::sd(draws), spec_sd(spec), tolerance = 0.05, info = nm)
    }
  }
})

test_that("draws respect the declared support", {
  set.seed(20240102)
  expect_true(all(draw_from_spec(example_specs$lognormal, 2000) > 0))
  expect_true(all(draw_from_spec(example_specs$neg_lognormal, 2000) < 0))

  u <- draw_from_spec(example_specs$uniform, 2000)
  expect_true(all(u >= -2 & u <= 4))

  tri <- draw_from_spec(example_specs$triangular, 2000)
  expect_true(all(tri >= 0 & tri <= 9))

  tn <- draw_from_spec(example_specs$truncated_normal, 2000)
  expect_true(all(tn >= -1 & tn <= 2))
})

test_that("every distribution is described on one readable line", {
  for (nm in names(example_specs)) {
    line <- describe_spec(example_specs[[nm]], "bpar")
    expect_length(line, 1)
    expect_match(line, "bpar", info = nm)
    expect_match(line, nm, fixed = TRUE, info = nm)
    if (nm != "fixed") {
      expect_match(line, "mean = ", info = nm)
      expect_match(line, "sd = ", info = nm)
    }
  }
})

test_that("a bare number is shorthand for a fixed coefficient", {
  spec <- as_dist_spec(-0.25, "bprice")
  expect_equal(spec$dist, "fixed")
  expect_equal(spec$value, -0.25)
  expect_equal(spec_mean(-0.25), -0.25)
  expect_equal(spec_sd(-0.25), 0)
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("a missing argument is reported with the parameter and argument names", {
  expect_error(
    as_dist_spec(list(dist = "normal", mean = 0), "bprice"),
    "'bprice' \\(dist = 'normal'\\) is missing required argument\\(s\\): sd"
  )
  expect_error(
    as_dist_spec(list(dist = "triangular", min = 0), "bqual"),
    "missing required argument\\(s\\): max, mode"
  )
})

test_that("an unknown distribution lists the ones that exist", {
  err <- tryCatch(as_dist_spec(list(dist = "cauchy"), "bprice"), error = conditionMessage)
  expect_match(err, "Unknown distribution 'cauchy' for parameter 'bprice'")
  for (nm in supported_dists()) expect_match(err, nm, fixed = TRUE)
})

test_that("a non-numeric or empty argument is refused by name", {
  expect_error(
    as_dist_spec(list(dist = "normal", mean = "a lot", sd = 1), "bprice"),
    "needs numeric argument\\(s\\): mean"
  )
  expect_error(
    as_dist_spec(list(dist = "normal", mean = numeric(0), sd = 1), "bprice"),
    "needs a single value for argument\\(s\\): mean"
  )
})

test_that("a vector of numbers is refused", {
  expect_error(as_dist_spec(c(1, 2), "bprice"), "must be a single number")
})

test_that("a list without dist is refused", {
  expect_error(
    as_dist_spec(list(mean = 0, sd = 1), "bprice"),
    "must be a numeric scalar or a list with a `dist` element"
  )
})

# ── truncated normal ─────────────────────────────────────────────────────────

test_that("truncated_normal handles one-sided truncation", {
  set.seed(20240103)
  neg_only <- list(dist = "truncated_normal", mean = 0, sd = 1, min = -Inf, max = 0)
  draws <- draw_from_spec(neg_only, 5000)

  expect_true(all(draws <= 0))
  expect_false(is.na(spec_sd(neg_only)))
  expect_equal(mean(draws), spec_mean(neg_only), tolerance = 0.05)
  expect_equal(stats::sd(draws), spec_sd(neg_only), tolerance = 0.05)
})

test_that("truncated_normal with wide bounds matches the untruncated normal", {
  wide <- list(dist = "truncated_normal", mean = -0.5, sd = 0.2, min = -Inf, max = Inf)
  expect_equal(spec_mean(wide), -0.5, tolerance = 1e-8)
  expect_equal(spec_sd(wide), 0.2, tolerance = 1e-8)
})

test_that("truncated_normal rejects impossible bounds", {
  expect_error(
    draw_from_spec(list(dist = "truncated_normal", mean = 0, sd = 1, min = 2, max = 1), 5),
    "min < max"
  )
  expect_error(
    draw_from_spec(list(dist = "truncated_normal", mean = 0, sd = 0, min = -1, max = 1), 5),
    "sd > 0"
  )
})

# ── has_random_params and bcoeff_table ───────────────────────────────────────

test_that("has_random_params sees any non-numeric entry", {
  expect_false(has_random_params(list(a = 1, b = -2)))
  expect_true(has_random_params(list(a = 1, b = list(dist = "normal", mean = 0, sd = 1))))
})

test_that("bcoeff_table turns dots into underscores to match mixl", {
  tab <- bcoeff_table(list(b.price = -1, b_qual = 2))
  expect_equal(tab$parname, c("b_price", "b_qual"))
})

test_that("bcoeff_table on an empty or NULL bcoeff gives an empty table", {
  for (x in list(NULL, list())) {
    tab <- bcoeff_table(x)
    expect_equal(nrow(tab), 0)
    expect_named(tab, c("parname", "truepar", "truesd"))
  }
})

# ── bcoeff_moments, the user-facing view ─────────────────────────────────────

test_that("bcoeff_moments reports one row per parameter with its distribution", {
  bc <- list(
    bprice = list(dist = "neg_lognormal", meanlog = -3, sdlog = 0.5),
    bqual = 0.4
  )
  m <- bcoeff_moments(bc)

  expect_equal(nrow(m), 2)
  expect_named(m, c("parameter", "dist", "mean", "sd"))
  expect_equal(m$parameter, c("bprice", "bqual"))
  expect_equal(m$dist, c("neg_lognormal", "fixed"))
  expect_equal(m$sd[2], 0)
  expect_lt(m$mean[1], 0)
})

test_that("bcoeff_moments explains a lognormal in coefficient units", {
  # meanlog = 0 does not mean the coefficient averages 0
  m <- bcoeff_moments(list(b = list(dist = "lognormal", meanlog = 0, sdlog = 0.5)))
  expect_equal(m$mean, exp(0 + 0.5^2 / 2))
  expect_gt(m$mean, 1)
})

test_that("bcoeff_moments validates like the rest", {
  expect_error(bcoeff_moments("nope"), "must be a list")
  expect_error(bcoeff_moments(list(1, 2)), "needs a name")
})
