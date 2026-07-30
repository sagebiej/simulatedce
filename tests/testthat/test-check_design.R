## check_design() exists because a design whose attributes cannot be told apart
## still converges, still fills in the summary table, and is still wrong.

## price = 2 * quality + 4 * origin in differences, so origin is not identified
collinear <- data.frame(
  Choice.situation = 1:16,
  alt1.price = rep(c(2, 4, 6, 8), 4), alt1.quality = rep(c(0, 1), 8),
  alt1.origin = rep(c(0, 0, 1, 1), 4),
  alt2.price = rep(c(8, 6, 4, 2), 4), alt2.quality = rep(c(1, 0), 8),
  alt2.origin = rep(c(1, 1, 0, 0), 4)
)

well_conditioned <- local({
  set.seed(4711)
  prof <- expand.grid(price = c(2, 4, 6, 8), quality = c(0, 1), origin = c(0, 1))
  n <- nrow(prof)
  partner <- sample(n)
  data.frame(
    Choice.situation = seq_len(n), Block = rep(1:2, each = n / 2),
    alt1.price = prof$price, alt1.quality = prof$quality, alt1.origin = prof$origin,
    alt2.price = prof$price[partner], alt2.quality = prof$quality[partner],
    alt2.origin = prof$origin[partner]
  )
})

ul3 <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality + borigin * alt1.origin,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality + borigin * alt2.origin
))
bc3 <- list(bprice = -0.3, bquality = 0.5, borigin = 0.4)

# ── identification ────────────────────────────────────────────────────────────

test_that("a collinear design is reported as not identified, naming the aliased term", {
  res <- check_design(collinear)
  expect_false(res$identified)
  expect_equal(res$rank, 2L)
  expect_equal(res$n_terms, 3L)
  expect_length(res$aliased, 1)
  expect_true(any(grepl("Not identified", res$problems)))
})

test_that("a well conditioned design passes with no problems", {
  res <- check_design(well_conditioned)
  expect_true(res$identified)
  expect_equal(res$rank, res$n_terms)
  expect_length(res$aliased, 0)
  expect_length(res$problems, 0)
})

test_that("the same verdict is reached from the utility functions", {
  from_names <- check_design(well_conditioned)
  from_u <- check_design(well_conditioned, u = ul3)

  expect_equal(from_u$rank, from_names$rank)
  expect_equal(from_u$identified, from_names$identified)
  # but the terms are named after the coefficients
  expect_equal(from_u$terms, names(bc3))
  expect_true(from_u$from_utility)
  expect_false(from_names$from_utility)
})

test_that("collinearity is caught from the utility functions too", {
  res <- check_design(collinear, u = ul3)
  expect_false(res$identified)
  expect_equal(res$rank, 2L)
})

test_that("an interaction that adds information raises the rank", {
  d <- well_conditioned
  ul_int <- list(u1 = list(
    v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality +
      bboth * (alt1.price * alt1.quality),
    v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality +
      bboth * (alt2.price * alt2.quality)
  ))
  res <- check_design(d, u = ul_int)
  expect_equal(res$n_terms, 3L)
  expect_true(res$identified)
})

test_that("a term duplicating another is caught", {
  ul_dup <- list(u1 = list(
    v1 = V.1 ~ bprice * alt1.price + bcopy * (2 * alt1.price),
    v2 = V.2 ~ bprice * alt2.price + bcopy * (2 * alt2.price)
  ))
  res <- check_design(well_conditioned, u = ul_dup)
  expect_false(res$identified)
  expect_equal(res$rank, 1L)
})

test_that("an alternative-specific constant is identified", {
  ul_asc <- list(u1 = list(
    v1 = V.1 ~ basc + bprice * alt1.price,
    v2 = V.2 ~ bprice * alt2.price
  ))
  res <- check_design(well_conditioned, u = ul_asc)
  expect_true(res$identified)
  expect_setequal(res$terms, c("basc", "bprice"))
})

test_that("a term with no within-set variation is reported", {
  d <- well_conditioned
  d$alt1.flat <- 1
  d$alt2.flat <- 1
  ul_flat <- list(u1 = list(
    v1 = V.1 ~ bprice * alt1.price + bflat * alt1.flat,
    v2 = V.2 ~ bprice * alt2.price + bflat * alt2.flat
  ))
  res <- check_design(d, u = ul_flat)
  expect_equal(res$no_variation, "bflat")
  expect_false(res$identified)
  expect_true(any(grepl("No variation", res$problems)))
})

# ── structure reporting ──────────────────────────────────────────────────────

test_that("situations, blocks and alternatives are reported", {
  res <- check_design(well_conditioned)
  expect_equal(res$situations, 16L)
  expect_equal(res$blocks, 2L)
  expect_equal(res$alternatives, 2L)
  expect_equal(res$sets_per_block, c(8L, 8L))
})

test_that("unequal block sizes are flagged", {
  d <- data.frame(
    Choice.situation = 1:7, Block = c(1, 1, 1, 1, 2, 2, 2),
    alt1.x = 1:7, alt2.x = 7:1
  )
  res <- check_design(d)
  expect_true(any(grepl("different sizes", res$problems)))
})

test_that("identical alternatives within a choice situation are counted", {
  d <- data.frame(
    Choice.situation = 1:4,
    alt1.x = c(1, 2, 3, 4), alt2.x = c(1, 5, 6, 7)
  )
  res <- check_design(d)
  expect_equal(res$identical_alternatives, 1L)
  expect_true(any(grepl("identical", res$problems)))
})

test_that("dominated situations are counted only when bcoeff is given", {
  without <- check_design(well_conditioned, u = ul3)
  with_bc <- check_design(well_conditioned, u = ul3, bcoeff = bc3)
  expect_true(is.na(without$dominated))
  expect_false(is.na(with_bc$dominated))
  expect_gte(with_bc$dominated, 0)
})

test_that("three or more alternatives are handled", {
  d <- data.frame(
    Choice.situation = 1:8,
    alt1.x = c(1, 2, 3, 4, 1, 2, 3, 4), alt2.x = c(4, 1, 2, 3, 3, 4, 1, 2),
    alt3.x = c(2, 4, 1, 1, 4, 3, 2, 3)
  )
  res <- check_design(d)
  expect_equal(res$alternatives, 3L)
  expect_true(res$identified)
})

# ── input handling ───────────────────────────────────────────────────────────

test_that("a design file path is read for you", {
  f <- system.file("extdata", "SE_DRIVE", "effconstrsmall.ngd", package = "simulateDCE")
  res <- check_design(f)
  expect_s3_class(res, "dce_design_check")
  expect_equal(res$situations, 60L)
  expect_equal(res$blocks, 5L)
})

test_that("a design with no recognisable alternatives says what was expected", {
  expect_error(
    check_design(data.frame(Choice.situation = 1:4, price = 1:4)),
    "Expected columns like"
  )
})

test_that("a mistyped column is caught when bcoeff says which names are coefficients", {
  ul_bad <- list(u1 = list(
    v1 = V.1 ~ bprice * alt1.nonexistent,
    v2 = V.2 ~ bprice * alt2.price
  ))
  expect_error(
    check_design(well_conditioned, u = ul_bad, bcoeff = list(bprice = -0.3)),
    "not columns of the design and not in `bcoeff`"
  )
})

test_that("without bcoeff a mistyped column is still flagged, as a non-linearity", {
  ## With no `bcoeff` to say which names are coefficients, a mistyped column is
  ## taken for one, so `bprice * alt1.nonexistent` looks like two coefficients
  ## multiplied together. The verdict is "this is not linear in the coefficients"
  ## rather than "not identified", but either way it does not pass silently.
  ul_bad <- list(u1 = list(
    v1 = V.1 ~ bprice * alt1.nonexistent,
    v2 = V.2 ~ bprice * alt2.price
  ))
  res <- check_design(well_conditioned, u = ul_bad)
  expect_false(res$linear_in_coefficients)
  expect_true(is.na(res$identified))
  expect_gt(length(res$problems), 0)
})

test_that("the result prints as a readable report", {
  out <- utils::capture.output(print(check_design(collinear)))
  expect_true(any(grepl("Design check", out)))
  expect_true(any(grepl("rank 2 of 3", out)))
  expect_true(any(grepl("Not identified", out)))
})

test_that("a clean design prints that it looks fine", {
  out <- utils::capture.output(print(check_design(well_conditioned)))
  expect_true(any(grepl("Looks fine", out)))
})

# ── the warning from sim_all ─────────────────────────────────────────────────

test_that("sim_all warns when the design cannot identify the model", {
  td <- withr::local_tempdir()
  saveRDS(collinear, file.path(td, "collinear.rds"))

  expect_warning(
    suppressMessages(sim_all(
      nosim = 1, resps = 40, designpath = td, u = ul3, bcoeff = bc3,
      estimate = FALSE, mode = "sequential", verbose = 0
    )),
    "cannot identify the model"
  )
})

test_that("sim_all does not warn on a design that is fine", {
  td <- withr::local_tempdir()
  saveRDS(well_conditioned, file.path(td, "ok.rds"))

  expect_no_warning(
    suppressMessages(sim_all(
      nosim = 1, resps = 40, designpath = td, u = ul3, bcoeff = bc3,
      estimate = FALSE, mode = "sequential", verbose = 0
    ))
  )
})
