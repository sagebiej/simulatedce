## Availability of alternatives, and no-choice options that are only sometimes
## offered.

make_design <- function(av3 = rep(c(1, 0), length.out = 16)) {
  set.seed(31)
  prof <- expand.grid(price = c(2, 4, 6, 8), quality = c(0, 1))
  alt1 <- prof[rep(seq_len(nrow(prof)), 2), ]
  n <- nrow(alt1)
  repeat {
    partner <- sample(n)
    if (all(rowSums(abs(alt1 - alt1[partner, ])) > 0)) break
  }
  data.frame(
    Choice.situation = seq_len(n), Block = rep(1:2, each = n / 2),
    alt1.price = alt1$price, alt1.quality = alt1$quality,
    alt2.price = alt1$price[partner], alt2.quality = alt1$quality[partner],
    av1 = 1, av2 = 1, av3 = av3
  )
}

ul_optout <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality,
  v3 = V.3 ~ bnone
))
bc_optout <- list(bprice = -0.3, bquality = 0.5, bnone = -1.5)

in_folder <- function(design, envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(design, file.path(td, "d.rds"))
  td
}

# ── detecting the columns ────────────────────────────────────────────────────

test_that("availability columns are found whatever separator they use", {
  for (nm in c("av1", "av.1", "av_1", "AV1")) {
    d <- data.frame(x = 1)
    d[[nm]] <- 1
    d[[sub("1$", "2", nm)]] <- 1
    expect_equal(length(availability_columns(d)), 2L, info = nm)
  }
})

test_that("no availability columns means nothing is restricted", {
  expect_null(availability_columns(data.frame(x = 1, alt1.price = 2)))
  expect_null(availability_matrix(data.frame(x = 1), 2))
})

test_that("the columns are ordered by alternative number, not alphabetically", {
  d <- data.frame(av10 = 1, av2 = 1, av1 = 1)
  expect_equal(availability_columns(d), c("av1", "av2", "av10"))
})

test_that("the matrix has one row per observation and one column per alternative", {
  d <- data.frame(av1 = c(1, 1), av2 = c(1, 0), av3 = c(0, 1))
  av <- availability_matrix(d, 3)
  expect_equal(dim(av), c(2L, 3L))
  expect_equal(av[1, ], c(1, 1, 0))
  expect_equal(av[2, ], c(1, 0, 1))
})

test_that("logical availability columns are accepted", {
  d <- data.frame(av1 = c(TRUE, TRUE), av2 = c(TRUE, FALSE))
  expect_equal(availability_matrix(d, 2)[, 2], c(1, 0))
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("a mismatched number of availability columns is refused", {
  d <- data.frame(av1 = 1, av2 = 1)
  expect_error(availability_matrix(d, 3), "do not match the alternatives")
  expect_error(availability_matrix(d, 3), "`av1`, `av2`, and `av3` were expected")
})

test_that("values other than 0 and 1 are refused by column name", {
  d <- data.frame(av1 = c(1, 1), av2 = c(1, 2))
  expect_error(availability_matrix(d, 2), "`av2` must be 0 or 1")
})

test_that("a row with nothing available is refused, naming the row", {
  d <- data.frame(av1 = c(1, 0), av2 = c(1, 0))
  expect_error(availability_matrix(d, 2), "no available alternative")
  expect_error(availability_matrix(d, 2), "row 2")
})

test_that("a design offering only one alternative everywhere is refused", {
  d <- data.frame(av1 = c(1, 1), av2 = c(0, 0))
  expect_error(availability_matrix(d, 2), "nothing to choose")
})

# ── simulating ───────────────────────────────────────────────────────────────

test_that("an unavailable alternative is never chosen", {
  td <- in_folder(make_design())
  d <- suppressMessages(sim_all(
    nosim = 1, resps = 200, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = FALSE, mode = "sequential", seed = 1, verbose = 0
  ))$d[[1]]

  av_cols <- availability_columns(d)
  expect_length(av_cols, 3)
  offered <- d[[av_cols[3]]]

  expect_equal(sum(d$CHOICE == 3 & offered == 0), 0)
  # and it is chosen sometimes when it is offered
  expect_gt(sum(d$CHOICE == 3 & offered == 1), 0)
})

test_that("CHOICE is still the argmax among the alternatives that were offered", {
  td <- in_folder(make_design())
  d <- suppressMessages(sim_all(
    nosim = 1, resps = 100, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = FALSE, mode = "sequential", seed = 2, verbose = 0
  ))$d[[1]]

  u <- as.matrix(d[, c("U_1", "U_2", "U_3")])
  av <- as.matrix(d[, availability_columns(d)])
  u[av == 0] <- -Inf
  expect_equal(d$CHOICE, max.col(u))
})

test_that("every alternative is available when there are no av columns", {
  design <- make_design()
  design <- design[, setdiff(names(design), c("av1", "av2", "av3"))]
  td <- in_folder(design)

  d <- suppressMessages(sim_all(
    nosim = 1, resps = 100, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = FALSE, mode = "sequential", seed = 3, verbose = 0
  ))$d[[1]]

  expect_equal(d$CHOICE, max.col(as.matrix(d[, c("U_1", "U_2", "U_3")])))
  expect_setequal(unique(d$CHOICE), 1:3)
})

# ── estimating ───────────────────────────────────────────────────────────────

test_that("the coefficients are recovered when an alternative is only sometimes offered", {
  skip_on_cran()
  td <- in_folder(make_design())
  res <- suppressMessages(sim_all(
    nosim = 20, resps = 400, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = TRUE, mode = "sequential", seed = 4, verbose = 0
  ))
  sa <- res$summaryall
  est <- function(p) sa$d.mean[sa$parname == p]

  # the opt-out constant is the one availability affects most
  expect_lt(abs(est("bnone") - (-1.5)), 0.15)
  expect_lt(abs(est("bprice") - (-0.3)), 0.05)
  expect_lt(abs(est("bquality") - 0.5), 0.1)
})

test_that("the availability matrix reaches the estimator", {
  skip_on_cran()
  ## If it did not, the model would think the opt-out was always on offer, see it
  ## chosen half as often relative to what was available, and put bnone too low.
  td <- in_folder(make_design(av3 = rep(c(1, 0, 0, 0), length.out = 16)))
  res <- suppressMessages(sim_all(
    nosim = 20, resps = 400, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = TRUE, mode = "sequential", seed = 5, verbose = 0
  ))
  sa <- res$summaryall
  expect_lt(abs(sa$d.mean[sa$parname == "bnone"] - (-1.5)), 0.2)
})

test_that("design_availabilities falls back on all-available", {
  d <- data.frame(x = 1:4)
  av <- design_availabilities(d, 3)
  expect_equal(dim(av), c(4L, 3L))
  expect_true(all(av == 1))
})

# ── an always-present no-choice needs no availability columns ─────────────────

test_that("a no-choice alternative is just an alternative with only a constant", {
  skip_on_cran()
  design <- make_design(av3 = 1)
  td <- in_folder(design)
  res <- suppressMessages(sim_all(
    nosim = 15, resps = 300, designpath = td, u = ul_optout, bcoeff = bc_optout,
    estimate = TRUE, mode = "sequential", seed = 6, verbose = 0
  ))
  sa <- res$summaryall
  expect_lt(abs(sa$d.mean[sa$parname == "bnone"] - (-1.5)), 0.2)
})
