## power_curve() answers "how many respondents do I need", and the seed argument
## makes any run reproducible from the script alone.

pc_design <- local({
  set.seed(88)
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
    alt2.price = alt1$price[partner], alt2.quality = alt1$quality[partner]
  )
})

ul_pc <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality
))

pc_folder <- function(envir = parent.frame(), name = "d.rds") {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(pc_design, file.path(td, name))
  td
}

pc <- function(td, ...) {
  suppressMessages(power_curve(
    designpath = td, u = ul_pc, bcoeff = list(bprice = -0.06, bquality = 0.12),
    mode = "sequential", verbose = 0, ...
  ))
}

# ── shape of the result ──────────────────────────────────────────────────────

test_that("one row per design, sample size and parameter, plus joint power", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(60, 150), nosim = 10, seed = 3)

  expect_s3_class(out, "data.frame")
  expect_named(out, c(
    "design", "resps", "parameter", "power", "se",
    "truepar", "estimate", "sd", "converged"
  ))
  # 2 coefficients plus the joint row, at 2 sample sizes
  expect_equal(nrow(out), 3 * 2)
  expect_setequal(out$parameter, c("bprice", "bquality", "(all)"))
  expect_setequal(out$resps, c(60L, 150L))
})

test_that("power is a percentage and the true values are carried through", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(80, 200), nosim = 10, seed = 3)

  expect_true(all(out$power >= 0 & out$power <= 100))
  expect_equal(unique(out$truepar[out$parameter == "bprice"]), -0.06)
  expect_equal(unique(out$truepar[out$parameter == "bquality"]), 0.12)
  # the joint row describes no single parameter
  expect_true(all(is.na(out$truepar[out$parameter == "(all)"])))
})

test_that("the standard error of the power estimate is reported", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = 100, nosim = 20, seed = 3)

  expect_true(all(out$se >= 0, na.rm = TRUE))
  ## se of a share over n runs never exceeds 50 / sqrt(n)
  expect_true(all(out$se <= 50 / sqrt(20) + 1e-8, na.rm = TRUE))
  expect_true(all(out$converged <= 20))
})

test_that("rows come back sorted by design, parameter and sample size", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(200, 60, 120), nosim = 8, seed = 3)
  by_par <- split(out$resps, out$parameter)
  for (v in by_par) expect_equal(v, sort(v))
})

test_that("duplicate and unsorted sample sizes are tidied up", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(120, 60, 120), nosim = 6, seed = 3)
  expect_setequal(unique(out$resps), c(60L, 120L))
})

# ── it actually shows power rising ───────────────────────────────────────────

test_that("power rises with the sample size", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(60, 600), nosim = 25, seed = 11)

  small <- out$power[out$parameter == "bprice" & out$resps == 60]
  large <- out$power[out$parameter == "bprice" & out$resps == 600]
  expect_gt(large, small)
  expect_gt(large, 90)
})

test_that("the estimates recover the truth at every sample size", {
  skip_on_cran()
  td <- pc_folder()
  out <- pc(td, resps = c(150, 600), nosim = 20, seed = 11)
  coefs <- out[out$parameter != "(all)", ]
  expect_true(all(abs(coefs$estimate - coefs$truepar) < 0.05))
})

test_that("several designs are reported separately", {
  skip_on_cran()
  td <- pc_folder()
  saveRDS(pc_design[nrow(pc_design):1, ], file.path(td, "other.rds"))
  out <- pc(td, resps = c(80, 160), nosim = 8, seed = 3)
  expect_setequal(unique(out$design), c("d", "other"))
  expect_equal(nrow(out), 2 * 3 * 2)
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("resps must be a vector of sample sizes", {
  expect_error(power_curve(), "must be a vector of sample sizes")
  expect_error(power_curve(resps = "many"), "must be a vector of sample sizes")
  expect_error(power_curve(resps = numeric(0)), "must be a vector of sample sizes")
  expect_error(power_curve(resps = c(10, 2.5)), "whole number")
})

test_that("estimate = FALSE is refused, because power needs models", {
  td <- pc_folder()
  expect_error(
    pc(td, resps = 50, nosim = 2, estimate = FALSE),
    "`estimate` cannot be FALSE"
  )
})

test_that("progress is announced at verbose 1", {
  skip_on_cran()
  td <- pc_folder()
  expect_message(
    power_curve(
      resps = c(50, 100), nosim = 4, designpath = td, u = ul_pc,
      bcoeff = list(bprice = -0.06, bquality = 0.12),
      mode = "sequential", seed = 1, verbose = 1
    ),
    "Sample size 50 of 100"
  )
})

# ── the seed argument ────────────────────────────────────────────────────────

test_that("the same seed gives the same simulation", {
  td <- pc_folder()
  run <- function() {
    suppressMessages(sim_all(
      nosim = 3, resps = 100, designpath = td, u = ul_pc,
      bcoeff = list(bprice = -0.3, bquality = 0.5),
      estimate = FALSE, mode = "sequential", seed = 4242, verbose = 0
    ))$d[[1]]$CHOICE
  }
  expect_identical(run(), run())
})

test_that("different seeds give different simulations", {
  td <- pc_folder()
  run <- function(s) {
    suppressMessages(sim_all(
      nosim = 1, resps = 100, designpath = td, u = ul_pc,
      bcoeff = list(bprice = -0.3, bquality = 0.5),
      estimate = FALSE, mode = "sequential", seed = s, verbose = 0
    ))$d[[1]]$CHOICE
  }
  expect_false(identical(run(1), run(2)))
})

test_that("the seed is recorded in the arguments", {
  td <- pc_folder()
  res <- suppressMessages(sim_all(
    nosim = 1, resps = 60, designpath = td, u = ul_pc,
    bcoeff = list(bprice = -0.3, bquality = 0.5),
    estimate = FALSE, mode = "sequential", seed = 99, verbose = 0
  ))
  expect_equal(res$arguments$Seed, 99)

  without <- suppressMessages(sim_all(
    nosim = 1, resps = 60, designpath = td, u = ul_pc,
    bcoeff = list(bprice = -0.3, bquality = 0.5),
    estimate = FALSE, mode = "sequential", verbose = 0
  ))
  expect_true(is.na(without$arguments$Seed))
})

test_that("sim_choice takes a seed too", {
  f <- file.path(pc_folder(), "d.rds")
  run <- function() {
    suppressMessages(sim_choice(
      designfile = f, no_sim = 2, respondents = 80, u = ul_pc,
      bcoeff = list(bprice = -0.3, bquality = 0.5),
      estimate = FALSE, mode = "sequential", seed = 7, verbose = 0
    ))[[1]]$CHOICE
  }
  expect_identical(run(), run())
})

test_that("a non-integer seed is refused", {
  td <- pc_folder()
  expect_error(
    suppressMessages(sim_all(
      nosim = 1, resps = 60, designpath = td, u = ul_pc,
      bcoeff = list(bprice = -0.3, bquality = 0.5),
      estimate = FALSE, mode = "sequential", seed = 1.5, verbose = 0
    )),
    "whole number"
  )
})
