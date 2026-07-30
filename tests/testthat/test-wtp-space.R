## Specifications in willingness-to-pay space multiply two coefficients together,
## so they are not linear in the parameters. mixl estimates them anyway. What must
## not happen is check_design() reporting them as a broken design.

wtp_design <- local({
  set.seed(12)
  prof <- expand.grid(cost = c(2, 4, 6, 8), qual = c(0, 1))
  alt1 <- prof[rep(seq_len(nrow(prof)), 2), ]
  n <- nrow(alt1)
  repeat {
    partner <- sample(n)
    if (all(rowSums(abs(alt1 - alt1[partner, ])) > 0)) break
  }
  data.frame(
    Choice.situation = seq_len(n), Block = rep(1:2, each = n / 2),
    alt1.cost = alt1$cost, alt1.qual = alt1$qual,
    alt2.cost = alt1$cost[partner], alt2.qual = alt1$qual[partner]
  )
})

## utility = -lambda * (cost - wtp * qual), so lambda is the price coefficient and
## wtp is willingness to pay for quality, in money
ul_wtp <- list(u1 = list(
  v1 = V.1 ~ -blambda * (alt1.cost - bwtp * alt1.qual),
  v2 = V.2 ~ -blambda * (alt2.cost - bwtp * alt2.qual)
))
bc_wtp <- list(blambda = 0.4, bwtp = 2)

ul_pref <- list(u1 = list(
  v1 = V.1 ~ bcost * alt1.cost + bqual * alt1.qual,
  v2 = V.2 ~ bcost * alt2.cost + bqual * alt2.qual
))

wtp_folder <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(wtp_design, file.path(td, "d.rds"))
  td
}

# ── check_design knows the difference ────────────────────────────────────────

test_that("a WTP-space utility is reported as not linear in the coefficients", {
  res <- check_design(wtp_design, u = ul_wtp, bcoeff = bc_wtp)

  expect_false(res$linear_in_coefficients)
  expect_true(is.na(res$identified))
  expect_true(any(grepl("not linear in its coefficients", res$problems)))
  # and it must not claim the design is at fault
  expect_false(any(grepl("the design has to change", res$problems)))
  expect_false(any(grepl("No variation within choice situations", res$problems)))
})

test_that("a preference-space utility on the same design is judged normally", {
  res <- check_design(wtp_design, u = ul_pref, bcoeff = list(bcost = -0.4, bqual = 0.8))
  expect_true(res$linear_in_coefficients)
  expect_true(res$identified)
  expect_equal(res$rank, 2L)
})

test_that("the report says the rank does not apply", {
  out <- utils::capture.output(print(check_design(wtp_design, u = ul_wtp)))
  expect_true(any(grepl("NOT linear in the coefficients", out)))
  expect_true(any(grepl("does not apply", out)))
})

test_that("sim_all does not warn about a WTP-space specification", {
  td <- wtp_folder()
  expect_no_warning(
    suppressMessages(sim_all(
      nosim = 1, resps = 60, designpath = td, u = ul_wtp, bcoeff = bc_wtp,
      estimate = FALSE, mode = "sequential", verbose = 0
    ))
  )
})

test_that("sim_all still warns about a genuinely collinear design", {
  td <- withr::local_tempdir()
  collinear <- data.frame(
    Choice.situation = 1:8,
    alt1.a = c(1, 2, 3, 4, 1, 2, 3, 4), alt1.b = c(2, 4, 6, 8, 2, 4, 6, 8),
    alt2.a = c(4, 3, 2, 1, 3, 4, 1, 2), alt2.b = c(8, 6, 4, 2, 6, 8, 2, 4)
  )
  saveRDS(collinear, file.path(td, "d.rds"))
  ul <- list(u1 = list(
    v1 = V.1 ~ ba * alt1.a + bb * alt1.b,
    v2 = V.2 ~ ba * alt2.a + bb * alt2.b
  ))
  expect_warning(
    suppressMessages(sim_all(
      nosim = 1, resps = 40, designpath = td, u = ul,
      bcoeff = list(ba = 0.3, bb = 0.2),
      estimate = FALSE, mode = "sequential", verbose = 0
    )),
    "cannot identify the model"
  )
})

# ── the regressor is a difference, so a bare constant does not leak ──────────

test_that("a constant carrying no coefficient does not contaminate the regressors", {
  ul_const <- list(u1 = list(
    v1 = V.1 ~ 3 + bcost * alt1.cost,
    v2 = V.2 ~ bcost * alt2.cost
  ))
  res <- check_design(wtp_design, u = ul_const, bcoeff = list(bcost = -0.4))
  expect_true(res$linear_in_coefficients)
  expect_true(res$identified)
})

test_that("an alternative-specific constant is still identified", {
  ul_asc <- list(u1 = list(
    v1 = V.1 ~ basc + bcost * alt1.cost,
    v2 = V.2 ~ bcost * alt2.cost
  ))
  res <- check_design(wtp_design, u = ul_asc, bcoeff = list(basc = 0.2, bcost = -0.4))
  expect_true(res$identified)
  expect_setequal(res$terms, c("basc", "bcost"))
})

# ── it actually estimates ────────────────────────────────────────────────────

test_that("a WTP-space specification simulates and recovers its parameters", {
  skip_on_cran()
  td <- wtp_folder()
  res <- suppressMessages(sim_all(
    nosim = 10, resps = 400, designpath = td, u = ul_wtp, bcoeff = bc_wtp,
    mode = "sequential", seed = 4, verbose = 0
  ))
  sa <- res$summaryall
  est <- function(p) sa$d.mean[sa$parname == p]

  expect_lt(abs(est("blambda") - 0.4), 0.06)
  expect_lt(abs(est("bwtp") - 2), 0.3)
  expect_equal(res$convergence$d$failed, 0)
})

test_that("the WTP-space utility script keeps both coefficients as parameters", {
  db <- suppressMessages(simulate_choices(
    createDataset(wtp_design, 100), ul_wtp, bcoeff = bc_wtp, verbose = 0
  ))
  script <- transform_utility(ul_wtp, bc_wtp, db, "exact")

  expect_match(script, "@blambda")
  expect_match(script, "@bwtp")
  expect_match(script, "$alt1_cost", fixed = TRUE)
  expect_false(grepl("@$", script, fixed = TRUE))
})

test_that("willingness to pay is recovered directly rather than as a ratio", {
  skip_on_cran()
  td <- wtp_folder()

  ## in preference space you would divide two coefficients and inherit both errors;
  ## in WTP space the quantity is a parameter of its own
  in_wtp <- suppressMessages(sim_all(
    nosim = 10, resps = 400, designpath = td, u = ul_wtp, bcoeff = bc_wtp,
    mode = "sequential", seed = 9, verbose = 0
  ))
  sa <- in_wtp$summaryall
  expect_true("bwtp" %in% sa$parname)
  # and it comes with its own standard error and coverage
  expect_false(is.na(sa$d.coverage[sa$parname == "bwtp"]))
})
