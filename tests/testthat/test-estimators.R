## The pluggable estimator interface, and a from-scratch conditional logit that
## cross-checks the default backend.

est_design <- local({
  set.seed(21)
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

ul_est <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality
))
bc_est <- list(bprice = -0.3, bquality = 0.5)

est_folder <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(est_design, file.path(td, "d.rds"))
  td
}

## The worked example from ?estimators, kept here so the documentation is tested.
plain_mnl <- function(data, spec) {
  mm <- spec$model_matrix()
  x <- mm$x
  av <- spec$availabilities
  chosen <- cbind(seq_len(nrow(data)), data$CHOICE)

  negll <- function(b) {
    v <- apply(x, c(1, 2), function(r) sum(r * b))
    v[av == 0] <- -Inf
    -sum(v[chosen] - log(rowSums(exp(v))))
  }

  fit <- stats::optim(rep(0, length(mm$terms)), negll, method = "BFGS", hessian = TRUE)
  se <- sqrt(diag(solve(fit$hessian)))

  list(
    coefficients = stats::setNames(fit$par, mm$terms),
    pvalues = stats::setNames(2 * stats::pnorm(-abs(fit$par / se)), mm$terms),
    converged = fit$convergence == 0,
    model = fit
  )
}

# ── resolving the argument ───────────────────────────────────────────────────

test_that("the built-in name resolves to a function", {
  expect_true(is.function(resolve_estimator("mixl")))
})

test_that("a function is passed through", {
  f <- function(data, spec) NULL
  expect_identical(resolve_estimator(f), f)
})

test_that("an unknown name is refused, with a suggestion", {
  expect_error(resolve_estimator("mixel"), "Unknown estimator 'mixel'")
  expect_error(resolve_estimator("mixel"), "Did you mean 'mixl'")
})

test_that("a function of the wrong arity is refused", {
  expect_error(resolve_estimator(function(data) NULL), "must take two arguments")
})

test_that("anything else is refused", {
  expect_error(resolve_estimator(42), "must be \"mixl\" or a function")
  expect_error(resolve_estimator(NULL), "must be \"mixl\" or a function")
})

# ── checking what comes back ─────────────────────────────────────────────────

test_that("a result that is not a list is refused", {
  expect_error(check_estimator_result(42, 1), "must return a list")
})

test_that("missing elements are named", {
  expect_error(
    check_estimator_result(list(coefficients = c(a = 1), converged = TRUE), 1),
    "missing `pvalues`"
  )
  expect_error(
    check_estimator_result(list(coefficients = c(a = 1)), 1),
    "missing `pvalues` and `converged`"
  )
})

test_that("unnamed vectors are refused", {
  expect_error(
    check_estimator_result(list(coefficients = 1, pvalues = c(a = 1), converged = TRUE), 1),
    "`coefficients` from the estimator must be a named numeric vector"
  )
  expect_error(
    check_estimator_result(list(coefficients = c(a = 1), pvalues = 1, converged = TRUE), 1),
    "`pvalues` from the estimator must be a named numeric vector"
  )
})

test_that("mismatched names are refused, showing both sets", {
  err <- tryCatch(
    check_estimator_result(
      list(coefficients = c(a = 1), pvalues = c(b = 0.1), converged = TRUE), 1
    ),
    error = conditionMessage
  )
  expect_match(err, "must have the same names")
  expect_match(err, "Coefficients: `a`")
  expect_match(err, "p values: `b`")
})

test_that("a non-logical converged flag is refused", {
  expect_error(
    check_estimator_result(
      list(coefficients = c(a = 1), pvalues = c(a = 0.1), converged = "yes"), 1
    ),
    "must be a single TRUE or FALSE"
  )
})

test_that("the run number appears in the message", {
  expect_error(check_estimator_result(42, 7), "run 7")
})

test_that("a valid result passes through unchanged", {
  res <- list(coefficients = c(a = 1), pvalues = c(a = 0.1), converged = TRUE, model = NULL)
  expect_identical(check_estimator_result(res, 1), res)
})

# ── the spec handed over ─────────────────────────────────────────────────────

test_that("the spec carries everything documented in ?estimators", {
  td <- est_folder()
  seen <- NULL
  spy <- function(data, spec) {
    seen <<- spec
    list(
      coefficients = c(bprice = 0, bquality = 0),
      pvalues = c(bprice = 1, bquality = 1),
      converged = TRUE, model = NULL
    )
  }
  suppressWarnings(suppressMessages(sim_all(
    nosim = 1, resps = 60, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = spy, mode = "sequential", verbose = 0
  )))

  expect_true(all(c(
    "utility", "bcoeff", "coefficient_names", "model", "n_draws", "n_alt",
    "availabilities", "script", "model_matrix"
  ) %in% names(seen)))
  expect_equal(seen$n_alt, 2L)
  expect_equal(seen$model, "mnl")
  expect_equal(seen$coefficient_names, names(bc_est))
  expect_true(is.function(seen$model_matrix))
})

test_that("the model matrix has the right shape and finds the renamed columns", {
  td <- est_folder()
  mm <- NULL
  spy <- function(data, spec) {
    mm <<- spec$model_matrix()
    list(
      coefficients = c(bprice = 0, bquality = 0),
      pvalues = c(bprice = 1, bquality = 1),
      converged = TRUE, model = NULL
    )
  }
  suppressWarnings(suppressMessages(sim_all(
    nosim = 1, resps = 60, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = spy, mode = "sequential", verbose = 0
  )))

  expect_equal(mm$terms, c("bprice", "bquality"))
  expect_equal(dim(mm$x), c(60 * 8, 2L, 2L))
  ## the price regressor must not be all zero: that was the symptom of the
  ## utility still pointing at the dotted column names
  expect_gt(stats::sd(mm$x[, 1, 1]), 0)
  expect_gt(stats::sd(mm$x[, 1, 2]), 0)
})

test_that("align_utility_to_data leaves numbers alone", {
  u <- list(v1 = V.1 ~ b * alt1.price + 0.5 * alt1.qual)
  aligned <- align_utility_to_data(u, data.frame(alt1_price = 1, alt1_qual = 1))
  txt <- paste(deparse(formula.tools::rhs(aligned[[1]])), collapse = "")

  expect_match(txt, "alt1_price", fixed = TRUE)
  expect_match(txt, "0.5", fixed = TRUE)
  expect_false(grepl("0_5", txt, fixed = TRUE))
})

# ── the cross-check ──────────────────────────────────────────────────────────

test_that("a from-scratch conditional logit agrees with the mixl backend", {
  skip_on_cran()
  td <- est_folder()

  with_mixl <- suppressMessages(sim_all(
    nosim = 8, resps = 300, designpath = td, u = ul_est, bcoeff = bc_est,
    mode = "sequential", seed = 7, verbose = 0
  ))
  with_own <- suppressMessages(sim_all(
    nosim = 8, resps = 300, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = plain_mnl, mode = "sequential", seed = 7, verbose = 0
  ))

  a <- with_mixl$summaryall
  b <- with_own$summaryall
  keep <- !grepl("^rob_pval0_", a$parname)

  expect_equal(a$parname, b$parname)
  expect_equal(a$d.mean[keep], b$d.mean[keep], tolerance = 1e-4)
})

test_that("power and convergence work with a custom estimator", {
  skip_on_cran()
  td <- est_folder()
  res <- suppressMessages(sim_all(
    nosim = 8, resps = 300, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = plain_mnl, mode = "sequential", seed = 7, verbose = 0
  ))

  expect_named(res$powa_by_par$d, c("bprice", "bquality"))
  expect_equal(res$convergence$d$runs, 8)
  expect_equal(res$convergence$d$failed, 0)
  expect_equal(nrow(res$estimates), 8)
})

test_that("a run reporting non-convergence is excluded and counted", {
  skip_on_cran()
  td <- est_folder()
  calls <- 0
  flaky <- function(data, spec) {
    calls <<- calls + 1
    res <- plain_mnl(data, spec)
    if (calls == 2) res$converged <- FALSE
    res
  }
  res <- suppressWarnings(suppressMessages(sim_all(
    nosim = 4, resps = 200, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = flaky, mode = "sequential", seed = 3, verbose = 0
  )))

  expect_equal(res$convergence$d$converged, 3)
  expect_equal(res$convergence$d$failed, 1)
  expect_equal(nrow(res$d$coefs), 3)
})

test_that("a run returning non-finite estimates is excluded as unusable", {
  skip_on_cran()
  td <- est_folder()
  calls <- 0
  broken <- function(data, spec) {
    calls <<- calls + 1
    res <- plain_mnl(data, spec)
    if (calls == 1) res$coefficients[1] <- Inf
    res
  }
  res <- suppressWarnings(suppressMessages(sim_all(
    nosim = 4, resps = 200, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = broken, mode = "sequential", seed = 3, verbose = 0
  )))

  expect_equal(res$convergence$d$unusable_estimates, 1)
  expect_equal(res$convergence$d$converged, 3)
})

test_that("an estimator that never works gives an error explaining what to check", {
  skip_on_cran()
  td <- est_folder()
  hopeless <- function(data, spec) {
    list(
      coefficients = c(bprice = NA_real_, bquality = NA_real_),
      pvalues = c(bprice = NA_real_, bquality = NA_real_),
      converged = TRUE, model = NULL
    )
  }
  expect_error(
    suppressMessages(sim_all(
      nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
      estimator = hopeless, mode = "sequential", verbose = 0
    )),
    "None of the 2 model\\(s\\)"
  )
})

test_that("the estimator used is recorded", {
  skip_on_cran()
  td <- est_folder()
  own <- suppressMessages(sim_all(
    nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = plain_mnl, mode = "sequential", verbose = 0
  ))
  built_in <- suppressMessages(sim_all(
    nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
    mode = "sequential", verbose = 0
  ))
  expect_equal(own$arguments$Estimator, "custom")
  expect_equal(built_in$arguments$Estimator, "mixl")
})

test_that("keep_models still hands back the backend's own object", {
  skip_on_cran()
  td <- est_folder()

  with_mixl <- suppressMessages(sim_all(
    nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
    mode = "sequential", verbose = 0
  ))
  expect_s3_class(with_mixl$d[[1]], "mixl")
  expect_true("data" %in% names(with_mixl$d[[1]]))

  with_own <- suppressMessages(sim_all(
    nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = plain_mnl, mode = "sequential", verbose = 0
  ))
  # optim() returns a plain list with these elements
  expect_true(all(c("par", "value", "convergence") %in% names(with_own$d[[1]])))
})

test_that("keep_models = FALSE drops the fitted objects for any estimator", {
  skip_on_cran()
  td <- est_folder()
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 100, designpath = td, u = ul_est, bcoeff = bc_est,
    estimator = plain_mnl, keep_models = FALSE, mode = "sequential", verbose = 0
  ))
  expect_true(all(nzchar(names(res$d))))
})
