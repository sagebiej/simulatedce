## extract_b_values() pulls priors out of an spdesign utility specification.

spd <- readRDS(system.file("extdata", "CSA", "linear", "BLIeff.RDS", package = "simulateDCE"))

test_that("returns a named list of numeric priors", {
  res <- extract_b_values(spd$utility)
  expect_type(res, "list")
  expect_true(all(vapply(res, is.numeric, logical(1))))
  expect_true(all(nzchar(names(res))))
  expect_gt(length(res), 0)
})

test_that("names keep the coefficient but drop the alternative prefix", {
  res <- extract_b_values(spd$utility)
  expect_true(all(grepl("^b_", names(res))))
  expect_false(any(grepl("^alt[0-9]+\\.", names(res))))
})

test_that("negative and decimal priors are parsed correctly", {
  utility <- c(alt1 = "b_cost[-0.05] * cost + b_qual[0.5] * qual")
  res <- extract_b_values(utility)
  expect_equal(res$b_cost, -0.05)
  expect_equal(res$b_qual, 0.5)
})

test_that("integer priors are parsed", {
  res <- extract_b_values(c(alt1 = "b_sq[-1] * sq"))
  expect_equal(res$b_sq, -1)
})

test_that("the result can be used directly as bcoeff", {
  res <- extract_b_values(spd$utility)
  expect_true(all(vapply(res, function(x) is.numeric(x) && length(x) == 1L, logical(1))))
})

test_that("a utility string without priors gives an empty result", {
  expect_length(extract_b_values(c(alt1 = "x1 + x2")), 0)
})
