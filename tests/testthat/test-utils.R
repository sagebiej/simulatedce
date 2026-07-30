## Coverage for the internal helpers in R/utils.R.

# ── modify_bcoeff_names() ─────────────────────────────────────────────────────

test_that("clean names are left alone and get a trivial lookup", {
  bcoeff <- list(bprice = -1, bqual = 2)
  res <- modify_bcoeff_names(bcoeff)

  expect_named(res, c("bcoeff", "bcoeff_lookup"))
  expect_equal(names(res$bcoeff), c("bprice", "bqual"))
  expect_equal(res$bcoeff_lookup$original, res$bcoeff_lookup$modified)
})

test_that("dots and underscores are stripped and recorded in the lookup", {
  bcoeff <- list(b.price = -1, b_qual = 2, bplain = 3)
  res <- modify_bcoeff_names(bcoeff)

  expect_equal(names(res$bcoeff), c("bprice", "bqual", "bplain"))
  expect_equal(res$bcoeff_lookup$original, c("b.price", "b_qual", "bplain"))
  expect_equal(res$bcoeff_lookup$modified, c("bprice", "bqual", "bplain"))
})

test_that("values and order are preserved while renaming", {
  bcoeff <- list(b.a = 1, b_b = 2, b.c = 3)
  res <- modify_bcoeff_names(bcoeff)
  expect_equal(unname(unlist(res$bcoeff)), c(1, 2, 3))
})

test_that("the lookup is attached as an attribute", {
  res <- modify_bcoeff_names(list(b.price = -1))
  expect_false(is.null(attr(res$bcoeff, "bcoeff_lookup")))
})

test_that("a second call is a no-op and says so", {
  first <- modify_bcoeff_names(list(b.price = -1, b_qual = 2))
  expect_message(modify_bcoeff_names(first$bcoeff, verbose = 3), "already cleaned")

  second <- suppressMessages(modify_bcoeff_names(first$bcoeff))
  expect_equal(names(second$bcoeff), names(first$bcoeff))
  expect_equal(second$bcoeff_lookup, first$bcoeff_lookup)
})

test_that("distribution specs survive renaming", {
  bcoeff <- list(b.price = list(dist = "normal", mean = -0.5, sd = 0.2), b_qual = 0.8)
  res <- modify_bcoeff_names(bcoeff)

  expect_equal(names(res$bcoeff), c("bprice", "bqual"))
  expect_equal(res$bcoeff$bprice$dist, "normal")
  expect_equal(res$bcoeff$bprice$mean, -0.5)
})

# ── vmsg() ────────────────────────────────────────────────────────────────────

test_that("vmsg prints only at or above the requested level", {
  expect_message(vmsg(1, 1, "shown"), "shown")
  expect_message(vmsg(3, 2, "shown"), "shown")
  expect_silent(vmsg(0, 1, "hidden"))
  expect_silent(vmsg(1, 2, "hidden"))
  expect_silent(vmsg(2, 3, "hidden"))
})

# ── find_dataframe() ──────────────────────────────────────────────────────────

test_that("find_dataframe digs a named data frame out of a nested list", {
  target <- data.frame(a = 1:2)
  nested <- list(x = 1, y = list(z = list(coefs = target)))
  expect_equal(find_dataframe(nested, "coefs"), target)
})

test_that("find_dataframe returns NULL when the name is absent", {
  expect_null(find_dataframe(list(a = 1, b = list(c = 2)), "coefs"))
})

test_that("find_dataframe ignores a match that is not a data frame", {
  expect_null(find_dataframe(list(coefs = 1:3), "coefs"))
})
