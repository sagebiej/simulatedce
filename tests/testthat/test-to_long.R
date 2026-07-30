## to_long() serves mlogit, gmnl and clogit. apollo and mixl take the wide format
## the package already returns, so they need nothing from here.

wide_toy <- data.frame(
  ID = rep(1:3, each = 2),
  task = rep(1:2, 3),
  alt1_price = c(2, 4, 6, 8, 2, 4),
  alt1_qual = c(0, 1, 0, 1, 1, 0),
  alt2_price = c(8, 6, 4, 2, 6, 8),
  alt2_qual = c(1, 0, 1, 0, 0, 1),
  CHOICE = c(1, 2, 1, 1, 2, 1)
)

# ── shape ────────────────────────────────────────────────────────────────────

test_that("one row per alternative per choice occasion", {
  long <- to_long(wide_toy)
  expect_equal(nrow(long), nrow(wide_toy) * 2)
  expect_true(all(c("ID", "task", "alt", "chosen", "available") %in% names(long)))
})

test_that("the attributes collapse to one column each", {
  long <- to_long(wide_toy)
  expect_true(all(c("price", "qual") %in% names(long)))
  expect_false(any(grepl("^alt[0-9]", names(long))))
})

test_that("exactly one alternative is chosen per occasion", {
  long <- to_long(wide_toy)
  per_occasion <- tapply(long$chosen, paste(long$ID, long$task), sum)
  expect_true(all(per_occasion == 1))
})

test_that("the chosen row carries the chosen alternative's attributes", {
  long <- to_long(wide_toy)
  chosen <- long[long$chosen, ]
  chosen <- chosen[order(chosen$ID, chosen$task), ]

  expected_price <- ifelse(wide_toy$CHOICE == 1, wide_toy$alt1_price, wide_toy$alt2_price)
  expect_equal(chosen$price, expected_price)
})

test_that("rows come out sorted by respondent, occasion and alternative", {
  long <- to_long(wide_toy)
  expect_equal(long$ID, rep(1:3, each = 4))
  expect_equal(long$alt, rep(1:2, 6))
})

test_that("identifier columns come first", {
  long <- to_long(wide_toy)
  expect_equal(names(long)[1:5], c("ID", "task", "alt", "chosen", "available"))
})

test_that("dotted attribute names are handled too", {
  dotted <- wide_toy
  names(dotted) <- sub("_", ".", names(dotted))
  long <- to_long(dotted)
  expect_true(all(c("price", "qual") %in% names(long)))
  expect_equal(nrow(long), nrow(wide_toy) * 2)
})

# ── availability and alternatives without attributes ─────────────────────────

optout_design <- data.frame(
  Choice.situation = 1:8,
  alt1.price = c(2, 4, 6, 8, 2, 4, 6, 8), alt1.qual = c(0, 0, 1, 1, 1, 1, 0, 0),
  alt2.price = c(6, 2, 8, 4, 8, 6, 2, 4), alt2.qual = c(1, 1, 0, 1, 0, 0, 1, 0),
  av1 = 1, av2 = 1, av3 = rep(c(1, 0), 4)
)

ul_optout <- list(u1 = list(
  v1 = V.1 ~ bp * alt1.price + bq * alt1.qual,
  v2 = V.2 ~ bp * alt2.price + bq * alt2.qual,
  v3 = V.3 ~ bnone
))

simulate_optout <- function(envir = parent.frame(), ...) {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(optout_design, file.path(td, "d.rds"))
  suppressMessages(sim_all(
    nosim = 1, resps = 50, designpath = td, u = ul_optout,
    bcoeff = list(bp = -0.3, bq = 0.5, bnone = -1.5),
    estimate = FALSE, mode = "sequential", seed = 6, verbose = 0, ...
  ))$d[[1]]
}

test_that("an alternative with no attributes of its own is not dropped", {
  wide <- simulate_optout()
  long <- to_long(wide)

  expect_equal(nrow(long), nrow(wide) * 3)
  expect_setequal(unique(long$alt), 1:3)
  # the opt-out has no attributes, so they are missing rather than invented
  expect_true(all(is.na(long$price[long$alt == 3])))
  expect_false(any(is.na(long$price[long$alt != 3])))
})

test_that("availability becomes a single logical column", {
  wide <- simulate_optout()
  long <- to_long(wide)

  expect_type(long$available, "logical")
  expect_false(any(grepl("^av[._]?[0-9]+$", names(long))))
  # the opt-out is on offer in half the choice situations
  expect_equal(mean(long$available[long$alt == 3]), 0.5, tolerance = 0.05)
  expect_true(all(long$available[long$alt %in% 1:2]))
})

test_that("an unavailable alternative is never the chosen one", {
  wide <- simulate_optout()
  long <- to_long(wide)
  expect_equal(sum(long$chosen & !long$available), 0)
})

test_that("respondent-level information is repeated down the alternatives", {
  wide <- simulate_optout()
  long <- to_long(wide)

  expect_true("Choice_situation" %in% names(long))
  # every occasion appears once per alternative
  expect_true(all(table(paste(long$ID, long$task)) == 3))
})

test_that("random coefficient draws are carried through", {
  td <- withr::local_tempdir()
  saveRDS(optout_design, file.path(td, "d.rds"))
  wide <- suppressMessages(sim_all(
    nosim = 1, resps = 40, designpath = td, u = ul_optout,
    bcoeff = list(
      bp = list(dist = "normal", mean = -0.3, sd = 0.1),
      bq = 0.5, bnone = -1.5
    ),
    estimate = FALSE, mode = "sequential", seed = 7, verbose = 0
  ))$d[[1]]

  long <- to_long(wide)
  expect_true("bp" %in% names(long))
  # constant within a respondent, as it was in the wide data
  expect_true(all(tapply(long$bp, long$ID, function(x) length(unique(x))) == 1))
})

# ── the utilities ────────────────────────────────────────────────────────────

test_that("the utilities are dropped by default and kept on request", {
  wide <- simulate_optout()

  without <- to_long(wide)
  expect_false(any(c("V", "e", "U") %in% names(without)))

  with_them <- to_long(wide, keep_utilities = TRUE)
  expect_true(all(c("V", "e", "U") %in% names(with_them)))
  expect_equal(with_them$U, with_them$V + with_them$e)
})

test_that("the chosen alternative has the highest available utility", {
  wide <- simulate_optout()
  long <- to_long(wide, keep_utilities = TRUE)

  by_occasion <- split(long, paste(long$ID, long$task))
  ok <- vapply(by_occasion, function(g) {
    offered <- g[g$available, ]
    isTRUE(offered$chosen[which.max(offered$U)])
  }, logical(1))
  expect_true(all(ok))
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("a dataset with no CHOICE column is refused", {
  expect_error(to_long(wide_toy[, setdiff(names(wide_toy), "CHOICE")]), "no `CHOICE` column")
})

test_that("something that is not a data frame is refused", {
  expect_error(to_long("nope"), "must be one simulated dataset")
  expect_error(to_long(list(a = 1)), "must be one simulated dataset")
})

test_that("a dataset with nothing alternative-specific is refused", {
  expect_error(
    to_long(data.frame(ID = 1:2, CHOICE = c(NA, NA))),
    "how many alternatives"
  )
})
