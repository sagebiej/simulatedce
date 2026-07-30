## Random allocation of choice sets for designs that are not blocked.

design12 <- data.frame(
  Choice.situation = 1:12,
  price = rep(c(1, 2, 3), 4),
  qual = rep(c(0, 1), 6)
)

# ── shape of the result ───────────────────────────────────────────────────────

test_that("the result has respondents x sets_per_resp rows", {
  set.seed(1)
  d <- draw_sets(design12, respondents = 30, sets_per_resp = 4)
  expect_equal(nrow(d), 30 * 4)
  expect_equal(as.vector(table(d$ID)), rep(4, 30))
})

test_that("ID, task and Choice.situation come first, and Block is added", {
  set.seed(2)
  d <- draw_sets(design12, respondents = 5, sets_per_resp = 3)
  expect_equal(names(d)[1:3], c("ID", "task", "Choice.situation"))
  expect_true("Block" %in% names(d))
  expect_equal(unique(d$Block), 1L)
})

test_that("task numbers the choice sets within each respondent", {
  set.seed(3)
  d <- draw_sets(design12, respondents = 7, sets_per_resp = 4)
  expect_true(all(tapply(d$task, d$ID, function(x) identical(x, 1:4))))
})

test_that("the design's own columns are carried over unchanged", {
  set.seed(4)
  d <- draw_sets(design12, respondents = 6, sets_per_resp = 5)
  # every row must match the design row it claims to be
  matched <- design12[match(d$Choice.situation, design12$Choice.situation), ]
  expect_equal(d$price, matched$price)
  expect_equal(d$qual, matched$qual)
})

test_that("row names are reset", {
  set.seed(5)
  d <- draw_sets(design12, respondents = 4, sets_per_resp = 3)
  expect_equal(rownames(d), as.character(seq_len(nrow(d))))
})

# ── the balanced scheme ──────────────────────────────────────────────────────

test_that("balanced gives every set the same number of appearances when it divides", {
  set.seed(6)
  # 30 respondents x 4 sets = 120 draws over 12 sets = 10 each
  d <- draw_sets(design12, respondents = 30, sets_per_resp = 4, scheme = "balanced")
  expect_equal(as.vector(table(d$Choice.situation)), rep(10, 12))
})

test_that("balanced keeps appearances within one of each other otherwise", {
  set.seed(7)
  # 7 respondents x 5 sets = 35 draws over 12 sets: 11 sets thrice, 1 twice
  d <- draw_sets(design12, respondents = 7, sets_per_resp = 5, scheme = "balanced")
  counts <- as.vector(table(d$Choice.situation))
  expect_equal(sum(counts), 35)
  expect_lte(max(counts) - min(counts), 1)
})

test_that("balanced never shows a respondent the same set twice", {
  set.seed(8)
  d <- draw_sets(design12, respondents = 50, sets_per_resp = 6, scheme = "balanced")
  expect_true(all(tapply(d$Choice.situation, d$ID, anyDuplicated) == 0))
})

test_that("balanced still randomises which respondent sees which set", {
  set.seed(9)
  a <- draw_sets(design12, respondents = 20, sets_per_resp = 4)
  b <- draw_sets(design12, respondents = 20, sets_per_resp = 4)
  expect_false(identical(a$Choice.situation, b$Choice.situation))
  # but both are balanced: 20 x 4 = 80 draws over 12 sets, so 6 or 7 each
  for (d in list(a, b)) {
    counts <- as.vector(table(d$Choice.situation))
    expect_equal(sum(counts), 80)
    expect_lte(max(counts) - min(counts), 1)
  }
})

test_that("balanced randomises the order the sets are presented in", {
  set.seed(10)
  d <- draw_sets(design12, respondents = 100, sets_per_resp = 4)
  # if presentation order were fixed, task 1 would always hold a low set number
  first_shown <- d$Choice.situation[d$task == 1]
  expect_gt(length(unique(first_shown)), 4)
})

test_that("sets_per_resp equal to the design size gives everyone every set", {
  set.seed(11)
  d <- draw_sets(design12, respondents = 5, sets_per_resp = 12)
  expect_true(all(tapply(d$Choice.situation, d$ID, function(x) setequal(x, 1:12))))
  expect_equal(as.vector(table(d$Choice.situation)), rep(5, 12))
})

# ── the random scheme ────────────────────────────────────────────────────────

test_that("random draws distinct sets per respondent", {
  set.seed(12)
  d <- draw_sets(design12, respondents = 40, sets_per_resp = 5, scheme = "random")
  expect_equal(nrow(d), 200)
  expect_true(all(tapply(d$Choice.situation, d$ID, anyDuplicated) == 0))
})

test_that("random is less balanced than balanced", {
  spread <- function(scheme) {
    set.seed(13)
    d <- draw_sets(design12, respondents = 40, sets_per_resp = 4, scheme = scheme)
    counts <- as.vector(table(d$Choice.situation))
    max(counts) - min(counts)
  }
  expect_lte(spread("balanced"), 1)
  expect_gt(spread("random"), 1)
})

# ── with replacement ─────────────────────────────────────────────────────────

test_that("with_replacement allows more sets per respondent than the design has", {
  set.seed(14)
  d <- draw_sets(design12, respondents = 5, sets_per_resp = 20,
                 scheme = "with_replacement")
  expect_equal(nrow(d), 100)
  expect_true(any(tapply(d$Choice.situation, d$ID, anyDuplicated) > 0))
})

# ── reproducibility ──────────────────────────────────────────────────────────

test_that("set.seed makes every scheme reproducible", {
  for (scheme in c("balanced", "random", "with_replacement")) {
    set.seed(15)
    a <- draw_sets(design12, respondents = 10, sets_per_resp = 4, scheme = scheme)
    set.seed(15)
    b <- draw_sets(design12, respondents = 10, sets_per_resp = 4, scheme = scheme)
    expect_identical(a, b, info = scheme)
  }
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("asking for more sets than the design has is refused with advice", {
  expect_error(
    draw_sets(design12, respondents = 5, sets_per_resp = 20),
    "only has 12 choice situation"
  )
  expect_error(
    draw_sets(design12, respondents = 5, sets_per_resp = 20),
    "with_replacement"
  )
})

test_that("a blocked design is refused, and says what to do instead", {
  blocked <- design12
  blocked$Block <- rep(1:2, each = 6)
  expect_error(
    draw_sets(blocked, respondents = 10, sets_per_resp = 3),
    "This design is blocked"
  )
  expect_error(
    draw_sets(blocked, respondents = 10, sets_per_resp = 3),
    "remove the `Block` column"
  )
})

test_that("a design with a single block level is accepted", {
  one_block <- design12
  one_block$Block <- 1
  expect_no_error(draw_sets(one_block, respondents = 10, sets_per_resp = 3))
})

test_that("bad counts are refused by name", {
  expect_error(draw_sets(design12, respondents = 0, sets_per_resp = 3), "`respondents`")
  expect_error(draw_sets(design12, respondents = 5, sets_per_resp = 0), "`sets_per_resp`")
  expect_error(draw_sets(design12, respondents = 5, sets_per_resp = 2.5), "whole number")
})

test_that("a design without Choice.situation is refused", {
  expect_error(
    draw_sets(data.frame(a = 1:4), respondents = 2, sets_per_resp = 2),
    "needs a `Choice.situation` column"
  )
})

test_that("an unknown scheme is refused", {
  expect_error(draw_sets(design12, 5, 3, scheme = "clever"), "should be one of")
})

# ── through createDataset and sim_all ────────────────────────────────────────

test_that("createDataset delegates to draw_sets when sets_per_resp is given", {
  set.seed(16)
  a <- createDataset(design12, respondents = 20, sets_per_resp = 4)
  set.seed(16)
  b <- draw_sets(design12, respondents = 20, sets_per_resp = 4)
  expect_identical(a, b)
})

test_that("createDataset without sets_per_resp still uses the blocks", {
  blocked <- design12
  blocked$Block <- rep(1:2, each = 6)
  res <- createDataset(blocked, respondents = 4)
  expect_equal(as.vector(table(res$ID)), rep(6, 4))
})

test_that("sim_all runs an unblocked design with sets_per_resp", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(toy_design(nsets = 20), file.path(td, "unblocked.rds"))

  res <- suppressMessages(sim_all(
    nosim = 3, resps = 60, designpath = td, u = toy_utility(),
    bcoeff = toy_bcoeff(), sets_per_resp = 5,
    estimate = TRUE, mode = "sequential", verbose = 0
  ))

  expect_equal(nrow(res[["unblocked"]][[1]]$data), 60 * 5)
  expect_equal(res[["arguments"]][["Sets per respondent"]], 5)
  expect_equal(res[["arguments"]][["Set sampling"]], "balanced")
  # the true parameters are recovered
  sa <- res[["summaryall"]]
  expect_lt(abs(sa$unblocked.mean[sa$parname == "bprice"] - (-0.6)), 0.3)
})

test_that("resample redraws the allocation for every run by default", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(toy_design(nsets = 20), file.path(td, "unblocked.rds"))

  run <- function(resample) {
    set.seed(42)
    r <- suppressMessages(sim_all(
      nosim = 3, resps = 40, designpath = td, u = toy_utility(),
      bcoeff = toy_bcoeff(), sets_per_resp = 5, resample = resample,
      estimate = FALSE, mode = "sequential", verbose = 0
    ))
    purrr::map(r[["unblocked"]], ~ .x$Choice_situation)
  }

  redrawn <- run(TRUE)
  expect_false(identical(redrawn[[1]], redrawn[[2]]))

  fixed <- run(FALSE)
  expect_identical(fixed[[1]], fixed[[2]])
  expect_identical(fixed[[2]], fixed[[3]])
})

test_that("sets_per_resp against a blocked design gives the blocked-design error", {
  skip_on_cran()
  td <- withr::local_tempdir()
  saveRDS(toy_design(nsets = 12, blocks = 2), file.path(td, "blocked.rds"))
  expect_error(
    suppressMessages(sim_all(
      nosim = 1, resps = 20, designpath = td, u = toy_utility(),
      bcoeff = toy_bcoeff(), sets_per_resp = 3,
      estimate = FALSE, mode = "sequential", verbose = 0
    )),
    "This design is blocked"
  )
})
