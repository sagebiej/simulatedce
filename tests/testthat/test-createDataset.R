## createDataset() had no direct test coverage. These tests pin down the
## contract: how the design is expanded to respondents, how blocks are handed
## out, and what the output looks like.

test_that("a design without Block gets a single block and every respondent sees every set", {
  design <- data.frame(Choice.situation = 1:6, a = 1:6)
  res <- createDataset(design, respondents = 4)

  expect_s3_class(res, "data.frame")
  expect_true("Block" %in% names(res))
  expect_equal(unique(res$Block), 1)
  expect_equal(nrow(res), 6 * 4)
  expect_equal(as.vector(table(res$ID)), rep(6, 4))
  # each respondent sees each choice situation exactly once
  expect_true(all(tapply(res$Choice.situation, res$ID, function(x) {
    setequal(x, 1:6) && !anyDuplicated(x)
  })))
})

test_that("ID and Choice.situation are moved to the front", {
  design <- data.frame(Choice.situation = 1:4, a = 1:4, b = 4:1)
  res <- createDataset(design, respondents = 2)
  expect_equal(names(res)[1:3], c("ID", "task", "Choice.situation"))
})

test_that("ID runs from 1 to respondents", {
  design <- data.frame(Choice.situation = 1:5, a = 1:5)
  res <- createDataset(design, respondents = 7)
  expect_equal(sort(unique(res$ID)), 1:7)
})

test_that("blocks are handed out round robin, one block per respondent", {
  design <- data.frame(
    Choice.situation = rep(1:4, 2),
    Block = rep(1:2, each = 4),
    x = 1:8
  )
  res <- createDataset(design, respondents = 6)

  # each respondent must sit in exactly one block
  blocks_per_resp <- tapply(res$Block, res$ID, function(x) length(unique(x)))
  expect_true(all(blocks_per_resp == 1))

  # and blocks alternate: 1, 2, 1, 2, ...
  expect_equal(
    as.vector(tapply(res$Block, res$ID, function(x) unique(x))),
    rep(1:2, 3)
  )

  # sets per respondent is nsets / nblocks
  expect_equal(as.vector(table(res$ID)), rep(4, 6))
})

test_that("a respondent sees each choice situation of their own block exactly once", {
  design <- data.frame(
    Choice.situation = rep(1:3, 3),
    Block = rep(1:3, each = 3),
    x = 1:9
  )
  res <- createDataset(design, respondents = 6)
  expect_true(all(tapply(res$Choice.situation, res$ID, anyDuplicated) == 0))
})

test_that("bundled blocked designs expand to respondents x sets-per-block", {
  design <- suppressMessages(readdesign(
    system.file("extdata", "SE_DRIVE", "effconstrsmall.ngd", package = "simulateDCE"),
    designtype = "ngene"
  ))
  nblocks <- length(unique(design$Block))
  setpp <- nrow(design) / nblocks

  res <- createDataset(design, respondents = nblocks * 4)
  expect_equal(nrow(res), nblocks * 4 * setpp)
  expect_equal(as.vector(table(res$ID)), rep(setpp, nblocks * 4))
})

test_that("respondents equal to the number of blocks gives one respondent per block", {
  design <- data.frame(
    Choice.situation = rep(1:2, 3),
    Block = rep(1:3, each = 2),
    x = 1:6
  )
  res <- createDataset(design, respondents = 3)
  expect_equal(nrow(res), 6)
  expect_equal(as.vector(tapply(res$Block, res$ID, unique)), 1:3)
})

test_that("all original design columns survive", {
  design <- data.frame(Choice.situation = 1:4, alt1.x = 1:4, alt2.x = 4:1)
  res <- createDataset(design, respondents = 3)
  expect_true(all(c("alt1.x", "alt2.x") %in% names(res)))
  expect_equal(res$alt1.x[res$ID == 1], design$alt1.x)
})
