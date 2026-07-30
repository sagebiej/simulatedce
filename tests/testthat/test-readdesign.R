design_path <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")


test_that("wrong designtype", {
  expect_error(readdesign(design = design_path, designtype = "ng"), "must be one of")
})


test_that("file does not exist", {
  expect_error(
    readdesign(design = system.file("data-raw/agora/alcf_eff.ngd", package = "simulateDCE"), designtype = "ngene"),
    "Design file not found"
  )
})

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "ngene"))
})

# test if autodetect ngd design

test_that("expect message of guess", {
  expect_message(readdesign(design = design_path), "ngene design")
})


test_that("with or without autodetct get same results for ngene", {
  t <- readdesign(design_path)
  t2 <- readdesign(design_path, designtype = "ngene")

  expect_equal(t, t2)
})


### Tests for spdesign

design_path <- system.file("extdata", "CSA", "linear", "BLIbay.RDS", package = "simulateDCE")

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})






# Same Tests for spdesign, but detect automatically if it is spdesign

test_that("prints message for guessing", {
  expect_message(readdesign(design = design_path), "spdesign design")
})

test_that("with or without autodetct get same results for spdesign", {
  t <- readdesign(design_path)
  t2 <- readdesign(design_path, designtype = "spdesign")

  expect_equal(t, t2)
})


## trying objects that do not work

design_path <- system.file("extdata", "testfiles", "nousefullist.RDS", package = "simulateDCE")

test_that("spdesign object is a list but does not contain the right element design", {
  expect_error(
    readdesign(design = design_path, designtype = "spdesign"),
    "'design' list element is missing"
  )
})


## test spdesign object containing original object

design_path <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")

test_that("all is correct with full spdesign objects", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})



### Tests for idefix

design_idefix <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")



test_that("all is correct with full idefix objects", {
  expect_no_error(readdesign(design_idefix, designtype = "idefix"))
})

#### new tests with kind help from chatgpt


library(testthat)
library(simulateDCE)

test_that("readdesign correctly identifies and processes ngene files", {
  # Arrange
  design_ngene <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")

  # Act
  result <- readdesign(design_ngene)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_ngene), "ngene design")
})

test_that("readdesign correctly identifies and processes spdesign files", {
  # Arrange
  design_sp <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")

  # Act
  result_default <- readdesign(design_sp)
  result_explicit <- readdesign(design_sp, designtype = "spdesign")

  # Assert
  expect_s3_class(result_default, "data.frame") # Check the result is a data frame
  expect_s3_class(result_explicit, "data.frame")
  expect_identical(result_default, result_explicit) # Default and explicit designtype should match
  expect_true("Choice.situation" %in% colnames(result_default)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result_default)[grepl("^alt", colnames(result_default))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result_default), 0) # Ensure the data frame has rows
  expect_gt(ncol(result_default), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_sp), "spdesign design")
})

test_that("readdesign correctly identifies and processes idefix files", {
  # Arrange
  design_idefix <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")

  # Act
  result <- readdesign(design_idefix)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_idefix), "idefix design")
})



test_that("readdesign returns a data frame with proper structure", {
  # Arrange
  design_ngene <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")

  # Act
  result <- readdesign(design_ngene)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result))

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
})

#── argument handling ──────────────────────────────────────────────────────────

test_that("designtype and destype cannot both be given", {
  d <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")
  expect_error(readdesign(d, designtype = "ngene", destype = "ngene"),
               "Use `designtype` only")
})

test_that("destype is deprecated but still honoured", {
  d <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")
  expect_message(readdesign(d, destype = "ngene"), "deprecated")
  expect_equal(
    suppressMessages(readdesign(d, destype = "ngene")),
    readdesign(d, designtype = "ngene")
  )
})

#── the ngene reader ───────────────────────────────────────────────────────────

test_that("ngene designs keep Choice.situation and Block and drop the Design column", {
  d <- readdesign(system.file("extdata", "SE_DRIVE", "effconstrsmall.ngd", package = "simulateDCE"),
                  designtype = "ngene")
  expect_true(all(c("Choice.situation", "Block") %in% names(d)))
  expect_false("Design" %in% names(d))
  expect_false(any(grepl("^\\.\\.\\.", names(d))))
})

test_that("ngene designs contain no empty trailing rows", {
  d <- readdesign(system.file("extdata", "SE_DRIVE", "effconstrsmall.ngd", package = "simulateDCE"),
                  designtype = "ngene")
  expect_false(anyNA(d$Choice.situation))
  expect_equal(sort(unique(d$Choice.situation)), seq_len(nrow(d)))
})

test_that("every ngene block holds the same number of choice situations", {
  d <- readdesign(system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE"),
                  designtype = "ngene")
  expect_equal(length(unique(table(d$Block))), 1L)
})

#── the spdesign reader ────────────────────────────────────────────────────────

test_that("spdesign designs get a Choice.situation numbered 1..n", {
  d <- readdesign(system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE"),
                  designtype = "spdesign")
  expect_equal(d$Choice.situation, seq_len(nrow(d)))
})

test_that("spdesign underscores become dots and block becomes Block", {
  d <- readdesign(system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE"),
                  designtype = "spdesign")
  expect_true("Block" %in% names(d))
  expect_true(any(grepl("^alt[0-9]+\\.", names(d))))
})

test_that("a bare data frame is accepted when spdesign is stated explicitly", {
  # Rbook/design1.RDS is a plain tibble, not an spdesign object
  raw <- readRDS(system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE"))
  expect_s3_class(raw, "data.frame")
  d <- readdesign(system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE"),
                  designtype = "spdesign")
  expect_equal(nrow(d), nrow(raw))
})

#── the idefix reader ──────────────────────────────────────────────────────────

test_that("idefix designs drop the no.choice rows and the cte columns", {
  d <- readdesign(system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE"),
                  designtype = "idefix")
  expect_false(any(grepl("cte", names(d))))
  expect_false(any(grepl("no.choice", names(d))))
  expect_true("Choice.situation" %in% names(d))
})

test_that("idefix Choice.situation is taken from the set number in the row names", {
  d <- readdesign(system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE"),
                  designtype = "idefix")
  expect_type(d$Choice.situation, "integer")
  expect_equal(sort(d$Choice.situation), seq_len(nrow(d)))
})

#── output shape shared by all readers ─────────────────────────────────────────

test_that("every reader returns one row per choice situation with unique ids", {
  files <- c(
    system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE"),
    system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE"),
    system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")
  )
  for (f in files) {
    d <- suppressMessages(readdesign(f))
    expect_equal(anyDuplicated(d$Choice.situation), 0L, info = basename(f))
    expect_gt(nrow(d), 0)
  }
})
