design_path <- system.file("extdata","agora", "altscf_eff.ngd" ,package = "simulateDCE")


test_that("wrong designtype", {
  expect_error(readdesign(design = design_path, designtype = "ng"),"Invalid value for design. Please provide either 'ngene' or 'spdesign'.")
})


test_that("file does not exist", {
  expect_error(readdesign(design = system.file("data-raw/agora/alcf_eff.ngd", package = "simulateDCE"), designtype = "ngene"),
               "does not exist in current working directory")
})

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "ngene"))
})

# test if autodetect ngd design

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path))
})


### Tests for spdesign

design_path <- system.file("extdata","CSA", "linear", "BLIbay.RDS" ,package = "simulateDCE")

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})

# Same Tests for spdesign, but detect automatically if it is spdesign

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path))
})


## trying objects that do not work

design_path <- system.file("extdata","testfiles", "nousefullist.RDS" ,package = "simulateDCE")

test_that("spdesign object is a list but does not contain the right element design", {
  expect_error(readdesign(design = design_path, designtype = "spdesign"),
               "list element is missing. Make sure to provide a ")
})


## test spdesign object containing original object

design_path <- system.file("extdata","ValuGaps", "des1.RDS" ,package = "simulateDCE")

test_that("all is correct with full spdesign objects", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})
