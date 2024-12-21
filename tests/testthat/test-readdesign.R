design_path <- system.file("extdata","agora", "altscf_eff.ngd" ,package = "simulateDCE")


test_that("wrong designtype", {
  expect_error(readdesign(design = design_path, designtype = "ng"),"Invalid value for design. Please provide either NULL, 'ngene', 'spdesign'or 'idefix',  or do not use the argument 'designtype'. NULL lets us to guess the design.")
})


test_that("file does not exist", {
  expect_error(readdesign(design = system.file("data-raw/agora/alcf_eff.ngd", package = "simulateDCE"), designtype = "ngene"),
               "does not exist in current working directory")
})

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "ngene"))
})

# test if autodetect ngd design

test_that("expect message of guess", {
  expect_message(readdesign(design = design_path), "I guessed it is an ngene file")
})


test_that("with or without autodetct get same results for ngene", {

  t <-readdesign(design_path)
  t2 <-readdesign(design_path, designtype = "ngene")

  expect_equal(t,t2)


}

)


### Tests for spdesign

design_path <- system.file("extdata","CSA", "linear", "BLIbay.RDS" ,package = "simulateDCE")

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})






# Same Tests for spdesign, but detect automatically if it is spdesign

test_that("prints message for guessing", {
  expect_message(readdesign(design = design_path), "I assume it is a spdesign")
})

test_that("with or without autodetct get same results for spdesign", {

  t <-readdesign(design_path)
  t2 <-readdesign(design_path, designtype = "spdesign")

  expect_equal(t,t2)


}

          )


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
