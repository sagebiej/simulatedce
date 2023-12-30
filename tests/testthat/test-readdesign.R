design_path <- system.file("extdata","agora", "altscf_eff.ngd" ,package = "simulateDCE")


test_that("wrong designtype", {
  expect_error(readdesign(design = design_path, designtype = "ng"),"Invalid value for design. Please provide either 'ngene' or 'spdesign'.")
})


test_that("file does not exist", {
  expect_error(readdesign(design = system.file("data-raw/agora/alcf_eff.ngd", package = "simulateDCE"), designtype = "ngene"),
               "does not exist in current working directory")
})

test_that("all is correct, but gives a warning", {
  expect_no_error(readdesign(design = design_path, designtype = "ngene"))
})


