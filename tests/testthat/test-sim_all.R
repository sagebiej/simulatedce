
library(rlang)
library(formula.tools)

designpath<- system.file("extdata","Rbook" ,package = "simulateDCE")

#notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps =40  # number of respondents
nosim=2 # number of simulations to run (about 500 is minimum)

#betacoefficients should not include "-"

bcoeff <-list(bsq=0.00,
              bredkite=-0.05,
              bdistance=0.50,
              bcost=-0.05,
              bfarm2=0.25,
              bfarm3=0.50,
              bheight2=0.25,
              bheight3=0.50)

destype <- "spdesign"


#place your utility functions here
ul<- list(u1= list(
  v1 =V.1 ~  bsq * alt1.sq,
  v2 =V.2 ~  bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 +  bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 =V.3 ~  bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 +  bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
)
)

test_that("u is not a list of lists", {
  expect_error(sim_all(nosim = nosim, resps=resps, destype = destype,
                       designpath = designpath, u=data.frame(u=" alp"), bcoeff = bcoeff),
               "must be provided and must be a list containing at least one list")
})

test_that("no value provided for  utility", {
  expect_error(sim_all(nosim = nosim, resps=resps, destype = destype,
                       designpath = designpath, bcoeff = bcoeff),
               "must be provided and must be a list containing at least one list")
})


test_that("wrong designtype", {
  expect_error(sim_all(nosim = nosim, resps=resps, destype = "ng",
                       designpath = designpath, u=ul, bcoeff = bcoeff),"Invalid value for design. Please provide either 'ngene' or 'spdesign'.")
})


test_that("folder does not exist", {
  expect_error(sim_all(nosim = nosim, resps=resps, destype = destype,
                       designpath = system.file("da/bullshit", package = "simulateDCE"), u=ul, bcoeff = bcoeff)
    ,
               "The folder where your designs are stored does not exist.")
})

test_that("seed setting makes code reproducible", {
  set.seed(3333)
  bcoeff <-list(bsq=0.00,
    bredkite=-0.05,
    bdistance=0.50,
    bcost=-0.05,
    bfarm2=0.25,
    bfarm3=0.50,
    bheight2=0.25,
    bheight3=0.50)
  result1 <- sim_all(nosim = nosim, resps = resps, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff)

  set.seed(3333)
  bcoeff <-list(bsq=0.00,
          bredkite=-0.05,
          bdistance=0.50,
          bcost=-0.05,
          bfarm2=0.25,
          bfarm3=0.50,
          bheight2=0.25,
          bheight3=0.50)
  result2 <- sim_all(nosim = nosim, resps = resps, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff)

  expect_identical(result1[["summaryall"]], result2[["summaryall"]])
})

test_that("No seed setting makes code results different", {

  result1 <- sim_all(nosim = nosim, resps = resps, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff)


  result2 <- sim_all(nosim = nosim, resps = resps, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff)

  expect_failure(expect_identical(result1[["summaryall"]], result2[["summaryall"]]))
})


########### Additional Tests ##############
test_that("bcoeff is provided", {
    expect_error(sim_all(nosim = nosim, resps = resps, destype = destype,
                         designpath = designpath, u = ul))
})

test_that("bcoeff contains valid values", {
  expect_error(sim_all(nosim = nosim, resps = resps, destype = destype,
                       designpath = designpath, u = ul, bcoeff = list(bsq = "invalid")))
})

test_that("bcoeff is a list", {
  expect_error(sim_all(nosim = nosim, resps = resps, destype = destype,
                       designpath = designpath, u = ul, bcoeff = "not a list")
  )
})

test_that("B coefficients in the utility functions dont match those in the bcoeff list", {
  expect_error(sim_all(nosim = nosim, resps=resps, destype = destype,
                       designpath = designpath, u = ul, bcoeff <- list(bWRONG = 0.00)))
})

test_that("Utility functions are valid", {
  expect_no_error(eval(ul$u1$v1))
  expect_no_error(eval(ul$u1$v2))
})

test_that("Design path must be a valid directory", {
  # Test case: designpath is not a character string
  expect_error(sim_all(nosim = nosim, resps = resps, destype = destype, designpath = 123, u = ul, bcoeff = bcoeff))

  # Test case: designpath does not exist
  expect_error(sim_all(nosim = nosim, resps = resps, destype = destype, designpath = '/nonexistent/path', u = ul, bcoeff = bcoeff))

  # Test case: designpath is not a directory
  expect_error(sim_all(nosim = nosim, resps = resps, destype = destype, designpath = 'path/to/a/file.txt', u = ul, bcoeff = bcoeff))
})

test_that("Resps must be an integer", {
  # Test case: resps is missing
  expect_error(sim_all(nosim = nosim, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))

  # Test case: resps is not an integer
  expect_error(sim_all(nosim = nosim, resps = "abc", destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))

  # Test case: resps is a numeric but not an integer
  expect_error(sim_all(nosim = nosim, resps = 1.5, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff))

})

test_that("Function exists in simulateDCE", {
  expect_true("sim_all" %in% ls("package:simulateDCE"))
})

test_that("Simulation results are reasonable", {

  result1 <- sim_all(nosim = nosim, resps = resps, destype = destype, designpath = designpath, u = ul, bcoeff = bcoeff)

  ## The function below is intended to find a dataframe called $coef in the output
  ## regardless of the output data structure, which can vary
  find_dataframe <- function(list_object, dataframe_name) {
    # Check if the current object is a list
    if (is.list(list_object)) {
      # Check if the dataframe_name exists in the names of the current list object
      if (dataframe_name %in% names(list_object)) {
        # Check if the object corresponding to dataframe_name is a data frame
        if (is.data.frame(list_object[[dataframe_name]])) {
          return(list_object[[dataframe_name]])  # Return the data frame if found
        }
      }

      # Recursively search through each element of the current list object
      for (element in list_object) {
        result <- find_dataframe(element, dataframe_name)
        if (!is.null(result)) {
          return(result)  # Return the data frame if found in any nested list
        }
      }
    }

    return(NULL)  # Return NULL if dataframe_name not found
  }

  # Now access the coef data frame to compare to the input
  coeffNestedOutput <- find_dataframe(result1, "coefs")

  for (variable in names(bcoeff)){
    ### Compare singular input value (hypothesis) with the average value of all iterations. ###
    ### This could be made more rigorous by testing each iteration rather than the mean or by changing the ###
    ### tolerance around the input value considered valid.
    input_value <- bcoeff[[variable]]
    mean_output_value <- mean(coeffNestedOutput[[paste0("est_", variable)]]) ## access the mean value of each iteration



    ##change this depending on how rigorous you want to be
    # Check if variable exists in coeffNestedOutput
    expect_true(paste0("est_", variable) %in% names(coeffNestedOutput),
                sprintf("Variable est_%s does not exist in coeffNestedOutput", variable))

    # Check if variable is numeric
    ## expect_is(coeffNestedOutput[[paste0("est_", variable)]], "numeric", sprintf("Variable est_%s in coeffNestedOutput is not numeric", variable))

    # Check if each entry in the variable column is numeric
    expect_true(all(sapply(coeffNestedOutput[[paste0("est_", variable)]], is.numeric)),
                sprintf("Variable est_%s in coeffNestedOutput contains non-numeric values", variable))
    expect_gt(mean_output_value, input_value - 1)
    expect_lt(mean_output_value, input_value + 1)
  }


})
