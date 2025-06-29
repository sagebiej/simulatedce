designpath <- system.file("extdata", "Rbook", package = "simulateDCE")

design <- system.file("extdata", "Rbook", "design1.RDS", package = "simulateDCE")

# notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps <- 40 # number of respondents
nosim <- 2 # number of simulations to run (about 500 is minimum)

# betacoefficients should not include "-"

bcoeff <- list(
  bsq = 0.00,
  bredkite = -0.05,
  bdistance = 0.50,
  bcost = -0.05,
  bfarm2 = 0.25,
  bfarm3 = 0.50,
  bheight2 = 0.25,
  bheight3 = 0.50
)

destype <- "spdesign"


# place your utility functions here
ul <- list(u1 = list(
  v1 = V.1 ~ bsq * alt1.sq,
  v2 = V.2 ~ bfarm2 * alt2.farm2 + bfarm3 * alt2.farm3 + bheight2 * alt2.height2 + bheight3 * alt2.height3 + bredkite * alt2.redkite + bdistance * alt2.distance + bcost * alt2.cost,
  v3 = V.3 ~ bfarm2 * alt3.farm2 + bfarm3 * alt3.farm3 + bheight2 * alt3.height2 + bheight3 * alt3.height3 + bredkite * alt3.redkite + bdistance * alt3.distance + bcost * alt3.cost
))


formattedes <- readdesign(design = design, designtype = "spdesign")
data <- simulateDCE::createDataset(formattedes, respondents = resps)

test_that("no error", {
  expect_no_error(simulate_choices(data = data, bcoeff = bcoeff, u = ul))
})

ds <- simulate_choices(data = data, bcoeff = bcoeff, u = ul)
test_that("random values are unique", {
  expect_equal(dim(table(table(ds$e_1))), 1)
  expect_equal(dim(table(table(ds$e_2))), 1)
  expect_equal(dim(table(table(ds$e_3))), 1)
  expect_equal(dim(table(table(ds$U_1))), 1)
  expect_equal(dim(table(table(ds$U_2))), 1)
  expect_equal(dim(table(table(ds$U_3))), 1)
  expect_true("CHOICE" %in% names(ds))
  expect_equal(length(unique(ds$CHOICE)), 3)
  expect_equal(length(unique(ds$Block)), 10)
  expect_equal(length(unique(ds$Choice_situation)), 100)
  expect_equal(length(unique(ds$ID)), 40)
  expect_equal(length(unique(ds$group)), 1)
})
