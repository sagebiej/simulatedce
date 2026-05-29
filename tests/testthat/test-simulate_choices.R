library(rlang)

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


test_that("simulate_choices() warns when deprecated setspp is used", {
  expect_warning(
    simulate_choices(
      data   = data,
      utility = ul,
      setspp = 4,
      bcoeff = bcoeff
    ),
    regexp = "is deprecated and ignored"
  )
})

test_that("simulate_choices() does not error", {
  expect_error(
    simulate_choices(data = data, bcoeff = bcoeff, u = ul),
    regexp = NA  # ← this is key: expect *no* error
  )
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


#### more simple tests

# assume your simulate_choices() is already loaded

#–– 1) small toy dataset & simple utility for testing ––#
df_small <- data.frame(
  ID      = rep(1:5, each = 2),
  price   = rep(c(10, 20), 5),
  quality = rep(c(1,  2), 5)
)

beta <- list(
  bprice   = -1,
  bquality =  2
)

# note: use V.1 / V.2 so that rename_with("\\.", "_") => V_1/V_2
ut <- list(
  u1 = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  )
)

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#

test_that("basic output: columns, types, and deterministic V", {
  res <- simulate_choices(
    data = df_small,
    utility = ut,
    bcoeff = beta
  )

  # must be a data.frame
  expect_s3_class(res, "data.frame")

  # must contain the original columns + group + V_*/e_*/U_*/CHOICE
  expect_true(all(
    c("price","quality","V_1","V_2","e_1","e_2","U_1","U_2","CHOICE")
    %in% names(res)
  ))

  # since V_1 = -1*price + 2*quality, V_2 = 0
  # and these are created *before* the random error is added,
  # they must match exactly:
  expect_equal(res$V_1, -1 * res$price + 2 * res$quality)
  expect_equal(res$V_2, rep(0, nrow(res)))
})

test_that("CHOICE always in {1,2} for two alternatives", {
  res <- simulate_choices(df_small, ut, bcoeff = beta)
  expect_true(all(res$CHOICE %in% c(1L, 2L)))
})

test_that("preprocess_function must be a function", {
  expect_error(
    simulate_choices(df_small, ut,  bcoeff = beta,
                     preprocess_function = 123),
    "`preprocess_function` must be a function"
  )
})

test_that("preprocess_function merges back on ID", {
  # create a df with explicit ID
  df2 <- df_small
  df2$ID <- 1:nrow(df2)

  # preprocessing returns only ID=1 with extra column
  prep <- function() data.frame(ID = 1, extra = 99)

  res <- simulate_choices(df2, ut, bcoeff = beta,
                          preprocess_function = prep)

  expect_true("extra" %in% names(res))
  expect_equal(res$extra[1], 99)
  expect_true(all(is.na(res$extra[-1])))
})

test_that("manipulations are applied before utility", {
  # e.g. triple the price
  manip <- list(price = expr(price * 3))

  res <- simulate_choices(df_small, ut,
                          bcoeff = beta, manipulations = manip)

  # price in result should be three times the original
  expect_equal(res$price, df_small$price * 3)

  # and V_1 = -1 * (price*3) + 2*quality
  expect_equal(res$V_1, -1 * res$price + 2 * res$quality)
})

test_that("decisiongroups produces correct group labels", {
  # 10 rows → break at 50% → two groups of 5 each

  ut2 <- list(
    u1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    ),
    u2 = list(
      v1 = V.1 ~ bprice * price + bquality * quality * 1.5,
      v2 = V.2 ~ 0
    )
  )
  dg   <- c(0, .5, 1)
  df10 <- df_small[1:10, ]
  df10$ID <- 1:10

  res <- simulate_choices(df10, ut2,
                          bcoeff = beta, decisiongroups = dg)

  # first 5 rows group==1, next 5 group==2
  expect_equal(res$group, rep(1:2, each = 5))
})

#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#


### MORE GROUP SPECIFIC TESTS


test_that("utilities are applied correctly by group", {
  # 10 rows, alternating prices and qualities
  df10 <- data.frame(
    ID      = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(
    bprice   = -1,
    bquality =  2
  )

  ut <- list(
    g1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,  # group 1
      v2 = V.2 ~ 0
    ),
    g2 = list(
      v1 = V.1 ~ bprice * price + 2 * bquality * quality,  # group 2 → stronger quality
      v2 = V.2 ~ 0
    )
  )

  res <- simulate_choices(
    df10,
    utility = ut,
    bcoeff  = beta,
    decisiongroups = c(0, 0.5, 1)  # → 2 groups of 5
  )

  # extract utilities
  res1 <- res[res$group == 1, ]
  res2 <- res[res$group == 2, ]

  # expected: V_1 = -1 * price + (2 * quality)
  v1_g1_expected <- with(res1, -1 * price + 2 * quality)
  expect_equal(res1$V_1, v1_g1_expected)

  # group 2 has double quality weight: V_1 = -1 * price + 4 * quality
  v1_g2_expected <- with(res2, -1 * price + 4 * quality)
  expect_equal(res2$V_1, v1_g2_expected)
})


test_that("missing utility group throws error", {
  df10 <- data.frame(
    ID = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(bprice = -1, bquality = 2)

  ut <- list(only_one = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  ))

  expect_error(
    simulate_choices(df10, ut,  bcoeff = beta,
                     decisiongroups = c(0, 0.5, 1)),
    "Length of `utility`.*does not match.*decision groups"
  )
})


#––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#
### RANDOM PARAMETER TESTS

df_rand <- data.frame(
  ID      = rep(1:20, each = 4),
  price   = rep(c(10, 10, 20, 20), 20),
  quality = rep(c(1, 2, 1, 2), 20)
)

ut_rand <- list(
  u1 = list(
    v1 = V.1 ~ bprice * price + bquality * quality,
    v2 = V.2 ~ 0
  )
)

beta_mixed <- list(
  bprice   = list(dist = "normal", mean = -0.5, sd = 0.2),
  bquality = 0.8
)

test_that("mixed bcoeff runs without error", {
  expect_no_error(
    simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  )
})

test_that("mixed path returns a data.frame with expected columns", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("ID", "price", "quality", "bprice", "bquality","V_1", "V_2", "CHOICE") %in% names(res)))
})

test_that("random parameter column is present and numeric", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expect_true("bprice" %in% names(res))
  expect_type(res$bprice, "double")
})

test_that("random parameter varies across respondents", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  # one draw per respondent — values should not all be identical
  draws_per_resp <- tapply(res$bprice, res$ID, function(x) x[1])
  expect_gt(length(unique(draws_per_resp)), 1)
})

test_that("random parameter is constant within a respondent", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  # all rows for the same ID should share the same bprice draw
  all_constant <- tapply(res$bprice, res$ID, function(x) length(unique(x)) == 1)
  expect_true(all(all_constant))
})

test_that("fixed parameter in mixed bcoeff is identical for all respondents", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expect_true("bquality" %in% names(res))
  expect_true(all(res$bquality == 0.8))
})

test_that("V_1 matches manual calculation using per-row bprice and bquality", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expected_V1 <- with(res, bprice * price + bquality * quality)
  expect_equal(res$V_1, expected_V1)
})

test_that("CHOICE is in valid range for mixed path", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expect_true(all(res$CHOICE %in% c(1L, 2L)))
})

test_that("group in data not covered by utility triggers error", {
  df10 <- data.frame(
    ID = 1:10,
    price   = rep(c(10, 20), 5),
    quality = rep(c(1,  2), 5)
  )

  beta <- list(bprice = -1, bquality = 2)

  ut <- list(
    g1 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    ),
    g2 = list(
      v1 = V.1 ~ bprice * price + bquality * quality,
      v2 = V.2 ~ 0
    )
  )

  # corrupt the decision group vector
  expect_error(
    simulate_choices(df10, ut, bcoeff = beta,
                     decisiongroups = c(0, 0.4, 0.8, 1)),  # 3 breaks but only 2 utils
    "Length of `utility`.*does not match.*decision groups"
  )
})


#── additional random parameter tests ──────────────────────────────────────────

test_that("all-random bcoeff runs without error", {
  beta_all_rand <- list(
    bprice   = list(dist = "normal",      mean = -0.5, sd = 0.2),
    bquality = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3)
  )
  expect_no_error(simulate_choices(df_rand, ut_rand, bcoeff = beta_all_rand))
})

test_that("neg_lognormal random param is always negative", {
  beta <- list(
    bprice   = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3),
    bquality = 0.8
  )
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta)
  expect_true(all(res$bprice < 0))
})

test_that("number of unique random draws equals number of respondents", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  n_unique_draws <- length(unique(tapply(res$bprice, res$ID, `[`, 1)))
  expect_equal(n_unique_draws, length(unique(res$ID)))
})

test_that("fixed-only bcoeff does not add coefficient columns to output", {
  beta_fixed <- list(bprice = -0.5, bquality = 0.8)
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_fixed)
  expect_false("bprice"   %in% names(res))
  expect_false("bquality" %in% names(res))
})

test_that("output row count matches input for mixed bcoeff", {
  res <- simulate_choices(df_rand, ut_rand, bcoeff = beta_mixed)
  expect_equal(nrow(res), nrow(df_rand))
})
