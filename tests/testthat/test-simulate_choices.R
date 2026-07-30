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
    "defines .* decision group|Give one list of utility functions per group"
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
    "defines .* decision group|Give one list of utility functions per group"
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

#── the random utility identity ────────────────────────────────────────────────

df_three <- data.frame(
  ID      = rep(1:50, each = 4),
  price   = rep(c(10, 12, 20, 25), 50),
  quality = rep(c(1, 2, 3, 4), 50)
)

ut_three <- list(u1 = list(
  v1 = V.1 ~ bp * price,
  v2 = V.2 ~ bq * quality,
  v3 = V.3 ~ 0
))

beta_three <- list(bp = -0.2, bq = 0.5)

test_that("U_k equals V_k plus e_k for every alternative", {
  set.seed(1001)
  res <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  for (k in 1:3) {
    expect_equal(res[[paste0("U_", k)]],
                 res[[paste0("V_", k)]] + res[[paste0("e_", k)]])
  }
})

test_that("CHOICE is the argmax of the total utilities", {
  set.seed(1002)
  res <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  expect_equal(res$CHOICE, max.col(as.matrix(res[, c("U_1", "U_2", "U_3")])))
})

test_that("there is one V, e and U column per utility function", {
  res <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  expect_length(grep("^V_[0-9]+$", names(res)), 3)
  expect_length(grep("^e_[0-9]+$", names(res)), 3)
  expect_length(grep("^U_[0-9]+$", names(res)), 3)
})

test_that("the error term looks like a standard Gumbel draw", {
  set.seed(1003)
  big <- data.frame(ID = rep(1:2000, each = 2), price = rep(c(10, 20), 2000))
  res <- simulate_choices(big, list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0)),
                          bcoeff = list(bp = -0.2), verbose = 0)
  e <- c(res$e_1, res$e_2)
  expect_equal(mean(e), -digamma(1), tolerance = 0.05)   # Euler-Mascheroni, 0.5772
  expect_equal(sd(e), pi / sqrt(6), tolerance = 0.05)    # 1.2825
})

test_that("errors are drawn independently across alternatives", {
  set.seed(1004)
  big <- data.frame(ID = rep(1:2000, each = 2), price = rep(c(10, 20), 2000))
  res <- simulate_choices(big, list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0)),
                          bcoeff = list(bp = -0.2), verbose = 0)
  expect_equal(cor(res$e_1, res$e_2), 0, tolerance = 0.05)
})

test_that("a stronger price coefficient shifts choices away from the dear alternative", {
  set.seed(1005)
  # alternative 1 is always the cheap one
  d <- data.frame(ID = rep(1:1000, each = 2), alt1.price = 1, alt2.price = 30)
  ut <- list(u1 = list(v1 = V.1 ~ bp * alt1.price, v2 = V.2 ~ bp * alt2.price))
  mild   <- simulate_choices(d, ut, bcoeff = list(bp = -0.01), verbose = 0)
  strong <- simulate_choices(d, ut, bcoeff = list(bp = -1),    verbose = 0)
  expect_gt(mean(strong$CHOICE == 1), mean(mild$CHOICE == 1))
  expect_gt(mean(strong$CHOICE == 1), 0.9)
  # with a near-zero coefficient the two alternatives are close to a coin flip
  expect_gt(mean(mild$CHOICE == 1), 0.45)
  expect_lt(mean(mild$CHOICE == 1), 0.70)
})

#── repeated calls redraw the stochastic parts ─────────────────────────────────

test_that("consecutive calls produce different errors and different choices", {
  set.seed(1006)
  a <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  b <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  expect_false(identical(a$e_1, b$e_1))
  expect_false(identical(a$CHOICE, b$CHOICE))
  # the deterministic part is unchanged
  expect_equal(a$V_1, b$V_1)
})

test_that("consecutive calls redraw the respondent-level coefficients", {
  set.seed(1007)
  bc <- list(bp = list(dist = "normal", mean = -0.2, sd = 0.1))
  ut <- list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0))
  d  <- data.frame(ID = rep(1:30, each = 2), price = rep(c(10, 20), 30))
  a <- simulate_choices(d, ut, bcoeff = bc, verbose = 0)
  b <- simulate_choices(d, ut, bcoeff = bc, verbose = 0)
  expect_false(identical(a$bp, b$bp))
})

test_that("set.seed makes simulate_choices reproducible", {
  set.seed(1008)
  a <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  set.seed(1008)
  b <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  expect_identical(a, b)
})

#── verbose ────────────────────────────────────────────────────────────────────

test_that("verbose = 0 emits nothing", {
  expect_silent(simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0))
})

test_that("verbose = 2 announces a preprocess function", {
  d <- df_three
  expect_message(
    simulate_choices(d, ut_three, bcoeff = beta_three,
                     preprocess_function = function() data.frame(ID = 1, extra = 1),
                     verbose = 2),
    "Preprocess function has been executed"
  )
})

test_that("verbose = 3 reports the decision group counts", {
  ut2 <- list(
    g1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0, v3 = V.3 ~ 0),
    g2 = list(v1 = V.1 ~ 2 * bp * price, v2 = V.2 ~ 0, v3 = V.3 ~ 0)
  )
  expect_message(
    simulate_choices(df_three, ut2, bcoeff = beta_three,
                     decisiongroups = c(0, 0.5, 1), verbose = 3),
    "Respondents per decision group"
  )
})

#── manipulations ──────────────────────────────────────────────────────────────

test_that("a manipulation can reference another column", {
  d <- data.frame(ID = rep(1:5, each = 2),
                  price = rep(c(10, 20), 5),
                  factor = rep(c(2, 4), 5))
  ut <- list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0))
  res <- simulate_choices(d, ut, bcoeff = list(bp = -1),
                          manipulations = list(price = rlang::expr(price * factor)),
                          verbose = 0)
  expect_equal(res$price, d$price * d$factor)
  expect_equal(res$V_1, -1 * res$price)
})

test_that("several manipulations are applied in order", {
  d <- data.frame(ID = rep(1:5, each = 2), price = rep(c(10, 20), 5))
  ut <- list(u1 = list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0))
  res <- simulate_choices(d, ut, bcoeff = list(bp = -1),
                          manipulations = list(
                            price = rlang::expr(price / 10),
                            cheap = rlang::expr(price < 1.5)
                          ),
                          verbose = 0)
  expect_equal(res$price, d$price / 10)
  expect_equal(res$cheap, d$price / 10 < 1.5)
})

test_that("an empty manipulations list leaves the data untouched", {
  res_none  <- simulate_choices(df_three, ut_three, bcoeff = beta_three,
                                manipulations = list(), verbose = 0)
  expect_equal(res_none$price, df_three$price)
})

#── decision groups ────────────────────────────────────────────────────────────

test_that("three decision groups split the rows by the given proportions", {
  d <- data.frame(ID = 1:100, price = rep(c(10, 20), 50))
  ut <- rep(list(list(v1 = V.1 ~ bp * price, v2 = V.2 ~ 0)), 3)
  names(ut) <- c("g1", "g2", "g3")
  res <- simulate_choices(d, ut, bcoeff = list(bp = -1),
                          decisiongroups = c(0, 0.2, 0.5, 1), verbose = 0)
  expect_equal(as.vector(table(res$group)), c(20, 30, 50))
})

test_that("a single decision group puts everyone in group 1", {
  res <- simulate_choices(df_three, ut_three, bcoeff = beta_three, verbose = 0)
  expect_equal(unique(res$group), 1)
})

#── mixed and fixed coefficients side by side ──────────────────────────────────

test_that("every supported distribution works end to end", {
  set.seed(1009)
  bc <- list(
    bnorm = list(dist = "normal",        mean = -0.5, sd = 0.2),
    blogn = list(dist = "lognormal",     meanlog = 0, sdlog = 0.3),
    bnegl = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3),
    bunif = list(dist = "uniform",       min = -1, max = 1),
    btri  = list(dist = "triangular",    min = -1, max = 1, mode = 0),
    bfix  = 0.25
  )
  d  <- data.frame(ID = rep(1:20, each = 4), x = rnorm(80))
  ut <- list(u1 = list(
    v1 = V.1 ~ bnorm * x + blogn * x + bnegl * x + bunif * x + btri * x + bfix * x,
    v2 = V.2 ~ 0
  ))
  res <- simulate_choices(d, ut, bcoeff = bc, verbose = 0)

  expect_true(all(names(bc) %in% names(res)))
  expect_true(all(res$blogn > 0))
  expect_true(all(res$bnegl < 0))
  expect_true(all(res$bunif >= -1 & res$bunif <= 1))
  expect_true(all(res$btri  >= -1 & res$btri  <= 1))
  expect_true(all(res$bfix == 0.25))
})

test_that("a random coefficient stays constant within a respondent across all their sets", {
  set.seed(1010)
  d  <- data.frame(ID = rep(1:25, each = 8), x = rnorm(200))
  ut <- list(u1 = list(v1 = V.1 ~ bx * x, v2 = V.2 ~ 0))
  res <- simulate_choices(d, ut,
                          bcoeff = list(bx = list(dist = "normal", mean = 0, sd = 1)),
                          verbose = 0)
  expect_true(all(tapply(res$bx, res$ID, function(x) length(unique(x))) == 1))
  expect_equal(length(unique(res$bx)), 25)
})

test_that("random draws respect the requested moments across respondents", {
  set.seed(1011)
  d  <- data.frame(ID = rep(1:4000, each = 2), x = rep(c(1, 2), 4000))
  ut <- list(u1 = list(v1 = V.1 ~ bx * x, v2 = V.2 ~ 0))
  res <- simulate_choices(d, ut,
                          bcoeff = list(bx = list(dist = "normal", mean = -0.5, sd = 0.2)),
                          verbose = 0)
  per_resp <- tapply(res$bx, res$ID, `[`, 1)
  expect_equal(mean(per_resp), -0.5, tolerance = 0.03)
  expect_equal(sd(per_resp),    0.2, tolerance = 0.03)
})
