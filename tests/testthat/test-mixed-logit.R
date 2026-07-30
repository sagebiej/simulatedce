## Mixed logit estimation, where the model fitted matches the heterogeneity that
## was simulated.

mixed_design <- local({
  set.seed(21)
  prof <- expand.grid(price = c(2, 4, 6, 8), quality = c(0, 1))
  alt1 <- prof[rep(seq_len(nrow(prof)), 2), ]
  n <- nrow(alt1)
  repeat {
    partner <- sample(n)
    if (all(rowSums(abs(alt1 - alt1[partner, ])) > 0)) break
  }
  data.frame(
    Choice.situation = seq_len(n), Block = rep(1:2, each = n / 2),
    alt1.price = alt1$price, alt1.quality = alt1$quality,
    alt2.price = alt1$price[partner], alt2.quality = alt1$quality[partner]
  )
})

ul_mix <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality
))

mixed_folder <- function(envir = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = envir)
  saveRDS(mixed_design, file.path(td, "d.rds"))
  td
}

# ── building the utility script ──────────────────────────────────────────────

test_that("a normal random parameter gets a location, a scale and its own draw", {
  bc <- list(bprice = list(dist = "normal", mean = -0.4, sd = 0.25), bquality = 0.5)
  script <- build_mixed_script("U_1 = @bprice * $alt1_price + @bquality * $x ;", bc)

  expect_match(script, "@bprice + @sigma_bprice * draw_bprice", fixed = TRUE)
  # the fixed coefficient is untouched
  expect_match(script, "@bquality * $x", fixed = TRUE)
})

test_that("lognormal shapes are wrapped in an exponential, with the right sign", {
  pos <- build_mixed_script(
    "U_1 = @b * $x ;",
    list(b = list(dist = "lognormal", meanlog = 0, sdlog = 0.3))
  )
  neg <- build_mixed_script(
    "U_1 = @b * $x ;",
    list(b = list(dist = "neg_lognormal", meanlog = 0, sdlog = 0.3))
  )
  expect_match(pos, "(exp(@b + @sigma_b * draw_b))", fixed = TRUE)
  expect_match(neg, "(-exp(@b + @sigma_b * draw_b))", fixed = TRUE)
})

test_that("every occurrence of the coefficient is replaced", {
  bc <- list(b = list(dist = "normal", mean = -1, sd = 0.5))
  script <- build_mixed_script("U_1 = @b * $x1 ;U_2 = @b * $x2 ;", bc)
  expect_equal(lengths(regmatches(script, gregexpr("draw_b", script)))[[1]], 2L)
})

test_that("each random parameter gets a distinct draw token", {
  bc <- list(
    b1 = list(dist = "normal", mean = 0, sd = 1),
    b2 = list(dist = "normal", mean = 0, sd = 1)
  )
  script <- build_mixed_script("U_1 = @b1 * $x + @b2 * $y ;", bc)
  expect_match(script, "draw_b1", fixed = TRUE)
  expect_match(script, "draw_b2", fixed = TRUE)
})

# ── which shapes are supported ───────────────────────────────────────────────

test_that("the supported shapes are accepted and the rest are refused by name", {
  ok <- list(
    normal = list(dist = "normal", mean = 0, sd = 1),
    lognormal = list(dist = "lognormal", meanlog = 0, sdlog = 1),
    neg_lognormal = list(dist = "neg_lognormal", meanlog = 0, sdlog = 1)
  )
  for (nm in names(ok)) {
    expect_equal(check_mixed_supported(list(b = ok[[nm]])), "b", info = nm)
  }

  not_ok <- list(
    uniform = list(dist = "uniform", min = 0, max = 1),
    triangular = list(dist = "triangular", min = 0, max = 1, mode = 0.5),
    truncated_normal = list(dist = "truncated_normal", mean = 0, sd = 1, min = -1, max = 1)
  )
  for (nm in names(not_ok)) {
    expect_error(check_mixed_supported(list(b = not_ok[[nm]])),
      "cannot be written for", info = nm
    )
  }
})

test_that("a mixed model with no random coefficient is refused", {
  expect_error(
    check_mixed_supported(list(a = 1, b = -2)),
    "needs at least one random coefficient"
  )
})

test_that("only the random coefficients are listed as random", {
  bc <- list(
    a = 1, b = list(dist = "normal", mean = 0, sd = 1), cc = -2,
    d = list(dist = "lognormal", meanlog = 0, sdlog = 1)
  )
  expect_equal(mixed_random_params(bc), c("b", "d"))
})

# ── the true values a mixed logit should recover ─────────────────────────────

test_that("bcoeff_table reports moments for mnl and distribution parameters for mixed", {
  bc <- list(
    bfix = 0.4,
    bnorm = list(dist = "normal", mean = -0.5, sd = 0.2),
    blog = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4)
  )

  mnl <- bcoeff_table(bc, "mnl")
  expect_equal(mnl$parname, c("bfix", "bnorm", "blog"))
  expect_equal(mnl$truepar[2], -0.5)
  expect_equal(mnl$truepar[3], spec_mean(bc$blog)) # the mean, not meanlog

  mixed <- bcoeff_table(bc, "mixed")
  expect_equal(
    mixed$parname,
    c("bfix", "bnorm", "sigma_bnorm", "blog", "sigma_blog")
  )
  expect_equal(mixed$truepar[mixed$parname == "bnorm"], -0.5)
  expect_equal(mixed$truepar[mixed$parname == "sigma_bnorm"], 0.2)
  # the lognormal reports meanlog and sdlog, not the moments
  expect_equal(mixed$truepar[mixed$parname == "blog"], -1)
  expect_equal(mixed$truepar[mixed$parname == "sigma_blog"], 0.4)
})

# ── start values ─────────────────────────────────────────────────────────────

test_that("a scale parameter never starts at zero", {
  bc <- list(b = list(dist = "normal", mean = -0.4, sd = 0.25))
  st <- mixed_start_values(c(b = 0), c("b", "sigma_b"), bc, "b")
  expect_gt(st[["sigma_b"]], 0)
})

test_that("a lognormal location starts on the log scale", {
  bc <- list(b = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4))
  st <- mixed_start_values(c(b = -0.35), c("b", "sigma_b"), bc, "b")
  expect_equal(st[["b"]], log(0.35))
})

test_that("a normal location starts at the multinomial estimate", {
  bc <- list(b = list(dist = "normal", mean = -0.4, sd = 0.25))
  st <- mixed_start_values(c(b = -0.31), c("b", "sigma_b"), bc, "b")
  expect_equal(st[["b"]], -0.31)
})

test_that("fixed coefficients are carried over from the multinomial fit", {
  bc <- list(
    b = list(dist = "normal", mean = -0.4, sd = 0.25),
    bq = 0.5
  )
  st <- mixed_start_values(c(b = -0.31, bq = 0.48), c("b", "sigma_b", "bq"), bc, "b")
  expect_equal(st[["bq"]], 0.48)
})

# ── end to end ───────────────────────────────────────────────────────────────

test_that("a normal mixed logit recovers both the mean and the spread", {
  skip_on_cran()
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "normal", mean = -0.4, sd = 0.25), bquality = 0.5)

  res <- suppressMessages(sim_all(
    nosim = 6, resps = 400, designpath = td, u = ul_mix, bcoeff = bc,
    model = "mixed", n_draws = 150, mode = "sequential", seed = 5, verbose = 0
  ))
  sa <- res$summaryall
  est <- function(p) sa$d.mean[sa$parname == p]

  expect_lt(abs(est("bprice") - (-0.4)), 0.06)
  expect_lt(abs(est("sigma_bprice") - 0.25), 0.08)
  expect_lt(abs(est("bquality") - 0.5), 0.08)
  expect_equal(res$convergence$d$failed, 0)
})

test_that("the mixed logit beats the multinomial on the same heterogeneous data", {
  skip_on_cran()
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "normal", mean = -0.4, sd = 0.25), bquality = 0.5)

  run <- function(model) {
    res <- suppressMessages(sim_all(
      nosim = 6, resps = 400, designpath = td, u = ul_mix, bcoeff = bc,
      model = model, n_draws = 150, mode = "sequential", seed = 5, verbose = 0
    ))
    sa <- res$summaryall
    sa$d.mean[sa$parname == "bprice"]
  }

  ## the multinomial attenuates towards zero, the mixed logit does not
  expect_lt(abs(run("mixed") - (-0.4)), abs(run("mnl") - (-0.4)))
})

test_that("scale parameters are reported as positive whatever sign the optimiser found", {
  skip_on_cran()
  ## The likelihood sees only sigma * draw and the draws are symmetric, so +sigma
  ## and -sigma fit identically and either can come back.
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4), bquality = 0.5)

  res <- suppressMessages(sim_all(
    nosim = 10, resps = 400, designpath = td, u = ul_mix, bcoeff = bc,
    model = "mixed", n_draws = 150, mode = "sequential", seed = 8, verbose = 0
  ))
  expect_true(all(res$d$coefs$est_sigma_bprice > 0))
  sa <- res$summaryall
  expect_lt(abs(sa$d.mean[sa$parname == "sigma_bprice"] - 0.4), 0.1)
})

test_that("a lognormal mixed logit recovers meanlog and sdlog", {
  skip_on_cran()
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "neg_lognormal", meanlog = -1, sdlog = 0.4), bquality = 0.5)

  res <- suppressMessages(sim_all(
    nosim = 8, resps = 400, designpath = td, u = ul_mix, bcoeff = bc,
    model = "mixed", n_draws = 200, mode = "sequential", seed = 8, verbose = 0
  ))
  sa <- res$summaryall
  expect_lt(abs(sa$d.mean[sa$parname == "bprice"] - (-1)), 0.1)
  expect_lt(abs(sa$d.mean[sa$parname == "sigma_bprice"] - 0.4), 0.1)
})

test_that("the model and the number of draws are recorded", {
  skip_on_cran()
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "normal", mean = -0.4, sd = 0.25), bquality = 0.5)
  res <- suppressMessages(sim_all(
    nosim = 2, resps = 200, designpath = td, u = ul_mix, bcoeff = bc,
    model = "mixed", n_draws = 100, mode = "sequential", verbose = 0
  ))
  expect_equal(res$arguments$Model, "mixed")
  expect_equal(res$arguments$Draws, 100)
  expect_equal(res$d$model, "mixed")
})

test_that("sim_all reports which coefficients are random at verbose 1", {
  td <- mixed_folder()
  bc <- list(bprice = list(dist = "normal", mean = -0.4, sd = 0.25), bquality = 0.5)
  expect_message(
    sim_all(
      nosim = 1, resps = 100, designpath = td, u = ul_mix, bcoeff = bc,
      model = "mixed", n_draws = 50, mode = "sequential", verbose = 1
    ),
    "mixed logit with 50 draws, random: bprice"
  )
})
