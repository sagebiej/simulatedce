## Checks the parallel path, which the automated suite deliberately does not.
##
## Under pkgload::load_all() a parallel worker loads the *installed* simulateDCE
## from the library rather than the sources you are editing, so a parallel test run
## with devtools::test() quietly exercises the wrong code. This script therefore
## installs the current source into a temporary library first and runs against that.
##
## Run it with:  Rscript tests/manual-tests/parallel.R
## It takes a few minutes, because the point is to use enough runs for parallel to
## be worth it.

## Find the package root, whether this is run from the root or from its own folder.
pkg_root <- normalizePath(".")
for (candidate in c(".", "..", "../..")) {
  if (file.exists(file.path(candidate, "DESCRIPTION")) && dir.exists(file.path(candidate, "R"))) {
    pkg_root <- normalizePath(candidate)
    break
  }
}
stopifnot(file.exists(file.path(pkg_root, "DESCRIPTION")))

lib <- file.path(tempdir(), "parallel_check_lib")
dir.create(lib, showWarnings = FALSE, recursive = TRUE)

message("Installing ", pkg_root, " into ", lib)
callr_ok <- system2("R",
  c("CMD", "INSTALL", "--no-docs", "-l", shQuote(lib), shQuote(pkg_root)),
  stdout = FALSE, stderr = FALSE
)
stopifnot(callr_ok == 0)

.libPaths(c(lib, .libPaths()))
library(simulateDCE)
message("testing simulateDCE ", utils::packageVersion("simulateDCE"))

## ---- a design big enough that estimation dominates -------------------------

set.seed(1)
profiles <- expand.grid(price = c(2, 4, 6, 8), quality = c(0, 1), origin = c(0, 1))
alt1 <- profiles[rep(seq_len(nrow(profiles)), 2), ]
n <- nrow(alt1)
repeat {
  partner <- sample(n)
  if (all(rowSums(abs(alt1 - alt1[partner, ])) > 0)) break
}

design <- data.frame(
  Choice.situation = seq_len(n),
  Block = rep(1:4, each = n / 4),
  alt1.price = alt1$price, alt1.quality = alt1$quality, alt1.origin = alt1$origin,
  alt2.price = alt1$price[partner], alt2.quality = alt1$quality[partner],
  alt2.origin = alt1$origin[partner]
)

design_dir <- file.path(tempdir(), "parallel_designs")
dir.create(design_dir, showWarnings = FALSE)
saveRDS(design, file.path(design_dir, "design.rds"))

ul <- list(u1 = list(
  v1 = V.1 ~ bprice * alt1.price + bquality * alt1.quality + borigin * alt1.origin,
  v2 = V.2 ~ bprice * alt2.price + bquality * alt2.quality + borigin * alt2.origin
))
bcoeff <- list(bprice = -0.3, bquality = 0.5, borigin = 0.4)

run <- function(...) {
  args <- utils::modifyList(
    list(
      designpath = design_dir, u = ul, bcoeff = bcoeff,
      estimate = TRUE, verbose = 0
    ),
    list(...)
  )
  do.call(sim_all, args)
}

check <- function(label, cond) {
  message(sprintf("%-58s %s", label, if (isTRUE(cond)) "ok" else "FAILED"))
  invisible(isTRUE(cond))
}

## ---- 1. each mode reproduces itself ----------------------------------------
##
## Parallel and sequential do NOT give the same numbers for the same seed: furrr
## draws from separate L'Ecuyer streams rather than from this session's stream.
## What must hold is that each mode reproduces itself, and that parallel results
## do not depend on the worker count.

set.seed(99)
seq_a <- run(nosim = 12, resps = 200, mode = "sequential")
set.seed(99)
seq_b <- run(nosim = 12, resps = 200, mode = "sequential")
check(
  "sequential reproduces itself",
  isTRUE(all.equal(seq_a$summaryall, seq_b$summaryall))
)

set.seed(99)
par_res <- run(nosim = 12, resps = 200, mode = "parallel", workers = 3)
set.seed(99)
par_again <- run(nosim = 12, resps = 200, mode = "parallel", workers = 3)
check(
  "parallel reproduces itself",
  isTRUE(all.equal(par_res$summaryall, par_again$summaryall))
)

## ---- 2. results do not depend on the number of workers ---------------------

set.seed(99)
par4 <- run(nosim = 12, resps = 200, mode = "parallel", workers = 4)
check(
  "results are the same with 3 and with 4 workers",
  isTRUE(all.equal(par_res$summaryall, par4$summaryall))
)

## The two modes should still agree about the estimates, just not run for run.
## With only 12 runs the Monte Carlo error is what it is, so compare against the
## reported standard error rather than to a fixed number of decimals.
gap <- abs(seq_a$summaryall$design.mean[1:3] - par_res$summaryall$design.mean[1:3])
tol <- 3 * (seq_a$summaryall$design.se[1:3] + par_res$summaryall$design.se[1:3])
check(
  "sequential and parallel agree within Monte Carlo error",
  all(gap < tol)
)

## ---- 3. the caller's plan survives -----------------------------------------

future::plan(future::multisession, workers = 2)
before <- future::nbrOfWorkers()
invisible(run(nosim = 4, resps = 100, mode = "parallel"))
after <- future::nbrOfWorkers()
future::plan("sequential")
check("the caller's future plan is handed back", identical(before, after))

## ---- 4. parallel actually pays off at scale --------------------------------

## Enough runs that the worker startup cost is clearly amortised. Around 200 runs
## of 600 respondents the two are within noise of each other on this machine, so a
## check there would pass or fail depending on what else is running.
message("\ntiming, 500 runs of 400 respondents")
t_seq <- system.time(run(nosim = 500, resps = 400, mode = "sequential",
                         keep_models = FALSE, keep_utilities = FALSE))
t_par <- system.time(run(nosim = 500, resps = 400, mode = "parallel", workers = 8,
                         keep_models = FALSE, keep_utilities = FALSE))
message(sprintf(
  "  sequential %.1fs, parallel on 8 workers %.1fs, speedup %.2fx",
  t_seq[["elapsed"]], t_par[["elapsed"]], t_seq[["elapsed"]] / t_par[["elapsed"]]
))
check("parallel is faster at 500 runs", t_par[["elapsed"]] < t_seq[["elapsed"]])

## ---- 5. random parameters and chunks survive the trip to a worker ----------

set.seed(7)
mixed <- run(
  nosim = 8, resps = 200, mode = "parallel", workers = 3, chunks = 2,
  bcoeff = list(
    bprice = list(dist = "neg_lognormal", meanlog = -1.3, sdlog = 0.3),
    bquality = 0.5, borigin = 0.4
  )
)
check("random parameters estimate in parallel with chunks",
      nrow(mixed$design$coefs) == 8 && !anyNA(mixed$summaryall$truepar[1:3]))

## ---- 6. sets_per_resp in parallel ------------------------------------------

unblocked <- design[, setdiff(names(design), "Block")]
ub_dir <- file.path(tempdir(), "parallel_unblocked")
dir.create(ub_dir, showWarnings = FALSE)
saveRDS(unblocked, file.path(ub_dir, "unblocked.rds"))

ub <- sim_all(
  nosim = 8, resps = 200, designpath = ub_dir, u = ul, bcoeff = bcoeff,
  sets_per_resp = 8, estimate = TRUE, mode = "parallel", workers = 3, verbose = 0
)
check("random choice sets work in parallel", nrow(ub$unblocked$coefs) == 8)

## ---- 7. the new features survive the trip to a worker ----------------------

## a custom estimator is a closure sent to the workers
plain_mnl <- function(data, spec) {
  mm <- spec$model_matrix()
  x <- mm$x
  av <- spec$availabilities
  chosen <- cbind(seq_len(nrow(data)), data$CHOICE)
  negll <- function(b) {
    v <- apply(x, c(1, 2), function(r) sum(r * b))
    v[av == 0] <- -Inf
    -sum(v[chosen] - log(rowSums(exp(v))))
  }
  fit <- stats::optim(rep(0, length(mm$terms)), negll, method = "BFGS", hessian = TRUE)
  se <- sqrt(diag(solve(fit$hessian)))
  list(
    coefficients = stats::setNames(fit$par, mm$terms),
    pvalues = stats::setNames(2 * stats::pnorm(-abs(fit$par / se)), mm$terms),
    converged = fit$convergence == 0,
    model = fit
  )
}

own <- run(nosim = 8, resps = 200, mode = "parallel", workers = 3, estimator = plain_mnl)
check("a custom estimator works in parallel", nrow(own$design$coefs) == 8)

mixed_par <- run(
  nosim = 6, resps = 300, mode = "parallel", workers = 3,
  model = "mixed", n_draws = 100,
  bcoeff = list(
    bprice = list(dist = "normal", mean = -0.3, sd = 0.2),
    bquality = 0.5, borigin = 0.4
  )
)
check(
  "a mixed logit works in parallel",
  "sigma_bprice" %in% mixed_par$summaryall$parname &&
    all(mixed_par$design$coefs$est_sigma_bprice > 0)
)

## availability columns must reach the workers too
av_design <- design
av_design$av1 <- 1
av_design$av2 <- rep(c(1, 0), length.out = nrow(design))
av_dir <- file.path(tempdir(), "parallel_av")
dir.create(av_dir, showWarnings = FALSE)
saveRDS(av_design, file.path(av_dir, "av.rds"))

av_res <- sim_all(
  nosim = 6, resps = 200, designpath = av_dir, u = ul, bcoeff = bcoeff,
  estimate = TRUE, mode = "parallel", workers = 3, verbose = 0
)
check("availability works in parallel", nrow(av_res$av$coefs) == 6)

message("\ndone")
