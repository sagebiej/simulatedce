## Tests run sequentially on purpose.
##
## Two reasons. CRAN allows at most two cores, and starting worker processes costs
## more than these small models take to estimate. More importantly, under
## pkgload::load_all() a parallel worker loads the *installed* simulateDCE from the
## library rather than the sources being developed, so a parallel test quietly
## exercises the wrong code. The parallel path is checked against an installed
## build in tests/manual-tests/parallel.R instead.
future::plan("sequential")

withr::defer(future::plan("sequential"), teardown_env())

## Shared fixtures ------------------------------------------------------------

## A tiny hand-built design. Fast, and it keeps the tests independent of the
## bundled example files.
toy_design <- function(nsets = 12, blocks = NULL) {
  d <- data.frame(
    Choice.situation = seq_len(nsets),
    alt1.price = rep(c(1, 2, 3), length.out = nsets),
    alt1.qual  = rep(c(0, 1), length.out = nsets),
    alt2.price = rep(c(3, 2, 1), length.out = nsets),
    alt2.qual  = rep(c(1, 0), length.out = nsets)
  )
  if (!is.null(blocks)) d$Block <- rep(seq_len(blocks), length.out = nsets)
  d
}

toy_utility <- function() {
  list(u1 = list(
    v1 = V.1 ~ bprice * alt1.price + bqual * alt1.qual,
    v2 = V.2 ~ bprice * alt2.price + bqual * alt2.qual
  ))
}

toy_bcoeff <- function() list(bprice = -0.6, bqual = 0.8)

#' Write a toy design to a temp folder and return the folder
toy_design_folder <- function(envir = parent.frame(), nsets = 12, blocks = 2,
                              name = "toy.rds") {
  dir <- withr::local_tempdir(.local_envir = envir)
  saveRDS(toy_design(nsets, blocks), file.path(dir, name))
  dir
}
