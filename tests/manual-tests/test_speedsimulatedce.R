# ─────────────────────────────────────────────────────────────────────────────
# BENCHMARK: add NEW3 using dt[, .. := .., by=group]
# ─────────────────────────────────────────────────────────────────────────────

library(formula.tools); library(compiler)
library(dplyr);         library(purrr)
library(data.table);    library(tictoc)

# 1) Prepare data ------------------------------------------------------------
set.seed(2025)
n    <- 1e7
ngrp <- 10
df <- data.frame(
  price   = runif(n,  5, 100),
  quality = runif(n,  1,   5),
  group   = rep(1:ngrp, length.out = n)
)
bprice   <- -0.2; bquality <- 0.8
single_group <- list(
  V1 = V1 ~ bprice * price + bquality * quality,
  V2 = V2 ~ 0
)
utility_list <- rep(list(single_group), ngrp)

# 2) Helpers ---------------------------------------------------------------
by_formula <- function(eq) pick(everything()) %>% transmute(!!lhs(eq) := !!rhs(eq))
compile_one  <- function(fm){
  nm  <- as.character(lhs(fm)); rhs <- rhs(fm)
  fn  <- eval(bquote(function(df) with(df, .(rhs))))
  list(name = nm, fun = cmpfun(fn))
}
compile_utility_list <- function(u) lapply(u, function(fl){
  tmp <- lapply(fl, compile_one)
  setNames(lapply(tmp, `[[`, "fun"),
           vapply(tmp, `[[`, "",      "name"))
})

# 3) Methods --------------------------------------------------------------

old_group <- function(data, utility) {
  tic("OLD-GROUP")
  subs  <- split(data, data$group)
  subs2 <- map2(utility, subs, ~ mutate(.y, map_dfc(.x, by_formula)))
  out   <- bind_rows(subs2)
  toc(log = FALSE)
  out
}

new1_group <- function(data, utility) {
  ufuns <- compile_utility_list(utility)
  tic("NEW1-GROUP")
  subs  <- split(data, data$group)
  subs2 <- map2(ufuns, subs, ~ bind_cols(.y, lapply(.x, function(f) f(.y))))
  out   <- bind_rows(subs2)
  toc(log = FALSE)
  out
}

new2_group <- function(data, utility) {
  ufuns <- compile_utility_list(utility)
  dt    <- setDT(data)
  tic("NEW2-GROUP (loop)")
  for (g in seq_along(ufuns)) {
    fns <- ufuns[[g]]
    dt[group == g, (names(fns)) := lapply(fns, function(f) f(.SD))]
  }
  toc(log = FALSE)
  as.data.frame(dt)
}

new3_group <- function(data, utility) {
  ufuns    <- compile_utility_list(utility)
  dt       <- setDT(data)
  varnames <- names(ufuns[[1]])
  tic("NEW3-GROUP (by=group)")
  dt[, (varnames) := lapply(ufuns[[.BY$group]], function(f) f(.SD)), by = group]
  toc(log = FALSE)
  as.data.frame(dt)
}

# 4) Run & validate ---------------------------------------------------------
res_old  <- old_group(df, utility_list)
res_new1 <- new1_group(df, utility_list)
res_new2 <- new2_group(df, utility_list)
res_new3 <- new3_group(df, utility_list)

stopifnot(
  identical(res_old,  res_new1),
  identical(res_new1, res_new2),
  identical(res_new2, res_new3)
)
message("✅ All four methods agree on results.")
