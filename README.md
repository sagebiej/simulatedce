---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# simulateDCE

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/simulateDCE)](https://CRAN.R-project.org/package=simulateDCE)

[![R-CMD-check](https://github.com/sagebiej/simulatedce/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sagebiej/simulatedce/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of simulateDCE is to make it easy to simulate choice experiment datasets using designs from NGENE, `idefix` or `spdesign`. You have to store the design file(s) in a subfolder and need to specify certain parameters and the utility functions for the data generating process. The package is useful for:

1.  Test different designs in terms of statistical power, efficiency and unbiasedness

2.  To test the effects of deviations from RUM, e.g. heuristics, on model performance for different designs.

3.  In teaching, using simulated data is useful, if you want to know the data generating process. It helps to demonstrate Maximum likelihood and choice models, knowing exactly what you should expect.

4.  You can use simulation in pre-registration to justify your sample size and design choice.

5.  Before data collection, you can use simulated data to estimate the models you plan to use in the actual analysis. You can thus make sure, you can estimate all effects for given sample sizes.

## Installation

You can install simulateDCE directly from cran by 

``` r 
install.packages("simulateDCE") 
``` 


For the latest development version use this: 

``` r
install.packages("devtools")
devtools::install_git('https://github.com/sagebiej/simulateDCE', ref = "devel")
```


## Example

This is a basic example for a simulation. Two designs are read from a folder, and
70% of the simulated respondents follow full random utility maximisation while the
remaining 30% look only at price.


``` r
library(simulateDCE)

designpath <- system.file("extdata", "SE_DRIVE", package = "simulateDCE")

resps <- 120 # number of respondents
nosim <- 10 # number of simulations to run (about 500 is minimum)

decisiongroups <- c(0, 0.7, 1)

# place b coefficients into an r list:
bcoeff <- list(
  bpreis = -0.01,
  blade = -0.07,
  bwarte = 0.02
)

# rescale two attributes before the utilities are computed
manipulations <- list(
  alt1.x2 = rlang::expr(alt1.x2 / 10),
  alt1.x3 = rlang::expr(alt1.x3 / 10),
  alt2.x2 = rlang::expr(alt2.x2 / 10),
  alt2.x3 = rlang::expr(alt2.x3 / 10)
)

# place your utility functions here, one list per decision group
ul <- list(
  u1 = list(
    v1 = V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3,
    v2 = V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
  ),
  u2 = list(
    v1 = V.1 ~ bpreis * alt1.x1,
    v2 = V.2 ~ bpreis * alt2.x1
  )
)

sedrive <- sim_all(
  nosim = nosim, resps = resps, designtype = "ngene",
  designpath = designpath, u = ul, bcoeff = bcoeff,
  decisiongroups = decisiongroups, manipulations = manipulations,
  mode = "sequential"
)
#> 
#> Parameter specification:
#>   bpreis               fixed            value = -0.01
#>   blade                fixed            value = -0.07
#>   bwarte               fixed            value = 0.02
#> New names:
#> Summary table: n mean median sd min max range se bias rmse est_bpreis 10 -0.010
#> -0.009 0.002 -0.012 -0.007 0.005 0.001 0.000 0.002 est_blade 10 -0.047 -0.048
#> 0.005 -0.056 -0.038 0.018 0.002 0.023 0.023 est_bwarte 10 0.015 0.013 0.011
#> 0.003 0.033 0.030 0.003 -0.005 0.012 rob_pval0_bpreis 10 0.000 0.000 0.000
#> 0.000 0.001 0.001 0.000 NA NA rob_pval0_blade 10 0.000 0.000 0.000 0.000 0.000
#> 0.000 0.000 NA NA rob_pval0_bwarte 10 0.335 0.266 0.324 0.003 0.799 0.796 0.102
#> NA NA se_bpreis 10 0.002 0.002 0.000 0.002 0.002 0.000 0.000 NA NA se_blade 10
#> 0.008 0.008 0.001 0.007 0.010 0.002 0.000 NA NA se_bwarte 10 0.011 0.011 0.001
#> 0.010 0.012 0.002 0.000 NA NA coverage est_bpreis 100 est_blade 10 est_bwarte
#> 100 rob_pval0_bpreis NA rob_pval0_blade NA rob_pval0_bwarte NA se_bpreis NA
#> se_blade NA se_bwarte NA
#> Power results:
#> 
#> FALSE TRUE 70 30
#> New names:
#> Summary table: n mean median sd min max range se bias rmse est_bpreis 10 -0.010
#> -0.010 0.002 -0.013 -0.007 0.006 0.000 0.000 0.001 est_blade 10 -0.049 -0.049
#> 0.005 -0.056 -0.042 0.013 0.001 0.021 0.022 est_bwarte 10 0.011 0.013 0.008
#> 0.000 0.020 0.020 0.003 -0.009 0.011 rob_pval0_bpreis 10 0.000 0.000 0.001
#> 0.000 0.002 0.002 0.000 NA NA rob_pval0_blade 10 0.000 0.000 0.000 0.000 0.000
#> 0.000 0.000 NA NA rob_pval0_bwarte 10 0.370 0.211 0.362 0.034 0.996 0.962 0.115
#> NA NA se_bpreis 10 0.002 0.002 0.000 0.002 0.002 0.000 0.000 NA NA se_blade 10
#> 0.007 0.008 0.000 0.007 0.008 0.001 0.000 NA NA se_bwarte 10 0.010 0.010 0.001
#> 0.009 0.011 0.002 0.000 NA NA coverage est_bpreis 100 est_blade 10 est_bwarte
#> 90 rob_pval0_bpreis NA rob_pval0_blade NA rob_pval0_bwarte NA se_bpreis NA
#> se_blade NA se_bwarte NA
#> Power results:
#> 
#> FALSE TRUE 80 20
#> total time for simulation and estimation: 8.951 sec elapsed
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
```

The estimates, the true values and the variability across runs are collected for
every design side by side:


``` r
sa <- sedrive$summaryall
coef_rows <- sa$quantity == "estimate"

round(
  data.frame(
    truepar   = sa$truepar[coef_rows],
    small     = sa$effconstrsmall.mean[coef_rows],
    small_sd  = sa$effconstrsmall.sd[coef_rows],
    bayesian  = sa$bayeffdesignconstr.mean[coef_rows],
    bay_sd    = sa$bayeffdesignconstr.sd[coef_rows],
    row.names = sa$parname[coef_rows]
  ),
  4
)
#>        truepar   small small_sd bayesian bay_sd
#> bpreis   -0.01 -0.0097   0.0015  -0.0096 0.0019
#> blade    -0.07 -0.0488   0.0046  -0.0472 0.0052
#> bwarte    0.02  0.0114   0.0079   0.0149 0.0110
```

Power is reported per design and per coefficient:


``` r
sedrive$powa_by_par
#> $bayeffdesignconstr
#> bpreis  blade bwarte 
#>    100    100     30 
#> 
#> $effconstrsmall
#> bpreis  blade bwarte 
#>    100    100     20
```

## Random parameters

A coefficient can be a distribution instead of a number, so every simulated
respondent draws their own value:


``` r
bcoeff_mixed <- list(
  bpreis = list(dist = "neg_lognormal", meanlog = -4.6, sdlog = 0.4),
  blade  = list(dist = "normal", mean = -0.07, sd = 0.03),
  bwarte = 0.02
)

# what those specifications mean in the units of the coefficient
bcoeff_moments(bcoeff_mixed)
#>   parameter          dist        mean          sd
#> 1    bpreis neg_lognormal -0.01088902 0.004535783
#> 2     blade        normal -0.07000000 0.030000000
#> 3    bwarte         fixed  0.02000000 0.000000000
```

Pass it to `sim_all()` exactly as before. See
`vignette("random-parameters", package = "simulateDCE")`.

## Designs without blocks

If your design has no `Block` column, say how many choice sets each respondent
should see and they are drawn at random:


``` r
design <- data.frame(
  Choice.situation = 1:20,
  alt1.x1 = rep(c(1, 2, 3, 4, 5), 4),
  alt2.x1 = rep(c(5, 4, 3, 2, 1), 4)
)

drawn <- draw_sets(design, respondents = 100, sets_per_resp = 5)

# each respondent sees 5 distinct sets, and the design is used evenly
table(table(drawn$Choice.situation))
#> 
#> 25 
#> 20
```

## Checking a design before you use it

A design whose attributes always move together will still converge and still fill
in a summary table. The numbers will just be wrong. `check_design()` says so first:


``` r
check_design(design)
#> Design check
#> ------------------------------------------------------------ 
#>   20 choice situation(s), 2 alternative(s), 1 block(s)
#>   1 term(s) from the column names: x1
#>   identifying variation: rank 1 of 1, 5 distinct pattern(s)
#>   4 situation(s) offer identical alternatives
#> ------------------------------------------------------------ 
#>   ! 4 choice situation(s) offer two identical alternatives. Those choices
#>       are coin flips and carry no information.
```

## Vignettes

| Vignette | Covers |
|---|---|
| `simulateDCE` | Getting started, one complete simulation |
| `design-files` | Reading designs from Ngene, spdesign and idefix |
| `random-parameters` | Coefficients that vary across respondents |
| `decision-groups` | Subgroups following different decision rules |
| `manipulations` | Changing attributes before utilities are computed |
| `unblocked-designs` | Drawing choice sets at random per respondent |
| `results` | The output, and running large simulations |
| `estimators` | Availability, mixed logit, WTP space, and fitting your own model |
