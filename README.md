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
#> Summary table: n mean median sd min max range se est_bpreis 10 -0.009 -0.009
#> 0.002 -0.014 -0.006 0.008 0.001 est_blade 10 -0.045 -0.043 0.010 -0.061 -0.032
#> 0.030 0.003 est_bwarte 10 0.015 0.017 0.005 0.006 0.021 0.015 0.002
#> rob_pval0_bpreis 10 0.001 0.000 0.002 0.000 0.006 0.006 0.001 rob_pval0_blade
#> 10 0.000 0.000 0.000 0.000 0.000 0.000 0.000 rob_pval0_bwarte 10 0.204 0.106
#> 0.197 0.060 0.594 0.533 0.062
#> Power results:
#> 
#> FALSE TRUE 100 0
#> New names:
#> Summary table: n mean median sd min max range se est_bpreis 10 -0.010 -0.009
#> 0.002 -0.013 -0.008 0.005 0.001 est_blade 10 -0.049 -0.048 0.005 -0.057 -0.043
#> 0.013 0.002 est_bwarte 10 0.012 0.008 0.009 0.003 0.027 0.023 0.003
#> rob_pval0_bpreis 10 0.000 0.000 0.000 0.000 0.000 0.000 0.000 rob_pval0_blade
#> 10 0.000 0.000 0.000 0.000 0.000 0.000 0.000 rob_pval0_bwarte 10 0.383 0.422
#> 0.297 0.009 0.735 0.726 0.094
#> Power results:
#> 
#> FALSE TRUE 70 30
#> total time for simulation and estimation: 8.841 sec elapsed
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
```

The estimates, the true values and the variability across runs are collected for
every design side by side:


``` r
sa <- sedrive$summaryall
coef_rows <- !grepl("^rob_pval0_", sa$parname)

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
#> bpreis   -0.01 -0.0102   0.0021  -0.0091 0.0025
#> blade    -0.07 -0.0490   0.0049  -0.0452 0.0099
#> bwarte    0.02  0.0116   0.0089   0.0152 0.0053
```

Power is reported per design and per coefficient:


``` r
sedrive$powa_by_par
#> $bayeffdesignconstr
#> bpreis  blade bwarte 
#>    100    100      0 
#> 
#> $effconstrsmall
#> bpreis  blade bwarte 
#>    100    100     30
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
