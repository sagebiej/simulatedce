
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulateDCE

<!-- badges: start -->
<!-- badges: end -->

The goal of simulateDCE is to make it easy to simulate choice experiment
datasets using designs from NGENE or `spdesign`. You have to store the
design file in a subfolder and need to specify certain parameters and
the utility functions for the data generating process. The package is
useful for

1.  Test different designs in terms of statistical power, efficiency and
    unbiasedness

2.  To test the effects of deviations from RUM, e.g. heuristics, on
    model performance for different designs.

3.  In teaching, using simulated data is useful, if you want to know the
    data generating process. It helps to demonstrate Maximum likelihood
    and choice models, knowing exactly what you should expect.

4.  You can use simulation in pre-registration to justify your sample
    size and design choice.

5.  Before data collection, you can use simulated data to estimate the
    models you plan to use in the actual analysis. You can thus make
    sure, you can estimate all effects for given sample sizes.

## Installation

You can install the development version of simulateDCE from gitlab. You
need to install the `remotes` package first. The current version is
alpha and there is no version on cran:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
install.packages("remotes")
remotes::install_gitlab(repo = "dj44vuri/simulateDCE" , host = "https://git.idiv.de")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(simulateDCE)
## basic example code
```
