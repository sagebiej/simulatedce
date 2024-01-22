
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
install.packages("remotes")
remotes::install_gitlab(repo = "dj44vuri/simulateDCE" , host = "https://git.idiv.de")
```

## Example

This is a basic example for a simulation:

``` r
 library(simulateDCE)
library(rlang)
library(formula.tools)
#> 
#> Attaching package: 'formula.tools'
#> The following object is masked from 'package:rlang':
#> 
#>     env

# Designpath indicates the folder where all designs that should be simulated are stored. Can be either ngd files (for NGENE) or R objects for spdesign)
designpath<- system.file("extdata","SE_DRIVE" ,package = "simulateDCE")

# on your computer, it would be something like
# designpath <- "c:/myfancyDCE/Designs"


# number of respondents
resps =120

# number of simulations to run (about 200 is minimum if you want to be serious -- but it takes some time. always test your code with 2 simulations, and if it runs smoothly, go for more.)
nosim= 2 

# If you want to use different groups of respondents, use this. The following line means that you have one group of 70% size and one group of 30% size
decisiongroups=c(0,0.7,1)

# set the values of the parameters you want to use in the simulation
bpreis = -0.01
blade = -0.07
bwarte = 0.02

# If you want to do some manipulations in the design before you simulate, add a list called manipulations. Here, we devide some attributes by 10

manipulations = list(alt1.x2=     expr(alt1.x2/10),
                     alt1.x3=     expr(alt1.x3/10),
                     alt2.x2=     expr(alt2.x2/10),
                     alt2.x3=     expr(alt2.x3/10)
)


#place your utility functions here. We have two utility functions and two sets of utility functions. This is because we assume that 70% act according to the utility u1 and 30% act to the utility u2 (that is, they only decide according to the price and ignore the other attributes)
u<-list( u1 =

           list(
             v1 =V.1~  bpreis * alt1.x1 + blade*alt1.x2 + bwarte*alt1.x3   ,
             v2 =V.2~  bpreis * alt2.x1 + blade*alt2.x2 + bwarte*alt2.x3
           )

         ,
         u2 = list(  v1 =V.1~  bpreis * alt1.x1    ,
                     v2 =V.2~  bpreis * alt2.x1)

)

# specify the designtype "ngene" or "spdesign"
destype="ngene"


#lets go
sedrive <- sim_all()
#> Utility function used in simulation, ie the true utility: 
#> 
#> $u1
#> $u1$v1
#> V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3
#> 
#> $u1$v2
#> V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
#> 
#> 
#> $u2
#> $u2$v1
#> V.1 ~ bpreis * alt1.x1
#> 
#> $u2$v2
#> V.2 ~ bpreis * alt2.x1
#> 
#> 
#> Utility function used for Logit estimation with mixl: 
#> 
#> [1] "U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;"
#> New names:
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
#>  
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                7      80     2.5    10.0      60    20.0      10     1
#> 2  1               19      20     2.5     5.0      60     2.5       0     1
#> 3  1               30      20    10.0     5.0      80     5.0      10     1
#> 4  1               32      40    20.0     2.5      80     2.5       0     1
#> 5  1               39      40    20.0     0.0      80    10.0      10     1
#> 6  1               48      60     5.0     2.5      20     5.0      10     1
#>   group    V_1    V_2        e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.800  2.1016946  2.7841271  1.3266946  0.9841271      1
#> 2     1 -0.275 -0.775 -1.5595593  0.9636286 -1.8345593  0.1886286      2
#> 3     1 -0.800 -0.950  0.9803524 -0.1170033  0.1803524 -1.0670033      1
#> 4     1 -1.750 -0.975 -0.9363311 -0.2117177 -2.6863311 -1.1867177      2
#> 5     1 -1.800 -1.300  0.2762203  2.7548043 -1.5237797  1.4548043      2
#> 6     1 -0.900 -0.350  1.3557159  0.7504347  0.4557159  0.4004347      1
#> 
#>  
#>  This is Run number  1 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                7      80     2.5    10.0      60    20.0      10     1
#> 2  1               19      20     2.5     5.0      60     2.5       0     1
#> 3  1               30      20    10.0     5.0      80     5.0      10     1
#> 4  1               32      40    20.0     2.5      80     2.5       0     1
#> 5  1               39      40    20.0     0.0      80    10.0      10     1
#> 6  1               48      60     5.0     2.5      20     5.0      10     1
#>   group    V_1    V_2         e_1         e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.800 -0.40273624 -0.10075471 -1.1777362 -1.9007547      1
#> 2     1 -0.275 -0.775  0.48834933  1.92667239  0.2133493  1.1516724      2
#> 3     1 -0.800 -0.950  2.11775049 -0.36980424  1.3177505 -1.3198042      1
#> 4     1 -1.750 -0.975 -0.21469434 -0.05134613 -1.9646943 -1.0263461      2
#> 5     1 -1.800 -1.300  1.66727931  1.79712166 -0.1327207  0.4971217      2
#> 6     1 -0.900 -0.350 -0.01714569  1.40783581 -0.9171457  1.0578358      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#> -920.0 -992.5  605.0 
#> initial  value 998.131940 
#> iter   2 value 994.367289
#> iter   3 value 965.312071
#> iter   4 value 964.856191
#> iter   5 value 960.956078
#> iter   6 value 960.938761
#> iter   6 value 960.938748
#> iter   6 value 960.938748
#> final  value 960.938748 
#> converged
#> This is Run number  2 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                7      80     2.5    10.0      60    20.0      10     1
#> 2  1               19      20     2.5     5.0      60     2.5       0     1
#> 3  1               30      20    10.0     5.0      80     5.0      10     1
#> 4  1               32      40    20.0     2.5      80     2.5       0     1
#> 5  1               39      40    20.0     0.0      80    10.0      10     1
#> 6  1               48      60     5.0     2.5      20     5.0      10     1
#>   group    V_1    V_2        e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.800 -0.5753867  1.1246768 -1.3503867 -0.6753232      2
#> 2     1 -0.275 -0.775  0.9246181 -0.7283475  0.6496181 -1.5033475      1
#> 3     1 -0.800 -0.950  1.6224900 -0.8607449  0.8224900 -1.8107449      1
#> 4     1 -1.750 -0.975 -0.6271373 -0.9882054 -2.3771373 -1.9632054      2
#> 5     1 -1.800 -1.300  2.4317376  0.4448995  0.6317376 -0.8551005      1
#> 6     1 -0.900 -0.350  0.7924767 -0.6407351 -0.1075233 -0.9907351      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#>  -1740   -650    400 
#> initial  value 998.131940 
#> iter   2 value 992.981264
#> iter   3 value 992.638207
#> iter   4 value 992.633735
#> iter   5 value 973.931212
#> iter   6 value 972.829429
#> iter   7 value 972.824075
#> iter   7 value 972.824073
#> iter   7 value 972.824073
#> final  value 972.824073 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.04  0.00  -0.05  -0.04   0.00  0.00
#> est_bwarte           3    2   0.02  0.02   0.01   0.04   0.02  0.01
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.12  0.16   0.00   0.23   0.23  0.11
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE  TRUE 
#>    50    50 
#> Utility function used in simulation, ie the true utility: 
#> 
#> $u1
#> $u1$v1
#> V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3
#> 
#> $u1$v2
#> V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
#> 
#> 
#> $u2
#> $u2$v1
#> V.1 ~ bpreis * alt1.x1
#> 
#> $u2$v2
#> V.2 ~ bpreis * alt2.x1
#> 
#> 
#> Utility function used for Logit estimation with mixl: 
#> 
#> [1] "U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;"
#> New names:
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
#>  
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1               12      60     2.5     0.0      20    20.0      10     1
#> 2  1               16      20    10.0     5.0      40     5.0       0     1
#> 3  1               17      20    20.0     0.0      80    10.0      10     1
#> 4  1               25      60     5.0    10.0      20    20.0       5     1
#> 5  1               29      20     5.0    10.0      80     5.0       0     1
#> 6  1               32      40    10.0     2.5      80     2.5       5     1
#>   group    V_1    V_2        e_1        e_2         U_1        U_2 CHOICE
#> 1     1 -0.775 -1.400  0.4766761  0.2671861 -0.29832392 -1.1328139      1
#> 2     1 -0.800 -0.750  1.5938385  3.8773811  0.79383850  3.1273811      2
#> 3     1 -1.600 -1.300  1.3023142  0.4975025 -0.29768579 -0.8024975      1
#> 4     1 -0.750 -1.500  0.6538038  1.3130123 -0.09619617 -0.1869877      1
#> 5     1 -0.350 -1.150  1.1097526 -0.4923369  0.75975258 -1.6423369      1
#> 6     1 -1.050 -0.875 -0.3331011 -0.3042609 -1.38310109 -1.1792609      2
#> 
#>  
#>  This is Run number  1 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1               12      60     2.5     0.0      20    20.0      10     1
#> 2  1               16      20    10.0     5.0      40     5.0       0     1
#> 3  1               17      20    20.0     0.0      80    10.0      10     1
#> 4  1               25      60     5.0    10.0      20    20.0       5     1
#> 5  1               29      20     5.0    10.0      80     5.0       0     1
#> 6  1               32      40    10.0     2.5      80     2.5       5     1
#>   group    V_1    V_2        e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.400 -0.1853781  5.5643363 -0.9603781  4.1643363      2
#> 2     1 -0.800 -0.750  0.4194371 -0.8106381 -0.3805629 -1.5606381      1
#> 3     1 -1.600 -1.300  0.3558603  0.6710868 -1.2441397 -0.6289132      2
#> 4     1 -0.750 -1.500  1.6643355  0.1201793  0.9143355 -1.3798207      1
#> 5     1 -0.350 -1.150 -1.0250955  1.4973893 -1.3750955  0.3473893      2
#> 6     1 -1.050 -0.875  0.5258572  1.4751937 -0.5241428  0.6001937      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -2160.0  -795.0   397.5 
#> initial  value 998.131940 
#> iter   2 value 990.951216
#> iter   3 value 973.839132
#> iter   4 value 973.439833
#> iter   5 value 965.450645
#> iter   6 value 965.442009
#> iter   7 value 965.441990
#> iter   7 value 965.441990
#> iter   7 value 965.441990
#> final  value 965.441990 
#> converged
#> This is Run number  2 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1               12      60     2.5     0.0      20    20.0      10     1
#> 2  1               16      20    10.0     5.0      40     5.0       0     1
#> 3  1               17      20    20.0     0.0      80    10.0      10     1
#> 4  1               25      60     5.0    10.0      20    20.0       5     1
#> 5  1               29      20     5.0    10.0      80     5.0       0     1
#> 6  1               32      40    10.0     2.5      80     2.5       5     1
#>   group    V_1    V_2          e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.400  0.649258598 -0.6307138 -0.1257414 -2.0307138      1
#> 2     1 -0.800 -0.750  0.223937357  3.6350297 -0.5760626  2.8850297      2
#> 3     1 -1.600 -1.300  0.725491030 -0.9744697 -0.8745090 -2.2744697      1
#> 4     1 -0.750 -1.500 -0.003168789 -1.2301114 -0.7531688 -2.7301114      1
#> 5     1 -0.350 -1.150  2.699417707 -1.0402518  2.3494177 -2.1902518      1
#> 6     1 -1.050 -0.875 -0.294873234  0.5681585 -1.3448732 -0.3068415      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#>   -780  -1135    595 
#> initial  value 998.131940 
#> iter   2 value 987.994441
#> iter   3 value 971.376969
#> iter   4 value 971.238977
#> iter   5 value 961.110685
#> iter   6 value 961.070507
#> iter   7 value 961.070355
#> iter   7 value 961.070355
#> iter   7 value 961.070355
#> final  value 961.070355 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.00  -0.05  -0.05   0.00  0.00
#> est_bwarte           3    2   0.01  0.02   0.00   0.03   0.03  0.01
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.44  0.60   0.01   0.86   0.85  0.42
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE  TRUE 
#>    50    50 
#> Utility function used in simulation, ie the true utility: 
#> 
#> $u1
#> $u1$v1
#> V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3
#> 
#> $u1$v2
#> V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
#> 
#> 
#> $u2
#> $u2$v1
#> V.1 ~ bpreis * alt1.x1
#> 
#> $u2$v2
#> V.2 ~ bpreis * alt2.x1
#> 
#> 
#> Utility function used for Logit estimation with mixl: 
#> 
#> [1] "U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;"
#> New names:
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
#>  
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1
#>   group    V_1    V_2        e_1        e_2        U_1         U_2 CHOICE
#> 1     1 -1.150 -0.350 -0.6442381  0.4295041 -1.7942381  0.07950409      2
#> 2     1 -0.675 -1.500  3.1970276 -0.2347316  2.5220276 -1.73473164      1
#> 3     1 -0.925 -1.600  0.3004974 -0.3128014 -0.6245026 -1.91280136      1
#> 4     1 -0.875 -0.850  0.3732139 -0.4194695 -0.5017861 -1.26946953      1
#> 5     1 -0.550 -0.900 -0.9300505  0.3951342 -1.4800505 -0.50486581      2
#> 6     1 -1.550 -0.725  2.0446600  1.5811755  0.4946600  0.85617546      2
#> 
#>  
#>  This is Run number  1 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1
#>   group    V_1    V_2        e_1         e_2        U_1        U_2 CHOICE
#> 1     1 -1.150 -0.350  0.6290714  1.29877259 -0.5209286  0.9487726      2
#> 2     1 -0.675 -1.500  1.8066986 -0.97425446  1.1316986 -2.4742545      1
#> 3     1 -0.925 -1.600  2.6601535  1.12301507  1.7351535 -0.4769849      1
#> 4     1 -0.875 -0.850 -1.0099599  1.88168762 -1.8849599  1.0316876      2
#> 5     1 -0.550 -0.900  2.6583790  0.09147805  2.1083790 -0.8085220      1
#> 6     1 -1.550 -0.725 -1.1958958  1.93592122 -2.7458958  1.2109212      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1620.0 -1040.0   382.5 
#> initial  value 998.131940 
#> iter   2 value 992.061567
#> iter   3 value 967.996960
#> iter   4 value 967.486565
#> iter   5 value 962.185161
#> iter   6 value 962.173351
#> iter   6 value 962.173337
#> iter   6 value 962.173337
#> final  value 962.173337 
#> converged
#> This is Run number  2 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1
#>   group    V_1    V_2        e_1        e_2         U_1        U_2 CHOICE
#> 1     1 -1.150 -0.350 -0.7309834  1.1284057 -1.88098344  0.7784057      2
#> 2     1 -0.675 -1.500  0.8179671  1.0964035  0.14296711 -0.4035965      1
#> 3     1 -0.925 -1.600  1.3940762  1.1170727  0.46907619 -0.4829273      1
#> 4     1 -0.875 -0.850  0.5730054  3.1034379 -0.30199458  2.2534379      2
#> 5     1 -0.550 -0.900  1.9047401  4.1223683  1.35474011  3.2223683      2
#> 6     1 -1.550 -0.725  1.6121097 -0.7004755  0.06210972 -1.4254755      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1540.0  -942.5   492.5 
#> initial  value 998.131940 
#> iter   2 value 992.664599
#> iter   3 value 971.477616
#> iter   4 value 971.475065
#> iter   5 value 967.041341
#> iter   6 value 966.930735
#> iter   7 value 966.930651
#> iter   7 value 966.930651
#> iter   7 value 966.930651
#> final  value 966.930651 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.01  -0.05  -0.04   0.01  0.00
#> est_bwarte           3    2   0.01  0.01   0.00   0.01   0.02  0.01
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.54  0.57   0.14   0.94   0.80  0.40
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE 
#>   100 
#> Utility function used in simulation, ie the true utility: 
#> 
#> $u1
#> $u1$v1
#> V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3
#> 
#> $u1$v2
#> V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
#> 
#> 
#> $u2
#> $u2$v1
#> V.1 ~ bpreis * alt1.x1
#> 
#> $u2$v2
#> V.2 ~ bpreis * alt2.x1
#> 
#> 
#> Utility function used for Logit estimation with mixl: 
#> 
#> [1] "U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;"
#> New names:
#> • `Choice situation` -> `Choice.situation`
#> • `` -> `...10`
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
#>  
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                9      80     5.0       0      60    20.0    10.0     1
#> 2  1               12      60     2.5      10      40    20.0     0.0     1
#> 3  1               13      20    20.0      10      80     2.5     0.0     1
#> 4  1               70      80     5.0      10      20    20.0     2.5     1
#> 5  1               71      60    20.0      10      80    10.0     0.0     1
#> 6  1               73      60    10.0       0      40    20.0    10.0     1
#>   group    V_1    V_2        e_1         e_2        U_1        U_2 CHOICE
#> 1     1 -1.150 -1.800  0.2796585  0.34664924 -0.8703415 -1.4533508      1
#> 2     1 -0.575 -1.800  0.8234627 -0.32814816  0.2484627 -2.1281482      1
#> 3     1 -1.400 -0.975 -0.9750900  0.16466845 -2.3750900 -0.8103316      2
#> 4     1 -0.950 -1.550  1.4931059  0.50414150  0.5431059 -1.0458585      1
#> 5     1 -1.800 -1.500 -0.6878677 -0.07763469 -2.4878677 -1.5776347      2
#> 6     1 -1.300 -1.600  5.5219839  0.72366180  4.2219839 -0.8763382      1
#> 
#>  
#>  This is Run number  1 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                9      80     5.0       0      60    20.0    10.0     1
#> 2  1               12      60     2.5      10      40    20.0     0.0     1
#> 3  1               13      20    20.0      10      80     2.5     0.0     1
#> 4  1               70      80     5.0      10      20    20.0     2.5     1
#> 5  1               71      60    20.0      10      80    10.0     0.0     1
#> 6  1               73      60    10.0       0      40    20.0    10.0     1
#>   group    V_1    V_2        e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -1.150 -1.800 -0.7572712  3.5425547 -1.9072712  1.7425547      2
#> 2     1 -0.575 -1.800 -0.4468573  1.9263080 -1.0218573  0.1263080      2
#> 3     1 -1.400 -0.975 -0.6624036 -0.4899357 -2.0624036 -1.4649357      2
#> 4     1 -0.950 -1.550 -0.3297019  0.8584054 -1.2797019 -0.6915946      2
#> 5     1 -1.800 -1.500  3.2012567  0.2906511  1.4012567 -1.2093489      1
#> 6     1 -1.300 -1.600  1.6578812  1.1758015  0.3578812 -0.4241985      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -3380.0 -2975.0   927.5 
#> initial  value 998.131940 
#> iter   2 value 965.332966
#> iter   3 value 954.229410
#> iter   4 value 953.971301
#> iter   5 value 914.603698
#> iter   6 value 914.486744
#> iter   7 value 914.486127
#> iter   7 value 914.486126
#> iter   7 value 914.486126
#> final  value 914.486126 
#> converged
#> This is Run number  2 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block
#> 1  1                9      80     5.0       0      60    20.0    10.0     1
#> 2  1               12      60     2.5      10      40    20.0     0.0     1
#> 3  1               13      20    20.0      10      80     2.5     0.0     1
#> 4  1               70      80     5.0      10      20    20.0     2.5     1
#> 5  1               71      60    20.0      10      80    10.0     0.0     1
#> 6  1               73      60    10.0       0      40    20.0    10.0     1
#>   group    V_1    V_2        e_1         e_2         U_1       U_2 CHOICE
#> 1     1 -1.150 -1.800  1.1341457 -0.26483742 -0.01585434 -2.064837      1
#> 2     1 -0.575 -1.800 -0.7963652  0.04667859 -1.37136517 -1.753321      1
#> 3     1 -1.400 -0.975  0.2048657 -0.21097918 -1.19513427 -1.185979      2
#> 4     1 -0.950 -1.550  0.3900273 -0.41202049 -0.55997270 -1.962020      1
#> 5     1 -1.800 -1.500  1.2810776 -1.50479969 -0.51892242 -3.004800      1
#> 6     1 -1.300 -1.600  0.1633678  2.89194036 -1.13663222  1.291940      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -3440.0 -2690.0  1097.5 
#> initial  value 998.131940 
#> iter   2 value 968.342074
#> iter   3 value 949.110332
#> iter   4 value 948.984724
#> iter   5 value 924.015573
#> iter   6 value 923.964006
#> iter   7 value 923.963865
#> iter   7 value 923.963864
#> iter   7 value 923.963864
#> final  value 923.963864 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.04  0.00  -0.04  -0.04   0.01  0.00
#> est_bwarte           3    2   0.01  0.01   0.01   0.02   0.01  0.01
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.22  0.29   0.01   0.42   0.41  0.20
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE  TRUE 
#>    50    50 
#> Utility function used in simulation, ie the true utility: 
#> 
#> $u1
#> $u1$v1
#> V.1 ~ bpreis * alt1.x1 + blade * alt1.x2 + bwarte * alt1.x3
#> 
#> $u1$v2
#> V.2 ~ bpreis * alt2.x1 + blade * alt2.x2 + bwarte * alt2.x3
#> 
#> 
#> $u2
#> $u2$v1
#> V.1 ~ bpreis * alt1.x1
#> 
#> $u2$v2
#> V.2 ~ bpreis * alt2.x1
#> 
#> 
#> Utility function used for Logit estimation with mixl: 
#> 
#> [1] "U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;"
#> New names:
#> • `Choice situation` -> `Choice.situation`
#>  
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3
#> 1  1                1     1      80      20     2.5    20.0      10       5
#> 2  1                2     1      60      40     5.0    10.0       5      10
#> 3  1                3     1      60      20    20.0    20.0       0      10
#> 4  1                4     1      20      80    20.0     2.5       0      10
#> 5  1                5     1      40      80    10.0     5.0      10       5
#> 6  1                6     1      60      80     5.0     2.5       0       0
#>   group    V_1    V_2        e_1        e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.500  2.0013484  1.2686068  1.2263484 -0.2313932      1
#> 2     1 -0.850 -0.900 -0.2160302 -0.1560303 -1.0660302 -1.0560303      2
#> 3     1 -2.000 -1.400  0.3647555  1.7510865 -1.6352445  0.3510865      2
#> 4     1 -1.600 -0.775 -0.7047881 -0.1607646 -2.3047881 -0.9357646      2
#> 5     1 -0.900 -1.050 -0.2254104 -1.0337366 -1.1254104 -2.0837366      1
#> 6     1 -0.950 -0.975  0.2606067 -0.2131090 -0.6893933 -1.1881090      1
#> 
#>  
#>  This is Run number  1 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3
#> 1  1                1     1      80      20     2.5    20.0      10       5
#> 2  1                2     1      60      40     5.0    10.0       5      10
#> 3  1                3     1      60      20    20.0    20.0       0      10
#> 4  1                4     1      20      80    20.0     2.5       0      10
#> 5  1                5     1      40      80    10.0     5.0      10       5
#> 6  1                6     1      60      80     5.0     2.5       0       0
#>   group    V_1    V_2        e_1        e_2        U_1         U_2 CHOICE
#> 1     1 -0.775 -1.500 -0.1653281  1.5240819 -0.9403281  0.02408193      2
#> 2     1 -0.850 -0.900  1.7604798 -1.0703138  0.9104798 -1.97031379      1
#> 3     1 -2.000 -1.400 -0.2342433 -0.2639953 -2.2342433 -1.66399530      2
#> 4     1 -1.600 -0.775  2.8543824 -0.4878686  1.2543824 -1.26286864      1
#> 5     1 -0.900 -1.050  0.1456366  1.8645970 -0.7543634  0.81459700      2
#> 6     1 -0.950 -0.975  2.0912305 -1.0990719  1.1412305 -2.07407190      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#>  720.0 -947.5  160.0 
#> initial  value 998.131940 
#> iter   2 value 995.788784
#> iter   3 value 986.036982
#> iter   4 value 985.747072
#> iter   5 value 981.687192
#> iter   6 value 981.685153
#> iter   6 value 981.685152
#> iter   6 value 981.685152
#> final  value 981.685152 
#> converged
#> This is Run number  2 
#>  does sou_gis exist:  FALSE 
#> 
#>  dataset final_set exists:  FALSE 
#> 
#>  decisiongroups exists:  TRUE
#>    1    2 
#> 1007  433 
#> 
#>  data has been made 
#> 
#>  First few observations 
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3
#> 1  1                1     1      80      20     2.5    20.0      10       5
#> 2  1                2     1      60      40     5.0    10.0       5      10
#> 3  1                3     1      60      20    20.0    20.0       0      10
#> 4  1                4     1      20      80    20.0     2.5       0      10
#> 5  1                5     1      40      80    10.0     5.0      10       5
#> 6  1                6     1      60      80     5.0     2.5       0       0
#>   group    V_1    V_2        e_1         e_2        U_1        U_2 CHOICE
#> 1     1 -0.775 -1.500  3.8883463  0.05263711  3.1133463 -1.4473629      1
#> 2     1 -0.850 -0.900  5.5864242  0.57285677  4.7364242 -0.3271432      1
#> 3     1 -2.000 -1.400 -0.3144368  0.03415373 -2.3144368 -1.3658463      2
#> 4     1 -1.600 -0.775  0.3462736  2.49581876 -1.2537264  1.7208188      2
#> 5     1 -0.900 -1.050  0.1750145  0.10667154 -0.7249855 -0.9433285      1
#> 6     1 -0.950 -0.975  0.4323006 -0.48374785 -0.5176994 -1.4587478      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#>   -520   -870    380 
#> initial  value 998.131940 
#> iter   2 value 989.856026
#> iter   3 value 986.668308
#> iter   4 value 986.626161
#> iter   5 value 973.334324
#> iter   6 value 973.329236
#> iter   7 value 973.329219
#> iter   7 value 973.329219
#> iter   7 value 973.329219
#> final  value 973.329219 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.04  0.00  -0.05  -0.04   0.01  0.00
#> est_bwarte           3    2   0.01  0.01   0.00   0.01   0.01  0.01
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.60  0.56   0.20   0.99   0.79  0.39
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE 
#>   100 
#> 33.325 sec elapsed
#> $tic
#> elapsed 
#>   0.813 
#> 
#> $toc
#> elapsed 
#>  34.138 
#> 
#> $msg
#> logical(0)
#> 
#> $callback_msg
#> [1] "33.325 sec elapsed"
```

<img src="man/figures/README-example-1.png" width="100%" /><img src="man/figures/README-example-2.png" width="100%" /><img src="man/figures/README-example-3.png" width="100%" />
