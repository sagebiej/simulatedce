
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simulateDCE

<!-- badges: start -->
<!-- badges: end -->

The goal of simulateDCE is to make it easy to simulate choice experiment
datasets using designs from NGENE or `spdesign`. You have to store the
design file in a subfolder and need to specify certain parameters and
the utility functions for the data generating process. The package is
useful for:

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


rm(list=ls())
library(simulateDCE)
library(rlang)
library(formula.tools)


library(rlang)

designpath<- system.file("extdata","SE_DRIVE" ,package = "simulateDCE")

resps =120  # number of respondents
nosim= 2 # number of simulations to run (about 500 is minimum)






# bcoeff <- list(
#   bpreis = -0.036,
#   blade  = -0.034,
#   bwarte = -0.049)


decisiongroups=c(0,0.7,1)

# wrong parameters

# place b coefficients into an r list:
bcoeff  = list(
  bpreis = -0.01,
  blade = -0.07,
  bwarte = 0.02)

manipulations = list(alt1.x2=     expr(alt1.x2/10),
                     alt1.x3=     expr(alt1.x3/10),
                     alt2.x2=     expr(alt2.x2/10),
                     alt2.x3=     expr(alt2.x3/10)
)


#place your utility functions here
ul<-list( u1 =

           list(
             v1 =V.1~  bpreis * alt1.x1 + blade*alt1.x2 + bwarte*alt1.x3   ,
             v2 =V.2~  bpreis * alt2.x1 + blade*alt2.x2 + bwarte*alt2.x3
           )

         ,
         u2 = list(  v1 =V.1~  bpreis * alt1.x1    ,
                     v2 =V.2~  bpreis * alt2.x1)

)


destype="ngene"

sedrive <- sim_all(nosim = nosim, resps=resps, destype = destype,
                   designpath = designpath, u=ul, bcoeff = bcoeff)
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1        e_2        U_1        U_2
#> 1  1                7      80     2.5    10.0      60    20.0      10     1     1 -0.775 -1.800 -0.15343271  3.6612318 -0.9284327  1.8612318
#> 2  1               19      20     2.5     5.0      60     2.5       0     1     1 -0.275 -0.775  0.47936686 -0.8086989  0.2043669 -1.5836989
#> 3  1               30      20    10.0     5.0      80     5.0      10     1     1 -0.800 -0.950  0.12634298  1.2300690 -0.6736570  0.2800690
#> 4  1               32      40    20.0     2.5      80     2.5       0     1     1 -1.750 -0.975  2.08710825 -0.1882935  0.3371082 -1.1632935
#> 5  1               39      40    20.0     0.0      80    10.0      10     1     1 -1.800 -1.300  1.05540385  3.1339326 -0.7445961  1.8339326
#> 6  1               48      60     5.0     2.5      20     5.0      10     1     1 -0.900 -0.350  0.07255885 -0.1156742 -0.8274412 -0.4656742
#>   CHOICE
#> 1      2
#> 2      1
#> 3      2
#> 4      1
#> 5      2
#> 6      2
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2        e_1       e_2       U_1        U_2
#> 1  1                7      80     2.5    10.0      60    20.0      10     1     1 -0.775 -1.800 -1.2775839 1.5689946 -2.052584 -0.2310054
#> 2  1               19      20     2.5     5.0      60     2.5       0     1     1 -0.275 -0.775  0.5934850 0.5453996  0.318485 -0.2296004
#> 3  1               30      20    10.0     5.0      80     5.0      10     1     1 -0.800 -0.950 -0.5127855 1.7551185 -1.312785  0.8051185
#> 4  1               32      40    20.0     2.5      80     2.5       0     1     1 -1.750 -0.975  3.4643234 1.3685812  1.714323  0.3935812
#> 5  1               39      40    20.0     0.0      80    10.0      10     1     1 -1.800 -1.300 -0.5128262 0.3011019 -2.312826 -0.9988981
#> 6  1               48      60     5.0     2.5      20     5.0      10     1     1 -0.900 -0.350  4.3343477 1.2265189  3.434348  0.8765189
#>   CHOICE
#> 1      2
#> 2      1
#> 3      2
#> 4      1
#> 5      2
#> 6      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1060.0  -812.5   520.0 
#> initial  value 998.131940 
#> iter   2 value 994.260781
#> iter   3 value 974.092906
#> iter   4 value 973.856287
#> iter   5 value 970.270450
#> iter   6 value 970.262794
#> iter   6 value 970.262788
#> iter   6 value 970.262788
#> final  value 970.262788 
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1        e_2        U_1        U_2
#> 1  1                7      80     2.5    10.0      60    20.0      10     1     1 -0.775 -1.800  0.34553136 -0.8375727 -0.4294686 -2.6375727
#> 2  1               19      20     2.5     5.0      60     2.5       0     1     1 -0.275 -0.775 -1.32361481  0.3195766 -1.5986148 -0.4554234
#> 3  1               30      20    10.0     5.0      80     5.0      10     1     1 -0.800 -0.950  0.08515524 -0.6090259 -0.7148448 -1.5590259
#> 4  1               32      40    20.0     2.5      80     2.5       0     1     1 -1.750 -0.975 -0.18021132  2.3073397 -1.9302113  1.3323397
#> 5  1               39      40    20.0     0.0      80    10.0      10     1     1 -1.800 -1.300 -0.55591900  3.4630292 -2.3559190  2.1630292
#> 6  1               48      60     5.0     2.5      20     5.0      10     1     1 -0.900 -0.350 -0.29734711  3.0420404 -1.1973471  2.6920404
#>   CHOICE
#> 1      1
#> 2      2
#> 3      1
#> 4      2
#> 5      2
#> 6      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#>   440.0 -1087.5   447.5 
#> initial  value 998.131940 
#> iter   2 value 995.094499
#> iter   3 value 974.488144
#> iter   4 value 974.227531
#> iter   5 value 971.482008
#> iter   6 value 971.477251
#> iter   6 value 971.477249
#> iter   6 value 971.477249
#> final  value 971.477249 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01   0.00   0.00  0.00
#> est_blade            2    2  -0.04  0.00  -0.04  -0.04   0.00  0.00
#> est_bwarte           3    2   0.03  0.00   0.03   0.03   0.00  0.00
#> rob_pval0_bpreis     4    2   0.01  0.01   0.00   0.02   0.02  0.01
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.01  0.00   0.01   0.01   0.00  0.00
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> TRUE 
#>  100 
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1        e_2        U_1        U_2
#> 1  1               12      60     2.5     0.0      20    20.0      10     1     1 -0.775 -1.400  0.63130736 -1.4347994 -0.1436926 -2.8347994
#> 2  1               16      20    10.0     5.0      40     5.0       0     1     1 -0.800 -0.750  5.09739937  0.4118885  4.2973994 -0.3381115
#> 3  1               17      20    20.0     0.0      80    10.0      10     1     1 -1.600 -1.300  0.22397799  0.4666321 -1.3760220 -0.8333679
#> 4  1               25      60     5.0    10.0      20    20.0       5     1     1 -0.750 -1.500 -0.05146482  2.2007592 -0.8014648  0.7007592
#> 5  1               29      20     5.0    10.0      80     5.0       0     1     1 -0.350 -1.150  1.57620781  4.9154679  1.2262078  3.7654679
#> 6  1               32      40    10.0     2.5      80     2.5       5     1     1 -1.050 -0.875 -0.47930823  0.7058788 -1.5293082 -0.1691212
#>   CHOICE
#> 1      1
#> 2      1
#> 3      2
#> 4      2
#> 5      2
#> 6      2
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2       e_1        e_2         U_1         U_2
#> 1  1               12      60     2.5     0.0      20    20.0      10     1     1 -0.775 -1.400 0.3494617  0.1551114 -0.42553827 -1.24488857
#> 2  1               16      20    10.0     5.0      40     5.0       0     1     1 -0.800 -0.750 1.5845207  0.5556039  0.78452066 -0.19439613
#> 3  1               17      20    20.0     0.0      80    10.0      10     1     1 -1.600 -1.300 4.1993459 -0.1612424  2.59934589 -1.46124241
#> 4  1               25      60     5.0    10.0      20    20.0       5     1     1 -0.750 -1.500 0.6527215  1.3949219 -0.09727852 -0.10507806
#> 5  1               29      20     5.0    10.0      80     5.0       0     1     1 -0.350 -1.150 2.6927356 -1.3232777  2.34273564 -2.47327770
#> 6  1               32      40    10.0     2.5      80     2.5       5     1     1 -1.050 -0.875 0.3758168  0.8556930 -0.67418318 -0.01930696
#>   CHOICE
#> 1      1
#> 2      1
#> 3      1
#> 4      1
#> 5      1
#> 6      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -2340.0  -857.5   510.0 
#> initial  value 998.131940 
#> iter   2 value 989.566757
#> iter   3 value 968.906950
#> iter   4 value 968.775516
#> iter   5 value 959.377427
#> iter   6 value 959.364632
#> iter   7 value 959.364588
#> iter   7 value 959.364588
#> iter   7 value 959.364588
#> final  value 959.364588 
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2        e_1           e_2        U_1
#> 1  1               12      60     2.5     0.0      20    20.0      10     1     1 -0.775 -1.400 -0.5067469 -4.908678e-05 -1.2817469
#> 2  1               16      20    10.0     5.0      40     5.0       0     1     1 -0.800 -0.750  2.1209149  5.835693e-01  1.3209149
#> 3  1               17      20    20.0     0.0      80    10.0      10     1     1 -1.600 -1.300 -0.3310010  7.106139e-01 -1.9310010
#> 4  1               25      60     5.0    10.0      20    20.0       5     1     1 -0.750 -1.500  1.0469501  3.053872e-01  0.2969501
#> 5  1               29      20     5.0    10.0      80     5.0       0     1     1 -0.350 -1.150  1.2182730 -6.215119e-01  0.8682730
#> 6  1               32      40    10.0     2.5      80     2.5       5     1     1 -1.050 -0.875 -0.3318808  5.251218e+00 -1.3818808
#>          U_2 CHOICE
#> 1 -1.4000491      1
#> 2 -0.1664307      1
#> 3 -0.5893861      2
#> 4 -1.1946128      1
#> 5 -1.7715119      1
#> 6  4.3762180      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -2120.0  -842.5   582.5 
#> initial  value 998.131940 
#> iter   2 value 990.498814
#> iter   3 value 970.036278
#> iter   4 value 970.031934
#> iter   5 value 961.943463
#> iter   6 value 961.698866
#> iter   7 value 961.698562
#> iter   7 value 961.698561
#> iter   7 value 961.698561
#> final  value 961.698561 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.00  -0.05  -0.05   0.00  0.00
#> est_bwarte           3    2   0.02  0.01   0.01   0.02   0.01  0.00
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.16  0.19   0.03   0.30   0.27  0.13
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1         e_2        U_1
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1     1 -1.150 -0.350  1.88045081  0.33059180  0.7304508
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1     1 -0.675 -1.500 -0.08733163 -0.07195918 -0.7623316
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1     1 -0.925 -1.600 -0.31269859  3.95512677 -1.2376986
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1     1 -0.875 -0.850  0.20206751 -0.87018279 -0.6729325
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1     1 -0.550 -0.900 -0.25607132 -1.21928402 -0.8060713
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1     1 -1.550 -0.725  1.39833272 -0.08165078 -0.1516673
#>          U_2 CHOICE
#> 1 -0.0194082      1
#> 2 -1.5719592      1
#> 3  2.3551268      2
#> 4 -1.7201828      1
#> 5 -2.1192840      1
#> 6 -0.8066508      1
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2        e_1        e_2         U_1
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1     1 -1.150 -0.350 -0.0940785 -0.8728874 -1.24407850
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1     1 -0.675 -1.500 -0.6796651 -1.1297414 -1.35466507
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1     1 -0.925 -1.600  1.7899847  0.6372528  0.86498471
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1     1 -0.875 -0.850  0.9429192  0.7744473  0.06791921
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1     1 -0.550 -0.900  0.1003092  2.5583115 -0.44969083
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1     1 -1.550 -0.725 -0.3851894  0.9776369 -1.93518939
#>           U_2 CHOICE
#> 1 -1.22288745      2
#> 2 -2.62974141      1
#> 3 -0.96274717      1
#> 4 -0.07555271      1
#> 5  1.65831145      2
#> 6  0.25263691      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -2300.0  -967.5   417.5 
#> initial  value 998.131940 
#> iter   2 value 989.783897
#> iter   3 value 967.441065
#> iter   4 value 966.807343
#> iter   5 value 957.535574
#> iter   6 value 957.518843
#> iter   7 value 957.518805
#> iter   7 value 957.518805
#> iter   7 value 957.518805
#> final  value 957.518805 
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1        e_2        U_1
#> 1  1                3      80     5.0     0.0      20     5.0    10.0     1     1 -1.150 -0.350  0.39615659 0.74248610 -0.7538434
#> 2  1                5      60     2.5     5.0      20    20.0     5.0     1     1 -0.675 -1.500 -0.17578286 0.04260786 -0.8507829
#> 3  1               10      80     2.5     2.5      20    20.0     0.0     1     1 -0.925 -1.600  0.44905385 0.79514566 -0.4759461
#> 4  1               34      80     2.5     5.0      60     5.0     5.0     1     1 -0.875 -0.850  0.27140789 4.63953174 -0.6035921
#> 5  1               37      40     5.0    10.0      60     5.0     2.5     1     1 -0.550 -0.900 -0.03370054 0.84622952 -0.5837005
#> 6  1               39      20    20.0     2.5      60     2.5     2.5     1     1 -1.550 -0.725 -0.47233862 1.05805421 -2.0223386
#>           U_2 CHOICE
#> 1  0.39248610      2
#> 2 -1.45739214      1
#> 3 -0.80485434      1
#> 4  3.78953174      2
#> 5 -0.05377048      2
#> 6  0.33305421      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1600.0  -857.5   402.5 
#> initial  value 998.131940 
#> iter   2 value 993.094191
#> iter   3 value 975.977284
#> iter   4 value 975.860148
#> iter   5 value 971.032264
#> iter   6 value 971.027871
#> iter   6 value 971.027867
#> iter   6 value 971.027867
#> final  value 971.027867 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.01  -0.05  -0.04   0.01  0.01
#> est_bwarte           3    2   0.00  0.01   0.00   0.01   0.01  0.00
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.68  0.20   0.54   0.82   0.28  0.14
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2        e_1        e_2         U_1        U_2
#> 1  1                9      80     5.0       0      60    20.0    10.0     1     1 -1.150 -1.800  0.8053210 -0.7760447 -0.34467900 -2.5760447
#> 2  1               12      60     2.5      10      40    20.0     0.0     1     1 -0.575 -1.800  2.4581484 -0.5422855  1.88314842 -2.3422855
#> 3  1               13      20    20.0      10      80     2.5     0.0     1     1 -1.400 -0.975  0.4806134 -1.7030310 -0.91938664 -2.6780310
#> 4  1               70      80     5.0      10      20    20.0     2.5     1     1 -0.950 -1.550 -0.8558539  1.9784273 -1.80585392  0.4284273
#> 5  1               71      60    20.0      10      80    10.0     0.0     1     1 -1.800 -1.500  1.4200481  0.8856199 -0.37995187 -0.6143801
#> 6  1               73      60    10.0       0      40    20.0    10.0     1     1 -1.300 -1.600  1.3592506 -0.1823192  0.05925063 -1.7823192
#>   CHOICE
#> 1      1
#> 2      1
#> 3      1
#> 4      2
#> 5      1
#> 6      1
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2        e_1       e_2         U_1        U_2
#> 1  1                9      80     5.0       0      60    20.0    10.0     1     1 -1.150 -1.800  1.1057596 0.8176933 -0.04424039 -0.9823067
#> 2  1               12      60     2.5      10      40    20.0     0.0     1     1 -0.575 -1.800  1.4664332 0.2855647  0.89143319 -1.5144353
#> 3  1               13      20    20.0      10      80     2.5     0.0     1     1 -1.400 -0.975 -0.2567456 2.0307365 -1.65674558  1.0557365
#> 4  1               70      80     5.0      10      20    20.0     2.5     1     1 -0.950 -1.550  1.7936733 0.2273817  0.84367334 -1.3226183
#> 5  1               71      60    20.0      10      80    10.0     0.0     1     1 -1.800 -1.500 -0.5080847 1.8371868 -2.30808468  0.3371868
#> 6  1               73      60    10.0       0      40    20.0    10.0     1     1 -1.300 -1.600  0.2315646 0.5250324 -1.06843538 -1.0749676
#>   CHOICE
#> 1      1
#> 2      1
#> 3      2
#> 4      1
#> 5      2
#> 6      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -2720.0 -3230.0  1477.5 
#> initial  value 998.131940 
#> iter   2 value 961.903346
#> iter   3 value 923.013503
#> iter   4 value 921.553693
#> iter   5 value 899.826852
#> iter   6 value 899.416093
#> iter   7 value 899.408733
#> iter   7 value 899.408728
#> iter   7 value 899.408728
#> final  value 899.408728 
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
#>    ID Choice_situation alt1_x1 alt1_x2 alt1_x3 alt2_x1 alt2_x2 alt2_x3 Block group    V_1    V_2         e_1       e_2        U_1        U_2
#> 1  1                9      80     5.0       0      60    20.0    10.0     1     1 -1.150 -1.800 -0.55606335 0.4418297 -1.7060633 -1.3581703
#> 2  1               12      60     2.5      10      40    20.0     0.0     1     1 -0.575 -1.800 -0.70525965 0.3030154 -1.2802596 -1.4969846
#> 3  1               13      20    20.0      10      80     2.5     0.0     1     1 -1.400 -0.975  0.76358526 1.1547805 -0.6364147  0.1797805
#> 4  1               70      80     5.0      10      20    20.0     2.5     1     1 -0.950 -1.550 -0.50057341 1.6569802 -1.4505734  0.1069802
#> 5  1               71      60    20.0      10      80    10.0     0.0     1     1 -1.800 -1.500  0.95196390 4.6640005 -0.8480361  3.1640005
#> 6  1               73      60    10.0       0      40    20.0    10.0     1     1 -1.300 -1.600 -0.07807331 0.9519585 -1.3780733 -0.6480415
#>   CHOICE
#> 1      2
#> 2      1
#> 3      2
#> 4      2
#> 5      2
#> 6      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1480.0 -3742.5   752.5 
#> initial  value 998.131940 
#> iter   2 value 931.326512
#> iter   3 value 904.389362
#> iter   4 value 902.747564
#> iter   5 value 895.585146
#> iter   6 value 895.212841
#> iter   7 value 895.208094
#> iter   7 value 895.208091
#> iter   7 value 895.208091
#> final  value 895.208091 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.01  -0.05  -0.04   0.01  0.01
#> est_bwarte           3    2   0.01  0.03   0.00   0.03   0.04  0.02
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.28  0.40   0.00   0.57   0.57  0.28
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
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3 group    V_1    V_2         e_1       e_2        U_1         U_2
#> 1  1                1     1      80      20     2.5    20.0      10       5     1 -0.775 -1.500  0.12234341 0.6503776 -0.6526566 -0.84962241
#> 2  1                2     1      60      40     5.0    10.0       5      10     1 -0.850 -0.900 -0.43360819 0.6615210 -1.2836082 -0.23847900
#> 3  1                3     1      60      20    20.0    20.0       0      10     1 -2.000 -1.400 -0.31286639 5.7827787 -2.3128664  4.38277870
#> 4  1                4     1      20      80    20.0     2.5       0      10     1 -1.600 -0.775 -0.15949911 0.6857678 -1.7594991 -0.08923219
#> 5  1                5     1      40      80    10.0     5.0      10       5     1 -0.900 -1.050 -0.05237788 1.5859039 -0.9523779  0.53590389
#> 6  1                6     1      60      80     5.0     2.5       0       0     1 -0.950 -0.975  2.34036634 0.2393918  1.3903663 -0.73560816
#>   CHOICE
#> 1      1
#> 2      2
#> 3      2
#> 4      2
#> 5      2
#> 6      1
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
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3 group    V_1    V_2         e_1        e_2          U_1
#> 1  1                1     1      80      20     2.5    20.0      10       5     1 -0.775 -1.500  1.74395858  1.6507121  0.968958575
#> 2  1                2     1      60      40     5.0    10.0       5      10     1 -0.850 -0.900  1.10763894  0.9337414  0.257638936
#> 3  1                3     1      60      20    20.0    20.0       0      10     1 -2.000 -1.400 -1.23519031 -0.7089281 -3.235190315
#> 4  1                4     1      20      80    20.0     2.5       0      10     1 -1.600 -0.775 -0.06854059  2.1896932 -1.668540589
#> 5  1                5     1      40      80    10.0     5.0      10       5     1 -0.900 -1.050  0.90927543  0.3884170  0.009275432
#> 6  1                6     1      60      80     5.0     2.5       0       0     1 -0.950 -0.975  0.57272851  0.4992305 -0.377271490
#>           U_2 CHOICE
#> 1  0.15071215      1
#> 2  0.03374141      1
#> 3 -2.10892809      2
#> 4  1.41469320      2
#> 5 -0.66158301      1
#> 6 -0.47576952      1
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#> bpreis  blade bwarte 
#>   -880   -830    405 
#> initial  value 998.131940 
#> iter   2 value 994.862696
#> iter   3 value 973.696602
#> iter   4 value 973.644209
#> iter   5 value 971.118222
#> iter   6 value 971.113908
#> iter   6 value 971.113906
#> iter   6 value 971.113906
#> final  value 971.113906 
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
#>    ID Choice_situation Block alt1_x1 alt2_x1 alt1_x2 alt2_x2 alt1_x3 alt2_x3 group    V_1    V_2        e_1        e_2       U_1        U_2
#> 1  1                1     1      80      20     2.5    20.0      10       5     1 -0.775 -1.500 -1.1552146  1.6918663 -1.930215  0.1918663
#> 2  1                2     1      60      40     5.0    10.0       5      10     1 -0.850 -0.900  3.5255143  2.5874719  2.675514  1.6874719
#> 3  1                3     1      60      20    20.0    20.0       0      10     1 -2.000 -1.400 -0.1288440  3.2280170 -2.128844  1.8280170
#> 4  1                4     1      20      80    20.0     2.5       0      10     1 -1.600 -0.775 -0.3072974 -0.3710956 -1.907297 -1.1460956
#> 5  1                5     1      40      80    10.0     5.0      10       5     1 -0.900 -1.050 -0.6042191 -0.1952303 -1.504219 -1.2452303
#> 6  1                6     1      60      80     5.0     2.5       0       0     1 -0.950 -0.975 -0.6233011 -0.2803958 -1.573301 -1.2553958
#>   CHOICE
#> 1      2
#> 2      1
#> 3      2
#> 4      2
#> 5      2
#> 6      2
#> 
#>  
#>  This is the utility functions 
#>  U_1 = @bpreis *$alt1_x1 + @blade *$alt1_x2 + @bwarte *$alt1_x3 ;U_2 = @bpreis *$alt2_x1 + @blade *$alt2_x2 + @bwarte *$alt2_x3 ;Initial function value: -998.1319 
#> Initial gradient value:
#>  bpreis   blade  bwarte 
#> -1060.0  -832.5   460.0 
#> initial  value 998.131940 
#> iter   2 value 994.307556
#> iter   3 value 972.255010
#> iter   4 value 972.242499
#> iter   5 value 968.104609
#> iter   6 value 968.103010
#> iter   6 value 968.103008
#> iter   6 value 968.103008
#> final  value 968.103008 
#> converged
#> 
#> 
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> \                 vars    n   mean    sd    min    max  range    se
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> est_bpreis           1    2  -0.01  0.00  -0.01  -0.01   0.00  0.00
#> est_blade            2    2  -0.05  0.00  -0.05  -0.05   0.00  0.00
#> est_bwarte           3    2   0.02  0.00   0.01   0.02   0.00  0.00
#> rob_pval0_bpreis     4    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_blade      5    2   0.00  0.00   0.00   0.00   0.00  0.00
#> rob_pval0_bwarte     6    2   0.12  0.08   0.06   0.18   0.12  0.06
#> ================  ====  ===  =====  ====  =====  =====  =====  ====
#> 
#> FALSE 
#>   100 
#> 9.804 sec elapsed
#> $tic
#>  elapsed 
#> 10314.36 
#> 
#> $toc
#>  elapsed 
#> 10324.16 
#> 
#> $msg
#> logical(0)
#> 
#> $callback_msg
#> [1] "9.804 sec elapsed"
```

<img src="man/figures/README-example-1.png" width="100%" /><img src="man/figures/README-example-2.png" width="100%" /><img src="man/figures/README-example-3.png" width="100%" />
