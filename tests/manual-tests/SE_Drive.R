

rm(list=ls())
devtools::load_all()


library(rlang)

designpath<- system.file("extdata","SE_DRIVE" ,package = "simulateDCE")

resps =120  # number of respondents
nosim= 2 # number of simulations to run (about 500 is minimum)






# bpreis = -0.036
# blade  = -0.034
# bwarte = -0.049


decisiongroups=c(0,0.7,1)

# wrong parameters

#
bpreis = -0.01
blade = -0.07
bwarte = 0.02

manipulations = list(alt1.x2=     expr(alt1.x2/10),
                     alt1.x3=     expr(alt1.x3/10),
                     alt2.x2=     expr(alt2.x2/10),
                     alt2.x3=     expr(alt2.x3/10)
)


#place your utility functions here
u<-list( u1 =

           list(
             v1 =V.1~  bpreis * alt1.x1 + blade*alt1.x2 + bwarte*alt1.x3   ,
             v2 =V.2~  bpreis * alt2.x1 + blade*alt2.x2 + bwarte*alt2.x3
           )

         ,
         u2 = list(  v1 =V.1~  bpreis * alt1.x1    ,
                     v2 =V.2~  bpreis * alt2.x1)

)


destype="ngene"

sedrive <- sim_all()
