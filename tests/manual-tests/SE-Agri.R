rm(list=ls())
devtools::load_all()


library(rlang)

designpath<- system.file("extdata","SE_AGRI", package = "simulateDCE")

destype = 'ngene'
resps =360  # number of respondents
nosim=2 # number of simulations to run (about 500 is minimum)

#betacoefficients should not include "-"
bcoeff <- list(
  basc = 4.2, ## very high asc
  bprof = 0.3,
  bexp = 0.3,
  bdomestic = 0.3,
  bforeign = 0.3,
  bdamage = 0.6,
  bprice = 0.2)



manipulations = list(alt1.professional=     expr(alt1.initiator==1),
                     alt2.professional=     expr(alt2.initiator==1),
                     alt1.expert      =     expr(alt1.initiator==2),
                     alt2.expert      =     expr(alt2.initiator==2),
                     alt1.domestic    =     expr(alt1.funding==1),
                     alt2.domestic    =     expr(alt2.funding==1),
                     alt1.foreign     =     expr(alt1.funding==2),
                     alt2.foreign     =     expr(alt2.funding==2))


#place your utility functions here
ul<- list(u1=
           list(
             v1 =V.1 ~  bprof*alt1.professional+ bexp * alt1.expert + bdomestic * alt1.domestic + bforeign * alt1.foreign + bdamage*alt1.damage + bprice * alt1.compensation,
             v2 =V.2 ~  bprof*alt2.professional + bexp * alt2.expert + bdomestic * alt2.domestic + bforeign * alt2.foreign + bdamage*alt2.damage + bprice * alt2.compensation,
             v3 =V.3 ~ basc)
)

seagri <- sim_all(nosim = nosim, resps=resps, destype = destype,
                   designpath = designpath, u=ul, bcoeff = bcoeff)

