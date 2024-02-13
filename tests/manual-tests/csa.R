
rm(list=ls())
devtools::load_all()

#place your utility functions here



set.seed(3393)

designpath<- system.file("extdata","CSA" ,package = "simulateDCE")
#notes <- "This design consists of different heuristics. One group did not attend the methan attribute, another group only decided based on the payment"

notes <- "No Heuristics"

resps =240  # number of respondents
nosim=1000 # number of simulations to run (about 500 is minimum)


bcoeff<-list(
  bx1 = -0.1,
  bx2 = -0.1,
  bx3 = -0.05,
  bx4 = -0.025)


destype <- "spdesign"


#place your utility functions here
ul<- list(u1= list(
  v1 =V.1 ~  bx1 * alt1.x1 + bx2 * alt1.x2 +  bx3 * alt1.x3  +  bx4 * alt1.x4,
  v2 =V.2 ~  bx1 * alt2.x1 + bx2 * alt2.x2 +  bx3 * alt2.x3  +  bx4 * alt2.x4,
  v3 =V.3 ~ -5
)
)



csa <- simulateDCE::sim_all(nosim = nosim, resps=resps, destype = destype,
                 designpath = designpath, u= ul, bcoeff = bcoeff)



saveRDS(csa,file = "tests/manual-tests/csa.RDS")

