rm(list=ls(all=TRUE))
########################
####Loading packages####
########################
library(pwr)

#################################################################
#### Medium Effects, alpha = 0.05 (two-tailed), 1-beta = 0.8 ####
#################################################################
pwr.t.test(d = 0.33, sig.level = 0.05, power = 0.8, type = "two.sample")


