
library("dplyr")
library(tidyverse)
library(broom)
library(readxl)

# Data for Homework 7
HwkPop <- read_excel("/Users/maireaddillon/Documents/Duke/BIOS-718/Longitudinal-Analysis/HW7/LargePubHlthDataset.xls")
# We use complete records; 135 incomplete records deleted.
HwkPop <- HwkPop[complete.cases(HwkPop),]

set.seed(100)

# CAUTION w/ t.test
#
#   Make sure your mean difference matches R's confidence interval estimate
#   which is mean(Y[X==0]) - mean(Y[X==1]). Be sure to take the negative of
#   the difference of t.test's "estimate". See PopDiff below.

# N = 100
B = 10000

## Step 1: Compute population parameter for true difference between means:
#
# Population-level mean difference, PopDiff
PopDiff <-  -diff(t.test(kcal ~ female, data=HwkPop)$estimate )

## Step 2: Generate random samples of size N for your Monte Carlo simulation
#          Place random samples in a list of data frames (make's life easier):

N <- 50 + 25*(1:2)
for (i in 1:length(N)) {
  
data_list <- map(.x = 1:B, .f = ~ sample_n(HwkPop, N[i]))
head(data_list[[1]])

## Step: 3: Collect your MC estimates and safe them in tmpout.
#           In this illustration, We've saved:
#           dif  = estimated mean difference: mean(Y[X==0]) - mean(Y[X==1])
#           lcl  = lower 95% conf. inv. estimate
#           ucl  = upper 95% conf. inv. estimate
#           cov  = decision of whether the CI contains the true difference
#           TrueDif = Population mean difference 

tmpout <- NULL
for(i in 1:B) {
#  tmpout <- rbind(tmpout, ( prod( t.test( kcal ~  female, data=data_list[[i]])$conf.int - TrueDif) < 0 )  )
  tst <- t.test( kcal ~  female, data=data_list[[i]])
  dif <- -diff(tst$estimate)
  lcl <- tst$conf.int[1]
  ucl <- tst$conf.int[2]
  cov <- ( lcl <= PopDiff & PopDiff <= ucl )
  stat <- c(TrueDif = PopDiff, dif = dif, lcl = lcl, ucl=ucl, Cover = cov ) 
  tmpout <- rbind(tmpout, stat  )
}
head(tmpout)
print(apply(tmpout, 2, mean))

}