###############################################################################
# 
# Filename: Hw7Sims.R
# 
# Example code for investigating nominal coverage (95%) w/ "robust" regression
# via Welch's unequal variance t-test.
# 
# Notes:
#
# Code can easily be modified to naive (equal variance) t-test by changing the
#    var.equal parameter to TRUE.
#
# Example code below evals crp (Y) vs female (X); includes variable names w/in
#    the code (poor). Better to pass variable names into functions (e.g., get).
#    Or you can just exchange variable names from tru_dif and t_ConfInv.
#
# Code only evaluates nominal coverage (95%). Update code to include MC
#    estimated differences (for bias) by updating the output from t_ConfInv
#    to echo "stats" instead of "covg".
#
# Update the for() loop for the MC simulations by changing from mean() to
#    apply(sim, 2, mean) for > 1 variable summary.
#
# Install/library dplyr to use the random sample function sample_n (in the
#    prec_t function).
#
# Use set.seed if you want to do pseudo-random number generation
#
# set.seed(NULL)
#
# Code that might be helpful to pass variable names into the simulation programs
# below 
#
#  Var <- function(data, Var){ get(Var, data)}
#
#  Instructions:
#
#  1. Install dplyr if you have not done so already
#
# install.packages("dplyr")  # For sample_n() function
# library("dplyr")
#
# 2. Load the functions tru_dif, t_ConfInv, t_prec into R 
#
# 3. Set your number of MC runs BigB, the sample sizes N, and create a blank
#    repository for you to save your results (in SaveEsts)
#
# 4. Use the for loop (over the sample sizes, N) for your MC simulations 
#
# 5. Summarize/evaluate the results from SaveEsts
#
# Data for Homework 7
library(readxl)
HwkPop <- read_excel("/Users/maireaddillon/Documents/Duke/BIOS-718/Longitudinal-Analysis/HW7/LargePubHlthDataset.xls")
# We use complete records; 135 incomplete records deleted.
HwkPop <- HwkPop[complete.cases(HwkPop),]
attach(HwkPop)

# Get the true "population" outcome mean (Y) difference between groups (X)
tru_dif <- function(Popdat){
  diff(t.test(crp ~ female, data=Popdat)$estimate )
}
# Get summaries (e.g., coverage, mean difference) from random samples 
t_ConfInv <- function(Sampdat,TrueDif){
  test  <- t.test(crp ~ female, data=Sampdat, var.equal = FALSE)
  covg  <- ( test$conf.int[1] <= TrueDif & TrueDif <= test$conf.int[2] )
  est   <- diff(test$estimate)
  stats <- c(N = N, Diff = est, True = TrueDif, Covg = covg ) 
  covg 
}

# prec_t will execute a MC simulation BigB times for a given sample size
prec_t <- function(Popdat, N, BigB, TrueDif){
  sim  <- replicate(BigB, t_ConfInv(sample_n(Popdat, N), TrueDif))
  sim 
}

# Number of simulation runs (for each sample size) 
BigB <- 1000
# Vary the sample sizes from 60 to 900 by 20
N    <- 60+20*(1:42)
SaveEsts <- matrix(data=NA, nrow=length(N), ncol=2)
#
# MC simulations over the sample sizes (N) specified. Each one will be
# executed BigB times (see prec_t function).
#
#   Mean summaries are saved in SaveEsts, with sample sizes, N
#
for (i in 1:length(N)) {
SaveEsts[i,] <- c( N[i], mean( prec_t(HwkPop, N[i], BigB, tru_dif(HwkPop) ) ) )
######################## Use apply(sim, 2, mean) to summarize "stats" in t_ConfInv
######################## not just coverage (covg); use t() on replicate() in prec_t
}

