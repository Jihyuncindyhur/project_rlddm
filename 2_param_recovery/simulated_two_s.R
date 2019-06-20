## subjects = 6
## total steps = 100 (step size = 0.02)
## one population, two starting points
## parameter recovery

library(rstan)
library(dplyr)
library(stringr)
setwd('/home/hur_jihyun/project_hur')

# data
rm(list=ls())
dat = read.csv('simulated_two_s_6sub.csv', header=TRUE)

allSubjs = unique(dat$subn)    # all subjects
N = length(allSubjs)           # number of subjects
T = table(dat$subn)[1]         # trial numbers
RTbound = 0.01                 # lower bound of RT
RTmax = 2                      # upper bound of RT

# replace NA with zeros
dat$rt[is.na(dat$rt)] <- RTmax

choice <- array(0, c(T, N))  
escape <- array(0, c(T, N))
cond <- array(0, c(T, N))      # 1-go to escape, 2-no-go to escape, 3-go to avoid, 4-no-go to avoid
fd <- array(0, c(T, N))
rt <- array(0, c(T, N))

for (i in 1:N) {
  curSubj = allSubjs[i]
  tmp     = subset(dat, subn == curSubj)
  choice[1:T, i] <- tmp$choice
  escape[1:T, i] <- tmp$escape
  cond[1:T, i] <- tmp$condn
  rt[1:T, i] <- tmp$rt
  fd[1:T, i] <- tmp$fdbk
}

# minimum response time per subject
minRT <- with(dat, aggregate(rt, by = list(y = subn), FUN = min)[["x"]])
total_steps = 100

dataList <- list(
  N        = N,
  T        = T,
  C        = choice,
  E        = escape,
  cond     = cond,
  fd       = fd,
  rt       = rt,
  minRT    = minRT,
  RTbound  = RTbound,
  Steps    = total_steps,
  RTmax    = RTmax
)

params <- c('tau', 'b0', 'b1', 'w1', 'w2', 'alpha', 'omega')

pars <- c()
pars <- c(pars, str_c('mu_', params))
pars <- c(pars, params, 'log_lik') 

options(mc.cores = parallel::detectCores())
sm <- rstan::stan_model('simulated_two_s.stan')

pars_init <- function() {
  ret <- list() 
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0, N)
  }
  return(ret)
}

sim_two_s_6sub_100_2 = rstan::sampling(sm, data = dataList, pars = pars, init = pars_init,
                                     iter = 2000, warmup = 1000, chains = 4)

save(sim_two_s_6sub_100_2, file='sim_two_s_6sub_100_2.RData')
#mu_tau      mu_b0      mu_b1      mu_w1      mu_w2   mu_alpha   mu_omega 
#0.09188545 0.52032282 1.83394511 0.39453991 0.34799191 0.18735706 1.73202208 

#     tau        b0       b1        w1        w2     alpha    omega
#1 0.07466291 0.1926431 1.901596 0.4432639 0.2749378 0.2658971 1.413908
#2 0.11363544 0.7039006 1.736550 0.3314496 0.2340108 0.2208476 1.859198
#3 0.08723098 0.3218547 3.028138 0.3369314 0.5011784 0.1329283 1.619022
#4 0.09107009 0.9380560 1.712668 0.3549563 0.3954458 0.1010412 1.616998
#5 0.09226960 0.1001286 2.342587 0.4617086 0.3766223 0.2179038 1.589520
#6 0.07160635 0.7699521 2.053187 0.4585965 0.2892615 0.1966464 1.799519
