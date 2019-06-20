## subjects = 30
## iters = 2000, warmup = 1000, chains = 4
## total steps = 50 (step size = 0.04)
## two drift rates, two starting points by condition
## hierarchical bayesian analysis (one population distribution)

## data
## choice:          choice (0-no-go, 1-go)
## response time:   rt
## state:           escape (0-avoid, 1-escape)
## feedback:        fdbk   (avoid;  0-not avoid, 1-avoid, escape; 0-not escape, 1-escape)
## go trial:        go     (0-no-go, 1-go)

rm(list=ls())

library(rstan)
library(dplyr)
library(stringr)

# data 
dat = read.csv('data_30sub.csv', header=TRUE)

allSubjs = unique(dat$subn)    # all subjects
N = length(allSubjs)           # number of subjects
T = table(dat$subn)[1]         # trial numbers 
RTbound = 0.01                 # lower bound of RT
RTmax = 2                      # upper bound of RT

# group 
grp <- dat %>% group_by(subn) %>% summarise(grp = max(grp_sui_nosui_v))
grp <- as.vector(grp$grp)

# replace NA with zeros
dat$rt[is.na(dat$rt)] <- RTmax

choice <- array(0, c(T, N))  
escape <- array(0, c(T, N))
cond <- array(0, c(T, N))      # 1-go to escape, 2-no-go to escape, 3-go to avoid, 4-no-go to avoid
fd <- array(0, c(T, N))
rt <- array(0, c(T, N))

for (i in 1:N) {
  curSubj = allSubjs[i]
  tmp     = subset(dat, X == curSubj)
  choice[1:T, i] <- tmp$choice
  escape[1:T, i] <- tmp$escape
  cond[1:T, i] <- tmp$Condn
  rt[1:T, i] <- tmp$rt
  fd[1:T, i] <- tmp$fdbk
}

# minimum response time per subject
minRT <- with(dat, aggregate(rt, by = list(y = subn), FUN = min)[["x"]])
total_steps = 50

dataList <- list(
  N        = N,
  T        = T,
  C        = choice,
  E        = escape,
  cond     = cond,
  fd       = fd,
  rt       = rt,
  grp      = grp,
  minRT    = minRT,
  RTbound  = RTbound,
  Steps    = total_steps,
  RTmax    = RTmax
)

params <- c('tau','alpha', 'omega')
cal_params <- c('b1', 'b2', 'b3', 'w1', 'w2')

pars <- c()
pars <- c(pars, str_c('mu_', params))
pars <- c(pars, str_c('mu_', cal_params))
pars <- c(pars, str_c('mu_', cal_params, '_sui'))
pars <- c(pars, str_c('mu_', cal_params, '_nosui'))
pars <- c(pars, params, cal_params, 'log_lik') 
pars

options(mc.cores = parallel::detectCores())
sm <- rstan::stan_model('hba_two_both.stan')

pars_init <- function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, 8)
  ret[['sigma']] <- rep(0.5, 8)
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0, N)
  }
  for (param in cal_params) {
    ret[[str_c(param, '_pr')]] <- rep(0, N)
  }
  return(ret)
}

hba_two_both_30sub_50 = rstan::sampling(sm, data = dataList, pars = pars, init = pars_init,
                           iter = 2000, warmup = 1000, chains = 4)

save(hba_two_both_30sub_50, file='hba_two_both_30sub_50.RData')
