## subjects = 30
## iters = 2000, warmup = 1000, chains = 4
## total steps = 50 (step size = 0.04)
## two drift rates, one starting point by condition
## hierarchical bayesian analysis (one population distribution)


library(rstan)
library(dplyr)
library(stringr)
library(loo)
setwd('/home/hur_jihyun/project_hur')

# data
rm(list=ls())
dat = read.csv('data_30sub.csv', header=TRUE)

allSubjs = unique(dat$subn)    # all subjects
allSubjs
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

params <- c('tau', 'b1', 'b2', 'b3', 'w', 'alpha', 'omega')

pars <- c()
pars <- c(pars, str_c('mu_', params))
pars <- c(pars, params, 
          'mu_b1_sui', 'mu_b1_nosui', 'mu_b2_sui', 'mu_b2_nosui', 'mu_b3_sui', 'mu_b3_nosui', 
          'mu_w_sui', 'mu_w_nosui', 'log_lik') 

options(mc.cores = parallel::detectCores())
sm <- rstan::stan_model('hba_two_v.stan')

pars_init <- function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0, N)
  }
  return(ret)
}

hba_two_v_30sub_50 = rstan::sampling(sm, data = dataList, pars = pars, init = pars_init,
                               iter = 2000, warmup = 1000, chains = 4)

save(hba_two_v_30sub_50, file='hba_two_v_30sub_50.RData')

