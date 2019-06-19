## subject = 6
## total steps = 50 (step size = 0.04)
## one population, two starting points, two drift rates
## parameter recovery

library(rstan)
library(dplyr)
library(stringr)
setwd('/home/hur_jihyun/project_hur')

# data
rm(list=ls())
dat = read.csv('simulated_two_both_6sub.csv', header=TRUE)

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
minRT
total_steps = 50

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

params <- c('tau', 'b1', 'b2', 'b3', 'w1', 'w2', 'alpha', 'omega')

pars <- c()
pars <- c(pars, str_c('mu_', params))
pars <- c(pars, params, 'log_lik') 

options(mc.cores = parallel::detectCores())
sm <- rstan::stan_model('simulated_two_both.stan')

pars_init <- function() {
  ret <- list()
  ret[['mu_pr']] <- rep(0.5, length(params))
  ret[['sigma']] <- rep(0.5, length(params))
  for (param in params) {
    ret[[str_c(param, '_pr')]] <- rep(0, N)
  }
  return(ret)
}

sim_two_both_12sub_50 = rstan::sampling(sm, data = dataList, pars = pars, init = pars_init,
                                     iter = 2000, warmup = 1000, chains = 4)

save(sim_two_both_12sub_50, file='sim_two_both_12sub_50.RData')

#true mean
#   mu_tau     mu_b1     mu_b2     mu_b3     mu_w1     mu_w2  mu_alpha  mu_omega 
#0.0875412 0.7286472 0.4939258 1.7620384 0.3588594 0.3433191 0.1942592 1.7467887

#true sd
#   mu_tau      mu_b1      mu_b2      mu_b3      mu_w1      mu_w2   mu_alpha   mu_omega 
#0.02740396 0.28877759 0.27085700 0.45113743 0.07689368 0.07234543 0.07642233 0.17765215 

load('~/project_hur/true_param_both.RData')
true_param

#       tau        b1          b2       b3        w1        w2      alpha    omega
#1  0.05941500 0.8285320 0.380471077 1.720132 0.2482125 0.3767651 0.01331133 1.773877
#2  0.05713069 1.0519968 0.004566197 2.224277 0.2717824 0.3300591 0.11482313 1.928912
#3  0.09166112 0.9667308 0.613647373 2.741841 0.4130645 0.1176829 0.11006792 1.713184
#4  0.05001988 0.4129782 0.533028856 1.991124 0.4365310 0.3384463 0.20320397 1.838554
#5  0.02688116 0.9107809 0.335013301 1.431123 0.2608247 0.3480213 0.16394036 1.887429
#6  0.08528266 0.2778487 0.046527153 2.348918 0.3253152 0.2879221 0.22583650 1.468518
#7  0.05522212 0.7678135 0.596461000 1.834287 0.2761471 0.4393195 0.31042614 1.566359
#8  0.14809441 0.9621373 0.447010856 2.755610 0.3417359 0.3027918 0.35910294 1.861715
#9  0.08058392 0.7413603 0.349988941 1.647096 0.3762432 0.3911825 0.14828677 1.463559
#10 0.10056135 1.0325245 0.771110402 1.676375 0.3114203 0.3922103 0.31290496 1.888810
#11 0.12450102 0.7285090 0.760203029 2.790342 0.6192176 0.3660700 0.18934908 1.553978
#12 0.01765523 0.7830814 0.478507106 1.268485 0.2666214 0.4481599 0.08927140 1.714725