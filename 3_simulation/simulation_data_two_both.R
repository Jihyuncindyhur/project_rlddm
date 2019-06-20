## subjects = 12                 
## total steps = 50 (step size = 0.04)
## model = hba_two_both 
## simulating data

rm=(list=ls())

setwd('/home/hur_jihyun/project_hur')
seed = 1005
set.seed(seed)

library(GMCM)
library(RWiener)
library(rstan)
load('true_param_both.RData')

# initialize
N = 12   # number of subjects
C = 40
conds <- c(rep('go_escape', C), rep('nogo_escape', C),
           rep('go_avoid', C),  rep('nogo_avoid', C))
total_trials = length(conds)
RTmax = 2

# simulate true parameters
params <- c('tau', 'b1', 'b2', 'b3', 'w1', 'w2', 'alpha', 'omega')
true_param <- data.frame(matrix(0, nrow=N, ncol=length(params)))
colnames(true_param) <- c('tau', 'b1', 'b2', 'b3', 'w1', 'w2', 'alpha', 'omega')
for (i in 1:length(params)) {
  true_param[, i] <- rnorm(N, true_param_mean_both[i], true_param_sd_both[i])
}

save(true_param, file = 'true_param_both.RData')

# create an empty dataset
all_data <- NULL

for (i in 1:N) {
  # individual level parameter values
  tau   <- true_param$tau[i]
  b1    <- true_param$b1[i]
  b2    <- true_param$b2[i]
  b3    <- true_param$b3[i]
  w1    <- true_param$w1[i]
  w2    <- true_param$w2[i]
  alpha <- true_param$alpha[i]
  omega <- true_param$omega[i]
  
  Q = data.frame(matrix(0, nrow=4, ncol=2)) # inital state-action values
  
  # calculate the probability of go per trial, by condition
  for (t in 1:total_trials) {
    if (conds[t] == 'go_escape') {
      n = 1
      escape = 1
      
      # starting point 
      w = w1
      
      # calculate probability of go
      p_go <- GMCM:::inv.logit(Q[n, 2] - Q[n, 1])     # 1 - nogo, 2 - go
      
      choice <- rbinom(n = 1, size = 1, prob = p_go)
      
      # drift rate (dr)
      dr <- b1 + (b3 * (Q[n, 2] - Q[n, 1]))
      
      # store rt data
      if (choice == 1) {
        rt <- rwiener(1, omega, tau, w, dr)[1]
        fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
        
        # rt should be less than RTmax
        if (rt >= RTmax) {
          rt <- NA 
          choice <- 0
          fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
          
        } else { rt <- rt }
      } else {
        rt <- NA
        fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
      }
      
      # update Q values
      Q[n, choice + 1] = Q[n, choice + 1] + alpha*(fdbk - Q[n, choice + 1])
      
      all_data <- rbind(all_data, c(i, n, escape, choice, rt, fdbk))
      
    } else if (conds[t] == 'nogo_escape') {
      n = 2
      escape = 1
      
      # starting point 
      w = w1
      
      # calculate probability of go
      p_go <- GMCM:::inv.logit(Q[n, 2] - Q[n, 1])
      
      choice <- rbinom(n = 1, size = 1, prob = p_go)
      
      # drift rate (dr)
      dr <- b1 + (b3 * (Q[n, 2] - Q[n, 1]))
      
      if (choice == 0) {
        rt <- NA
        fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
      } else {
        rt <- rwiener(1, omega, tau, w, dr)[1]
        fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
        
        # rt should be less than RTmax
        if (rt >= RTmax) {
          rt <- NA
          choice <- 0
          fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
        } else {rt <- rt}
      }
      
      Q[n, choice + 1] = Q[n, choice + 1] + alpha*(fdbk - Q[n, choice + 1])
      
      all_data <- rbind(all_data, c(i, n, escape, choice, rt, fdbk))
      
    } else if (conds[t] == 'go_avoid') {
      n = 3
      escape = 0
      
      # starting point
      w = w2
      
      # calculate probability of go
      p_go <- GMCM:::inv.logit(Q[n, 2] - Q[n, 1])
      
      choice <- rbinom(n = 1, size = 1, prob = p_go)
      
      # drift rate (dr)
      dr <- b2 + (b3 * (Q[n, 2] - Q[n, 1]))
      
      if (choice == 1) {
        rt <- rwiener(1, omega, tau, w, dr)[1]
        fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
        
        # rt should be less than RTmax
        if (rt >= RTmax) {
          rt <- NA
          choice <- 0
          fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
        } else { rt <- rt }
      } else {
        rt <- NA
        fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
      }
      
      Q[n, choice + 1] = Q[n, choice + 1] + alpha*(fdbk - Q[n, choice + 1])
      
      all_data <- rbind(all_data, c(i, n, escape, choice, rt, fdbk))
    } else {
      n = 4
      escape = 0
      
      # starting point
      w = w2
      
      # calculate probability of go
      p_go <- GMCM:::inv.logit(Q[n, 2] - Q[n, 1])
      
      choice <- rbinom(n = 1, size = 1, prob = p_go)
      
      # drift rate (dr)
      dr <- b2 + (b3 * (Q[n, 2] - Q[n, 1]))
      
      if (choice == 0) {
        rt <- NA
        fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
      } else {
        rt <- rwiener(1, omega, tau, w, dr)[1]
        fdbk <- rbinom(size = 1, n = 1, prob = 0.2)
        
        # rt should be less than RTmax
        if (rt >= RTmax) {
          rt <- NA 
          choice <- 0
          fdbk <- rbinom(size = 1, n = 1, prob = 0.8)
        } else { rt <- rt }
      }
      
      Q[n, choice + 1] = Q[n, choice + 1] + alpha*(fdbk - Q[n, choice + 1])
      
      all_data <- rbind(all_data, c(i, n, escape, choice, rt, fdbk))
    }
  }
}
colnames(all_data) <- c('subn', 'condn', 'escape', 'choice', 'rt', 'fdbk')
as.data.frame(all_data)

write.csv(all_data, file = "simulated_two_both_12sub.csv", row.names = F)

