// Stan code for RL-DDM model
// model: hba_two_both
// written by Jihyun Hur

data {
  int<lower=1> N;                    // subject
  int<lower=1> T;                    // trial
  int<lower=1> Steps;                // total number of steps
  int<lower=0, upper=1> C[T, N];     // choice
  int<lower=0, upper=1> E[T, N];     // escape
  int<lower=1, upper=4> cond[T, N];  // condition
  int<lower=0, upper=1> fd[T, N];    // feedback
  real rt[T, N];                     // response time
  real minRT[N];                     // minimum RT for each subject of the observed data
  real RTbound;                      // minimum threshold for response time of a go action
  real RTmax;                        // maximum RT 
}

transformed data {
}

parameters {
  // hyperparameters
  vector[8] mu_pr;
  vector<lower=0>[8] sigma;
  
  // subject-level raw parameters (for Matt trick)
  vector[N] tau_pr;                   
  vector[N] b1_pr;    // go bias in escape condition
  vector[N] b2_pr;    // go bias in avoid condition                 
  vector[N] b3_pr;    // differential action value weight                 
  vector[N] w1_pr;                    
  vector[N] w2_pr;                    
  vector[N] alpha_pr;                 
  vector[N] omega_pr;               
}

transformed parameters {
  // transform subject-level raw parameters
  vector<lower=RTbound, upper=max(minRT)>[N] tau;             // non-decision time
  vector<lower=-20, upper=20>[N] b1;                          // go bias in escape condition
  vector<lower=-20, upper=20>[N] b2;                          // go bias in avoid condition
  vector<lower=-20, upper=20>[N] b3;                          // differential action value weight
  vector<lower=0, upper=1>[N] w1;                             // starting point, escape condition
  vector<lower=0, upper=1>[N] w2;                             // starting point, avoid condition
  vector<lower=0, upper=1>[N] alpha;                          // learning rate for state-action values
  vector<lower=0>[N] omega;                                   // decision threshold
  
  for (i in 1:N) {
    tau[i] = Phi_approx(mu_pr[1] + sigma[1] * tau_pr[i]) * (minRT[i] - RTbound) + RTbound; 
    w1[i] = Phi_approx(mu_pr[4] + sigma[4] * w1_pr[i]);
    w2[i] = Phi_approx(mu_pr[5] + sigma[5] * w2_pr[i]);
    alpha[i] = Phi_approx(mu_pr[6] + sigma[6] * alpha_pr[i]);
  }
  b1 = mu_pr[2] + sigma[2] * b1_pr;
  b2 = mu_pr[3] + sigma[3] * b2_pr;
  b3 = mu_pr[8] + sigma[8] * b3_pr;
  omega = exp(mu_pr[7] + sigma[7] * omega_pr);
}

model {
  // for log nogo probabiliy density calculsion
  real step_size;
  real temp_rt;
  real prob_density;
  real nogo_lik;
  
  // hyperparameters
  mu_pr ~ normal(0, 1);
  sigma ~ normal(0, 0.5);                           
  
  // individual parameters for non-centered parameterization
  tau_pr ~ normal(0, 1);                   
  b1_pr ~ normal(0, 1);                    
  b2_pr ~ normal(0, 1);
  b3_pr ~ normal(0, 1);
  w1_pr ~ normal(0, 1);                    
  w2_pr ~ normal(0, 1);                    
  alpha_pr ~ normal(0, 1);                 
  omega_pr ~ normal(0, 1);
  
  // subject loop
  for (i in 1:N) {
    // assign individual variables
    real w;                                      
    int c;
    int s;
    matrix[4, 2] Q;
    
    // initial values for state-action values
    Q = rep_matrix(0.0, 4, 2);        
    
    for (t in 1:T) {
      real v;
      c = C[t, i];                                                                     
      s = cond[t, i];                                                                  
      
      w = (E[t, i] == 1) ? w1[i] : w2[i];                                              
      v = (b1[i] * E[t, i]) + (b2[i] * (1 - E[t, i])) + (b3[i] * (Q[s, 2] - Q[s, 1]));  // two drift rates
      
      // increment log probability density and update posterior
      if (c == 1) {
        rt[t, i] ~ wiener(omega[i], tau[i], w, v); 
      } else {
        nogo_lik = 0.0;
        for (n in 1:Steps) {
          step_size = (n * 1.0) / Steps;
          temp_rt = step_size * (RTmax - tau[i]) + tau[i];
          prob_density = exp(wiener_lpdf(temp_rt | omega[i], tau[i], 1-w, -v));
          nogo_lik += ((RTmax - tau[i]) / Steps) * prob_density;
          }
          if (is_nan(nogo_lik) == 1) {
          nogo_lik = 2.220446e-16;
          }
      target += log(nogo_lik);
      }
      
      // update Q-value
      Q[s, c+1] += alpha[i]*(fd[t, i] - Q[s, c+1]);
    }
  }
}

generated quantities {
  // for group level parameters
  real<lower=RTbound, upper=max(minRT)> mu_tau;         // nondecision time
  real<lower=-20, upper=20>             mu_b1;          // go bias in escape condition 
  real<lower=-20, upper=20>             mu_b2;          // go bias in avoid condition
  real<lower=-20, upper=20>             mu_b3;          // differential action value weight
  real<lower=0, upper=1>                mu_w1;          // starting point in escape condition
  real<lower=0, upper=1>                mu_w2;          // starting point in avoid condition
  real<lower=0, upper=1>                mu_alpha;       // learning rate for state-action values
  real<lower=0>                         mu_omega;       // decision threshold

  real log_lik[N];
  real nogo_lik;
  real step_size;
  real temp_rt;
  real prob_density;

  // assign group level parameter values
  mu_tau = Phi_approx(mu_pr[1]) * (mean(minRT) - RTbound) + RTbound; 
  mu_b1 = mu_pr[2];
  mu_b2 = mu_pr[3];
  mu_b3 = mu_pr[8]; 
  mu_w1 = Phi_approx(mu_pr[4]);
  mu_w2 = Phi_approx(mu_pr[5]);
  mu_alpha = Phi_approx(mu_pr[6]);
  mu_omega = exp(mu_pr[7]);
  
  {// for log likelihood calculation (local)
  for (i in 1:N) {
    // assign individual variables
    real w;                                      
    int c;
    int s;
    matrix[4, 2] Q;
    
    // initial values for state-action values
    Q = rep_matrix(0.0, 4, 2);      
    log_lik[i] = 0.0;
    
    for (t in 1:T) {
      real v;
      c = C[t, i];                                                                     
      s = cond[t, i];                                                                  
      
      w = (E[t, i] == 1) ? w1[i] : w2[i];                                              
      v = (b1[i] * E[t, i]) + (b2[i] * (1 - E[t, i])) + (b3[i] * (Q[s, 2] - Q[s, 1]));
      
      // calculate likelihood
      if (c == 1) {
        log_lik[i] += wiener_lpdf(rt[t, i] | omega[i], tau[i], w, v);
        } else {
          nogo_lik = 0.0;
          for (n in 1:Steps) {
            step_size = (n * 1.0) / Steps;
            temp_rt = step_size * (RTmax - tau[i]) + tau[i];
            prob_density = exp(wiener_lpdf(temp_rt | omega[i], tau[i], 1-w, -v));
            nogo_lik += ((RTmax - tau[i]) / Steps) * prob_density;
            }
            if (is_nan(nogo_lik) == 1) {
            nogo_lik = 2.220446e-16;
            }
        log_lik[i] += log(nogo_lik);
        }
      
      // update Q-value
      Q[s, c+1] += alpha[i]*(fd[t, i] - Q[s, c+1]);
      } 
    }
  }
}


