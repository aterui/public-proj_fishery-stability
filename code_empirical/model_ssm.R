model {
  
  ninfo <- 0.01

# prior -------------------------------------------------------------------
  
  for (j in 1:Nsite) {
    tau_r_time[j] ~ dscaled.gamma(2.5, 1)
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    tau_obs[j] ~ dscaled.gamma(2.5, 1)
    sd_obs[j] <- sqrt(1 / tau_obs[j])
    
    log_mu_r[j] ~ dnorm(log_global_r, tau_r_space)
  }
  
  log_global_r ~ dnorm(0, ninfo)
  tau_r_space ~ dscaled.gamma(2.5, 1)
  sd_r_space <- sqrt(1 / tau_r_space)
  
  for (j in 1:Nsite) {
    log_d[j, St_year[j]] ~ dnorm(0, ninfo)
  }
  
# likelihood --------------------------------------------------------------
  
  for (i in 1:Nsample) {
    N[i] ~ dpois(n_obs[Site[i], Year[i]])
    log(n_obs[Site[i], Year[i]]) <- log_d_obs[Site[i], Year[i]] + log(Area[i])
  }
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:End_year[j]) {
      log_d_obs[j, t] ~ dnorm(log_d[j, t], tau_obs[j])
      log(d[j, t]) <- log_d[j, t]
    }
  }  
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:(End_year[j] - 1)) {
      log_d[j, t + 1] <- log_r[j, t] + log_d[j, t]
      log_r[j, t] ~ dnorm(log_mu_r[j], tau_r_time[j])
    }
  }
  
# statistical quantity ----------------------------------------------------

  for (j in 1:Nsite) {
    sigma[j] <- sd(d[j, St_year[j]:End_year[j]])
    mu[j] <- mean(d[j, St_year[j]:End_year[j]])
    cv[j] <- sigma[j] / mu[j]
  }
    
}