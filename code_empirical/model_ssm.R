model {
  
  ninfo <- 0.01

# prior -------------------------------------------------------------------
  
  for (j in 1:Nsite) {
    tau_r_time[j] ~ dscaled.gamma(10, 1)
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    tau_obs[j] ~ dscaled.gamma(10, 1)
    sd_obs[j] <- sqrt(1 / tau_obs[j])
    
    log_mu_r[j] ~ dnorm(log_global_r, tau_r_space)
  }
  
  log_global_r ~ dnorm(0, ninfo)
  tau_r_space ~ dscaled.gamma(10, 1)
  sd_r_space <- sqrt(1 / tau_r_space)
  
  for (j in 1:Nsite) {
    log_d[St_year[j], j] ~ dnorm(0, ninfo)
  }
  
# likelihood --------------------------------------------------------------
  
  for (i in 1:Nsample) {
    N[i] ~ dpois(d_obs[Year[i], Site[i]] * Area[i])
  }
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:End_year[j]) {
      log(d_obs[t, j]) <- log_d_obs[t, j]
      log_d_obs[t, j] ~ dnorm(log_d[t, j], tau_obs[j])
      log(d[t, j]) <- log_d[t, j]
    }
  }  
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:(End_year[j] - 1)) {
      log_d[t + 1, j] <- log_r[t, j] + log_d[t, j]
      log_r[t, j] ~ dnorm(log_mu_r[j], tau_r_time[j])
    }
  }
  
# statistical quantity ----------------------------------------------------

  for (j in 1:Nsite) {
    cv[j] <- sd(d[St_year[j]:End_year[j], j]) / mean(d[St_year[j]:End_year[j], j])
  }
    
}