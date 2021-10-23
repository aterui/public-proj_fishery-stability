model {
  
  ninfo <- 0.01
  scale <- 2.5
  df <- 3
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters ####
  for (j in 1:Nsite) {
    tau_r_time[j] ~ dscaled.gamma(scale, df)
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    log_mu_r[j] ~ dnorm(log_global_r, tau_r_space)
  }
  
  for (j in 1:Nsite) {
    log_d[j, St_year[j]] ~ dnorm(0, ninfo)
  }
  
  
  ## hyper-parameters ####
  log_global_r ~ dnorm(0, ninfo)
  
  tau_r_space ~ dscaled.gamma(scale, df)
  sd_r_space <- sqrt(1 / tau_r_space)
  
  tau_obs ~ dscaled.gamma(scale, df)
  sd_obs <- sqrt(1 / tau_obs)
  
  ## regression parameters
  
  for (j in 1:Nsite) {
    b[j] <- mu_b
  }
  
  mu_b ~ dnorm(0, ninfo)
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (i in 1:Nsample) {
    N[i] ~ dpois(lambda[Site[i], Year[i]] * Area[i])
  }
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:End_year[j]) {
      lambda[j, t] <- d_obs[j, t] + b[j] * stock[j, t]
      log(d_obs[j, t]) <- log_d_obs[j, t]
      log_d_obs[j, t] ~ dnorm(log_d[j, t], tau_obs)
      
      log(d[j, t]) <- log_d[j, t]
    }
  }  
  
  ## state ####
  for (j in 1:Nsite) {
    for (t in St_year[j]:(End_year[j] - 1)) {
      log_d[j, t + 1] <- log_r[j, t] + log_d[j, t]
      log_r[j, t] ~ dnorm(log_mu_r[j], tau_r_time[j])
    }
  }
  
}


data {
  
  ## stock data conversion ####
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
}
