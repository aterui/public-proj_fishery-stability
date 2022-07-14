model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 3
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters
  for (j in 1:Nsite) {
    tau_r_time[j] ~ dscaled.gamma(scale, df)
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    log_mu_r[j] ~ dnorm(log_global_r, tau_r_space)
  }
  
  for (j in 1:Nsite) {
    for(t in St_year[j]:(St_year[j] + Q - 1)) {
      log_d[j, t] ~ dnorm(0, ninfo)
    }
  }
  
  for (j in 1:Nsite) {
    tau_obs[j] ~ dscaled.gamma(scale, df)
    sd_obs[j] <- sqrt(1 / tau_obs[j])
  }  
  
  ## hyper-parameters
  log_global_r ~ dnorm(0, ninfo)
  
  tau_r_space ~ dscaled.gamma(scale, df)
  sd_r_space <- sqrt(1 / tau_r_space)
  
  ## regression parameters
  
  for (j in 1:Nsite) {
    b[j] ~ dnorm(mu_b, tau_b)
  }
  
  mu_b ~ dnorm(0, ninfo)
  tau_b ~ dscaled.gamma(scale, df)
  sd_b <- sqrt(1 / tau_b)
  
  
  # likelihood --------------------------------------------------------------
  
  ## observation
  for (i in 1:Nsample) {
    N[i] ~ dpois(lambda[Site[i], Year[i]] * Area[i])
  }
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:End_year[j]) {
      lambda[j, t] <- d_obs[j, t] + Psi * b[j] * stock[j, t]
      log(d_obs[j, t]) <- log_d_obs[j, t]
      log_d_obs[j, t] ~ dnorm(log_d[j, t], tau_obs[j])
      
      log(d[j, t]) <- log_d[j, t]
    }
  }  
  
  ## state
  for (j in 1:Nsite) {
    for (t in (St_year[j] + Q):End_year[j]) {
      log_d[j, t] <- log_r[j, t] + log_d[j, t - Q]
      log_r[j, t] ~ dnorm(log_mu_r[j], tau_r_time[j])
    }
  }
  

  # Bayesian p-value --------------------------------------------------------

  ## observation
  for (i in 1:Nsample) {
    y_predict[i] <- lambda[Site[i], Year[i]] * Area[i]
    
    residual[i] <- N[i] - y_predict[i]
    sq[i] <- pow(residual[i], 2)
    
    y_new[i] ~ dpois(lambda[Site[i], Year[i]] * Area[i])
    sq_new[i] <- pow(y_new[i] - y_predict[i], 2)
  }
  
  fit <- sum(sq[])
  fit_new <- sum(sq_new[])
  bp_value <- step(fit_new - fit)
    
}


data {
  
  ## stock data conversion
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
}
