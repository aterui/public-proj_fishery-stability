model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 2
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters ####
  for (j in 1:Nsite) {
    ### community
    tau_r_time[j] ~ dscaled.gamma(scale, df)
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    tau_obs[j] ~ dscaled.gamma(scale, df)
    sd_obs[j] <- sqrt(1 / tau_obs[j])
    
    log_mu_r[j] ~ dnorm(log_global_r, tau_r_space)
    
    ### masu salmon
    tau_phi_time[j] ~ dscaled.gamma(scale, df)
    sd_phi_time[j] <- sqrt(1 / tau_phi_time[j])
    
    tau_m_obs[j] ~ dscaled.gamma(scale, df)
    sd_m_obs[j] <- sqrt(1 / tau_m_obs[j])
    
    log_mu_phi[j] ~ dnorm(log_global_phi, tau_phi_space)
    logit_mu_p[j] ~ dnorm(logit_global_p, tau_p_space)
  }
  
  for (j in 1:Nsite) {
    log_d[j, St_year[j]] ~ dnorm(0, ninfo)
    logit_p[j, St_year[j]] ~ dnorm(0, ninfo)
  }
  
  ## hyper-parameters ####
  ### community
  log_global_r ~ dnorm(0, ninfo)
  tau_r_space ~ dscaled.gamma(scale, df)
  sd_r_space <- sqrt(1 / tau_r_space)
  
  ### masu salmon
  log_global_phi ~ dnorm(0, ninfo)
  tau_phi_space ~ dscaled.gamma(scale, df)
  sd_phi_space <- sqrt(1 / tau_phi_space)
  
  logit_global_p ~ dnorm(0, ninfo)
  tau_p_space ~ dscaled.gamma(scale, df)
  sd_p_space <- sqrt(1 / tau_p_space)
    
  ## regression parameters ####
  ### community
  for (j in 1:Nsite) {
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)
  }
  
  mu_alpha ~ dnorm(0, ninfo)
  tau_alpha ~ dscaled.gamma(scale, df)
  sd_alpha <- sqrt(1 / tau_alpha)
  
  ### masu salmon
  for (j in 1:Nsite) {
    beta[j] ~ dnorm(mu_beta, tau_beta)
  }
  
  mu_beta ~ dnorm(0, ninfo)
  tau_beta ~ dscaled.gamma(scale, df)
  sd_beta <- sqrt(1 / tau_beta)
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (i in 1:Nsample) {
    N[i] ~ dpois(lambda[Site[i], Year[i]] * Area[i])
    N_m[i] ~ dpois(lambda_m[Site[i], Year[i]] * Area[i])
  }
  
  for (j in 1:Nsite) {
    for (t in St_year[j]:End_year[j]) {
      ### community
      lambda[j, t] <- d_obs[j, t] + alpha[j] * stock[j, t]
      log(d_obs[j, t]) <- log_d_obs[j, t]
      log_d_obs[j, t] ~ dnorm(log_d[j, t], tau_obs[j])
      
      ### masu salmon
      lambda_m[j, t] <- d_m_obs[j, t] + beta[j] * stock[j, t]
      log(d_m_obs[j, t]) <- log_d_m_obs[j, t]
      log_d_m_obs[j, t] ~ dnorm(log_d_m[j, t], tau_m_obs[j])
      log_d_m[j, t] <- log_d[j, t] + log(p[j, t])
      
      ### variable conversion
      log(d[j, t]) <- log_d[j, t]
      log(d_m[j, t]) <- log_d_m[j, t]
      d_u[j, t] <- d[j, t] - d_m[j, t]
      
      logit(p[j, t]) <- logit_p[j, t]
    }
    logit(mu_p[j]) <- logit_mu_p[j]
  }  
  
  ## state ####
  for (j in 1:Nsite) {
    for (t in St_year[j]:(End_year[j] - 1)) {
      logit_p[j, t + 1] <- logit_mu_p[j] + log_phi[j, t] + logit_p[j, t]
      log_d[j, t + 1] <- log_r[j, t] + log_d[j, t]
      
      log_r[j, t] ~ dnorm(log_mu_r[j], tau_r_time[j])
      log_phi[j, t] ~ dnorm(log_mu_phi[j], tau_phi_time[j])
    }
  }
  
  ## stock data conversion ####
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
  # statistical quantity ----------------------------------------------------
  
  for (j in 1:Nsite) {
    sigma[j] <- sd(d[j, St_year[j]:End_year[j]])
    mu[j] <- mean(d[j, St_year[j]:End_year[j]])
    cv[j] <- sigma[j] / mu[j]
  }
  
}