model {
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    N[n] ~ dpois(n_hat[n])
    n_hat[n] <- lambda[Site[n], Year[n], Group[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      for (g in 1:(Ng - 1)) {
        lambda[i, t, g] <- max(lambda0[i, t, g], 0) # remove negative values
        lambda0[i, t, g] <- d_obs[i, t, g] + Psi[g] * b[i] * stock[i, t]
        log(d_obs[i, t, g]) <- log_d_obs[i, t, g]
        log_d_obs[i, t, g] ~ dnorm(log_d[i, t, g], tau_obs[i, g])
      }
      
      lambda[i, t, 3] <- sum(lambda[i, t, 1:2])
    }  
  }
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      for (g in 1:(Ng - 1)) {
        log_d[i, t, g] ~ dnorm(log_mu_d[i, t, g], tau_r_time[i, g])
        log_mu_d[i, t, g] <- 
          log_r[i, g] + 
          inprod(nu[1:Q], log_d[i, (t - Q):(t - 1), g])
      }
    }
  }
  
  
  # Bayesian p-value --------------------------------------------------------
  
  # for (n in 1:Nsample) {
  #   residual[n] <- N[n] - n_hat[n]
  #   sq[n] <- pow(residual[n], 2) / n_hat[n]
  #   
  #   y_new[n] ~ dpois(n_hat[n])
  #   residual_new[n] <- y_new[n] - n_hat[n]
  #   sq_new[n] <- pow(residual_new[n], 2) / n_hat[n]
  # }
  # 
  # fit <- sum(sq[])
  # fit_new <- sum(sq_new[])
  # bp_value <- step(fit_new - fit)
  
  
  # prior -------------------------------------------------------------------
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, Ng - 1)
  
  ## local parameters ####
  
  ### population parameters
  for (i in 1:Nsite) {
    log_r[i, 1:2] ~ dmnorm(v_log_r[], TAU[ , ])
    
    for (g in 1:(Ng - 1)) {
      tau_r_time[i, g] ~ dscaled.gamma(scale0, df0)
      sd_r_time[i, g] <- sqrt(1 / tau_r_time[i, g])
      
      tau_obs[i, g] ~ dscaled.gamma(scale0, df0)
      sd_obs[i, g] <- sqrt(1 / tau_obs[i, g])
    }
  }
  
  for (q in 1:Q) {
    nu[q] <- 1
  }
  
  for (i in 1:Nsite) {
    for(t in St_year[i]:(St_year[i] + Q - 1)) {
      for (g in 1:(Ng - 1)) {
        log_d[i, t, g] ~ dnorm(0, tau0)
      }
    }
  }
  
  ### regression parameterS
  for (i in 1:Nsite) {
    b[i] ~ dnorm(mu_b, tau_b)
  }
  
  ## hyper parameters ####
  for (g in 1:(Ng - 1)) {
    v_log_r[g] ~ dnorm(0, tau0)
  }  
  
  TAU[1:(Ng - 1), 1:(Ng - 1)] ~ dscaled.wishart(v_scale0[], 2)
  OMEGA[1:(Ng - 1), 1:(Ng - 1)] <- inverse(TAU[ , ])
  
  for (g in 1:(Ng - 1)) {
    for (h in 1:(Ng - 1)) {
      rho[g, h] <- OMEGA[g, h] / sqrt(OMEGA[g, g] * OMEGA[h, h])
    }
  }
  
  mu_b ~ dnorm(0, tau0)
  tau_b ~ dscaled.gamma(scale0, df0)
  sd_b <- sqrt(1 / tau_b)
}


data {
  
  ## stock data conversion
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
}
