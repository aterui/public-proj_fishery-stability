model {
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    N[n] ~ dpois(n_hat[n])
    n_hat[n] <- lambda[Group[n], Site[n], Year[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      for (g in 1:(Ng - 1)) {
        lambda[g, i, t] <- max(lambda0[g, i, t], 0) # remove negative values
        lambda0[g, i, t] <- d_obs[g, i, t] + Psi[g] * b[i] * stock[i, t]
        log(d_obs[g, i, t]) <- log_d_obs[g, i, t]
        log_d_obs[g, i, t] ~ dnorm(log_d[g, i, t], tau_obs[g, i])
      }
      
      lambda[3, i, t] <- sum(lambda[1:2, i, t])
    }  
  }
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      for (g in 1:(Ng - 1)) {
        log_d[g, i, t] ~ dnorm(log_mu_d[g, i, t], tau_r_time[g, i])
        log_mu_d[g, i, t] <- 
          log_r[g, i] + 
          inprod(nu[1:Q], log_d[g, i, (t - Q):(t - 1)])
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
  v_scale0 <- rep(scale0, 2)
  
  ## local parameters ####
  for (i in 1:Nsite) {
    for (g in 1:(Ng - 1)) {
      log_r[g, i] ~ dnorm(0, tau0)

      tau_r_time[g, i] ~ dscaled.gamma(scale0, df0)
      sd_r_time[g, i] <- sqrt(1 / tau_r_time[g, i])
      
      tau_obs[g, i] ~ dscaled.gamma(scale0, df0)
      sd_obs[g, i] <- sqrt(1 / tau_obs[g, i])
    }
  }
  
  for (q in 1:Q) {
    nu[q] <- 1
  }
  
  for (i in 1:Nsite) {
    for(t in St_year[i]:(St_year[i] + Q - 1)) {
      for (g in 1:(Ng - 1)) {
        log_d[g, i, t] ~ dnorm(0, tau0)
      }
    }
  }
  
  ## regression parameters ####
  for (i in 1:Nsite) {
    b[i] ~ dnorm(mu_b, tau_b)
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
