model {
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, Q + 1)
  
  # prior -------------------------------------------------------------------
  
  ## local parameters ####
  for (i in 1:Nsite) {
    
    log_r[i] <- zeta[i, 1]
    
    for (q in 2:(Q + 1)) {
      nu[i, q - 1] <- zeta[i, q]
    }
    
    zeta[i, 1:(Q + 1)] ~ dmnorm(mu_zeta[], TAU[ , ])
  }
  
  for (i in 1:Nsite) {
    for(t in St_year[i]:(St_year[i] + Q - 1)) {
      log_d[i, t] ~ dnorm(0, tau0)
    }
  }
  
  for (i in 1:Nsite) {
    tau_r_time[i] ~ dscaled.gamma(scale0, df0)
    sd_r_time[i] <- sqrt(1 / tau_r_time[i])
    
    tau_obs[i] ~ dscaled.gamma(scale0, df0)
    sd_obs[i] <- sqrt(1 / tau_obs[i])
  }  
  
  ## regression parameters ####
  for (i in 1:Nsite) {
    b[i] ~ dnorm(mu_b, tau_b)
  }
  
  mu_b ~ dnorm(0, tau0)
  tau_b ~ dscaled.gamma(scale0, df0)
  sd_b <- sqrt(1 / tau_b)
  
  ## hyper-parameters ####
  for (k in 1:(Q + 1)) {
    mu_zeta[k] ~ dnorm(0, tau_zeta[k])
    tau_zeta[k] <- tau0 * z[k] + tau100 * (1 - z[k])
    z[k] ~ dbern(p0[k])
    p0[k] ~ dunif(0, 1)
  }
  
  TAU[1:(Q + 1), 1:(Q + 1)] ~ dscaled.wishart(v_scale0[], 2)
  OMEGA[1:(Q + 1), 1:(Q + 1)] <- inverse(TAU[ , ])
  
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    loglik[n] <- logdensity.pois(N[n], n_hat[n])
    N[n] ~ dpois(n_hat[n])
    
    n_hat[n] <- lambda[Site[n], Year[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      lambda[i, t] <- d_obs[i, t] + Psi * b[i] * stock[i, t]
      log(d_obs[i, t]) <- log_d_obs[i, t]
      log_d_obs[i, t] ~ dnorm(log_d[i, t], tau_obs[i])
      
      log(d[i, t]) <- log_d[i, t]
    }
  }  
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      log_d[i, t] ~ dnorm(log_mu_d[i, t], tau_r_time[i])
      log_mu_d[i, t] <- 
        log_r[i] + 
        inprod(nu[i, 1:Q], log_d[i, (t - Q):(t - 1)])
    }
  }
  
  
  # Bayesian p-value --------------------------------------------------------
  
  for (n in 1:Nsample) {
    residual[n] <- N[n] - n_hat[n]
    sq[n] <- pow(residual[n], 2) / n_hat[n]
    
    y_new[n] ~ dpois(n_hat[n])
    residual_new[n] <- y_new[n] - n_hat[n]
    sq_new[n] <- pow(residual_new[n], 2) / n_hat[n]
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
