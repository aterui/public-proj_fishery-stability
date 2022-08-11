model {
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, 2)

    
  # prior -------------------------------------------------------------------
  
  ## local parameters ####
  phi0 ~ dunif(0, 1)
  for (q in 1:Q) {
    phi[q] <- pow(phi0, Q - q + 1)
  }
  
  for (i in 1:Nsite) {
    log_r[i] <- beta[i, 1]
    xi[i] <- beta[i, 2]
    beta[i, 1:2] ~ dmnorm(mu_beta[], TAU[ , ])
  }
  
  for (i in 1:Nsite) {
    for(t in St_year[i]:(St_year[i] + Q - 1)) {
      log_d[i, t] ~ dt(Log_d1[i], 0.1, 1)T(, Log_max_d[i])
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
  mu_beta[1] ~ dnorm(0, tau0)
  mu_beta[2] ~ dnorm(1, tau0)
  
  TAU[1:2, 1:2] ~ dscaled.wishart(v_scale0[], 2)
  OMEGA[1:2, 1:2] <- inverse(TAU[ , ])
  
    
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    loglik[n] <- logdensity.pois(N[n], n_hat[n])
    N[n] ~ dpois(n_hat[n])
    
    n_hat[n] <- lambda[Site[n], Year[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      lambda[i, t] <- max(lambda0[i, t], 0)
      lambda0[i, t] <- d_obs[i, t] + Psi * b[i] * stock[i, t]
      log(d_obs[i, t]) <- log_d_obs[i, t]
      log_d_obs[i, t] ~ dnorm(log_d[i, t], tau_obs[i])
    }
  }  
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      log_d[i, t] ~ dnorm(log_mu_d[i, t], tau_r_time[i])
      log_mu_d[i, t] <- log_r[i] + xi[i] * w_log_d[i, t]
      w_log_d[i, t] <- inprod(phi[1:Q], log_d[i, (t - Q):(t - 1)]) / sum(phi[1:Q])
    }
  }
  
  
  # Bayesian p-value --------------------------------------------------------
  
  for (n in 1:Nsample) {
    residual[n] <- N[n] - n_hat[n]
    sq[n] <- pow(residual[n], 2)
    
    y_new[n] ~ dpois(n_hat[n])
    residual_new[n] <- y_new[n] - n_hat[n]
    sq_new[n] <- pow(residual_new[n], 2)
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
