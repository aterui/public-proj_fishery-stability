model {
  
  tau0 <- 0.1
  scale0 <- 2.5
  df0 <- 1
  a1 <- 2
  a2 <- 3
  
  # prior -------------------------------------------------------------------
  
  ## local parameters
  for (j in 1:Nsite) {
    
    log_r[j] ~ dnorm(0, tau0)
    
    for (q in 1:Q) {
      nu[j, q] ~ dnorm(0, tau0)
    }
  }
  
  for (j in 1:Nsite) {
    for(t in 1:Q) {
      log_d[j, t] ~ dnorm(0, tau0)
    }
  }
  
  for (j in 1:Nsite) {
    tau_obs[j] ~ dscaled.gamma(scale0, df0)
    sd_obs[j] <- sqrt(1 / tau_obs[j])
  }  
  
  ## regression parameters
  for (j in 1:Nsite) {
    b[j] ~ dnorm(mu_b, tau_b)
  }
  
  mu_b ~ dnorm(0, tau0)
  tau_b ~ dscaled.gamma(scale0, df0)
  sd_b <- sqrt(1 / tau_b)
  
  ### multiplicative gamma prior for factor loading
  delta[1] ~ dgamma(a1, 1)
  theta[1] <- delta[1]
  
  for(k in 2:Nf) {
    delta[k] ~ dgamma(a2, 1)
    theta[k] <- theta[k - 1] * delta[k]
  }
  
  ### var-covar matrix
  OMEGA[1:Nsite, 1:Nsite] <- t(xi[ , ]) %*% xi[ , ] + diag_omega[ , ]
  TAU[1:Nsite, 1:Nsite] <- inverse(OMEGA[ , ])
  
  for(i in 1:Nsite) {
    #### process error precision
    tau_r_time[i] <- TAU[i, i]
    sd_r_time[j] <- sqrt(1 / tau_r_time[j])
    
    #### unique error for site-level time series
    tau[i] ~ dscaled.gamma(scale0, df0)
    sigma[i] <- sqrt(1 / tau[i])
    
    for(j in 1:Nsite) {
      diag_omega[i, j] <- W[i, j] * pow(sigma[j], 2)
      rho[i, j] <- OMEGA[i, j] / sqrt(OMEGA[i, i] * OMEGA[j, j])
    }
  }
  
  
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
    for (t in (1 + Q):Nyear) {
      log_d[i, t] ~ dnorm(log_mu_d[i, t], tau_r_time[i])
      log_mu_d[i, t] <- 
        log_r[i] + 
        inprod(nu[i, 1:Q], log_d[i, (t - Q):(t - 1)]) +
        inprod(xi[ , i], eta[t - Q, ])
    }
  }
  
  ## factor analysis for epsilon ###
  
  ### latent variables
  for(t in 1:(Nyr - Q)) {
    for (k in 1:Nf) {
      eta[t, k] ~ dnorm(0, 1)
    }
  }
  
  for (k in 1:Nf) {
    for(i in 1:Nsite) {
      xi[k, i] ~ dnorm(0, phi[k, i] * theta[k])
      phi[k, i] ~ dgamma(1.5, 1.5)
    }
  }
  
  
  # Bayesian p-value --------------------------------------------------------
  
  ## observation
  for (n in 1:Nsample) {
    y_predict[n] <- lambda[Site[n], Year[n]] * Area[n]
    
    residual[n] <- N[n] - y_predict[n]
    sq[n] <- pow(residual[n], 2)
    
    y_new[n] ~ dpois(lambda[Site[n], Year[n]] * Area[n])
    sq_new[n] <- pow(y_new[n] - y_predict[n], 2)
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
