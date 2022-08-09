model {
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, Q + 1)
  a1 <- 10
  a2 <- 10
  
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
    for(t in 1:Q) {
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
  
  ## multiplicative gamma prior for factor loading ####
  delta[1] ~ dgamma(a1, 1)
  theta[1] <- delta[1]
  
  for(k in 2:Nf) {
    delta[k] ~ dgamma(a2, 1)
    theta[k] <- theta[k - 1] * delta[k]
  }
  
  ## var-covar matrix
  SIGMA[1:Nriver, 1:Nriver] <- t(xi[ , ]) %*% xi[ , ] + diag_sigma[ , ]
  KAPPA[1:Nriver, 1:Nriver] <- inverse(SIGMA[ , ])
  
  for(r in 1:Nriver) {
    ### process error precision
    tau_river[r] <- KAPPA[r, r]
    sd_river[r] <- sqrt(1 / tau_river[r])
    
    ### unique error for river-level time series
    tau[r] ~ dscaled.gamma(scale0, df0)
    sigma[r] <- sqrt(1 / tau[r])
    
    for(r_prime in 1:Nriver) {
      diag_sigma[r, r_prime] <- W[r, r_prime] * pow(sigma[r_prime], 2)
      RHO[r, r_prime] <- SIGMA[r, r_prime] / (sqrt(SIGMA[r, r] * SIGMA[r_prime, r_prime]))
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
      
      log_d_prime[i, t] <- log_d[i, t]
      log(d[i, t]) <- log_d[i, t]
    }
  }  
  
  ## state ####
  for (t in (1 + Q):Nyear) {
    for (i in 1:Nsite) {
      log_d[i, t] ~ dnorm(log_mu_d[i, t], tau_r_time[i])
      log_mu_d[i, t] <- 
        log_r[i] + 
        inprod(nu[i, 1:Q], log_d[i, (t - Q):(t - 1)]) +
        eps[River[i], t]
    }
    
    for(r in 1:Nriver) {
      eps[r, t] ~ dnorm(mu_eps[r, t], tau_river[r])
      mu_eps[r, t] <- inprod(xi[, r], eta[t, ])
    }
  }
  
  ### latent variables ####
  for(t in (1 + Q):Nyear) {
    for (k in 1:Nf) {
      eta[t, k] ~ dnorm(0, 1)
    }
  }
  
  for (k in 1:Nf) {
    for(r in 1:Nriver) {
      xi[k, r] ~ dnorm(0, phi[k, r] * theta[k])
      phi[k, r] ~ dgamma(1.5, 1.5)
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
