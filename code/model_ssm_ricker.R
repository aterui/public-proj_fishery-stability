model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 3
  a1 <- 2
  a2 <- 3
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters ####
  for (i in 1:Nsp) {
    log_d[1, i] ~ dnorm(0, ninfo)
    log_r[i] ~ dnorm(0, ninfo)
    
    tau_obs[i] ~ dscaled.gamma(scale, df)
    sd_obs[i] <- sqrt(1 / tau_obs[i])
  }  
  
  ## process error ####
  for(t in 1:(Nyr - Q)) {
    for (k in 1:Nd) {
      nu[t, k] ~ dnorm(0, 1)
    }
  }
  
  for (k in 1:Nd) {
    
    for(i in 1:Nsp) {
      xi_eps[k, i] ~ dnorm(0, phi_eps[k, i] * theta_eps[k])
      phi_eps[k, i] ~ dgamma(1.5, 1.5)
    }
    
  }
  
  ### multiplicative gamma prior for factor loadings xi
  delta_eps[1] ~ dgamma(a1, 1)
  theta_eps[1] <- delta_eps[1]
  
  for(k in 2:Nd) {
    delta_eps[k] ~ dgamma(a2, 1)
    theta_eps[k] <- theta_eps[k - 1] * delta_eps[k]
  }
  
  ## var-covar matrix
  OMEGA[1:Nsp, 1:Nsp] <- t(xi_eps[ , ]) %*% xi_eps[ , ] + diag_omega[ , ]
  TAU[1:Nsp, 1:Nsp] <- inverse(OMEGA[ , ])
  
  for(i in 1:Nsp) {
    
    tau[i] ~ dscaled.gamma(scale, df)
    sigma[i] <- sqrt(1 / tau[i])
    
    for(j in 1:Nsp) {
      
      diag_omega[i, j] <- W[i, j] * pow(sigma[j], 2)
      rho[i, j] <- OMEGA[i, j] / sqrt(OMEGA[i, i] * OMEGA[j, j])
      alpha[i, j] ~ dnorm(0, 1)
      
    }
  }
  
  # ## species interaction ####
  # 
  # for(i in 1:Nsp) {
  #   for (k in 1:Nd) {
  #     eta[i, k] ~ dnorm(0, 1)
  #   }
  # }
  # 
  # for (k in 1:Nd) {
  #   
  #   for(i in 1:Nsp) {
  #     xi_alpha[k, i] ~ dnorm(0, phi_alpha[k, i] * theta_alpha[k])
  #     phi_alpha[k, i] ~ dgamma(1.5, 1.5)
  #   }
  #   
  # }
  # 
  
  # likelihood --------------------------------------------------------------
  
  ## observation
  for (n in 1:Nsample) {
    N[n] ~ dpois(lambda[Year[n], Species[n]] * Area[n])
  }
  
  for (t in 1:Nyr) {
    for (i in 1:Nsp) {
      lambda[t, i] <- d_obs[t, i]
      log(d_obs[t, i]) <- log_d_obs[t, i]
      log_d_obs[t, i] ~ dnorm(log_d[t, i], tau_obs[i])
    }
  }  
  
  ## state
  for (t in (1 + Q):Nyr) {
    log_d[t, 1:Nsp] ~ dmnorm(log_mu_d[t, ], TAU[ , ])
    log_mu_d[t, 1:Nsp] <- log_d[t - Q, ] %*% alpha[ , ] + log_r[] + eps[t - Q, ]
  }
  
  eps[1:(Nyr - Q), 1:Nsp] <- nu[ , ] %*% xi_eps[ , ]
  
}
