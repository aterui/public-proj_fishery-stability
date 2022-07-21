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
    for (k in 1:Nf) {
      nu[t, k] ~ dnorm(0, 1)
    }
  }
  
  for (k in 1:Nf) {
    
    for(i in 1:Nsp) {
      xi_eps[k, i] ~ dnorm(0, phi_eps[k, i] * theta_eps[k])
      phi_eps[k, i] ~ dgamma(1.5, 1.5)
    }
    
  }
  
  ### multiplicative gamma prior for factor loading xi_eps
  delta_eps[1] ~ dgamma(a1, 1)
  theta_eps[1] <- delta_eps[1]
  
  for(k in 2:Nf) {
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
    }
  }
  
  ## species interaction ####

  for(i in 1:Nsp) {
    for (k in 1:Nd) {
      eta[i, k] ~ dnorm(0, 1)
    }
  }

  for (k in 1:Nd) {
    for(i in 1:Nsp) {
      xi_alpha[k, i] ~ dnorm(0, phi_alpha[k, i] * theta_alpha[k])
      phi_alpha[k, i] ~ dgamma(1.5, 1.5)
    }
  }
  
  for (i in 1:Nsp) {
    alpha0[i] ~ dnorm(0, 1)T(,0)
    for (j in 1:Nsp) {
      diag_alpha[i, j] <- W[i, j] * alpha0[i]
    }
  }
  
  alpha[1:Nsp, 1:Nsp] <- eta[ , ] %*% xi_alpha[ , ] + diag_alpha[ , ]
  
  ### multiplicative gamma prior for factor loading xi_alpha
  delta_alpha[1] ~ dgamma(a1, 1)
  theta_alpha[1] <- delta_alpha[1]
  
  for(k in 2:Nd) {
    delta_alpha[k] ~ dgamma(a2, 1)
    theta_alpha[k] <- theta_alpha[k - 1] * delta_alpha[k]
  }
  
  
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
      log(d[t, i]) <- log_d[t, i]
    }
  }  
  
  ## state
  for (t in (1 + Q):Nyr) {
    log_d[t, 1:Nsp] ~ dmnorm(log_mu_d[t, ], TAU[ , ])
    log_mu_d[t, 1:Nsp] <- log_r[] + d[t - Q, ] %*% alpha[ , ] + eps[t - Q, ]
  }
  
  eps[1:(Nyr - Q), 1:Nsp] <- nu[ , ] %*% xi_eps[ , ]
  
}
