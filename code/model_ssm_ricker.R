model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 3
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters
  for (i in 1:Nsp) {
    log_d[1, i] ~ dnorm(0, ninfo)
    log_r[i] ~ dnorm(0, ninfo)
    
    tau_obs[i] ~ dscaled.gamma(scale, df)
    sd_obs[i] <- sqrt(1 / tau_obs[i])
  }  
  
  ## factor analysis
  for(t in 1:(Nyr - Q)) {
    for (k in 1:Nd) {
      nu[t, k] ~ dnorm(0, 1)
    }
  }
  
  for (k in 1:Nd) {
    
    for(i in 1:Nsp) {
      xi[k, i] ~ dnorm(0, phi[k, i] * theta[k])
      phi[k, i] ~ dgamma(1.5, 1.5)
    }
    
  }
  
  ## multiplicative gamma prior
  a1 <- 2
  a2 <- 3
  
  delta[1] ~ dgamma(a1, 1)
  theta[1] <- delta[1]
  
  for(k in 2:Nd) {
    delta[k] ~ dgamma(a2, 1)
    theta[k] <- theta[k - 1] * delta[k]
  }
  
  ## var-covar matrix
  OMEGA[1:Nsp, 1:Nsp] <- t(xi[ , ]) %*% xi[ , ] + diag_omega[,]
  TAU[1:Nsp, 1:Nsp] <- inverse(OMEGA[,])
  
  for(i in 1:Nsp) {
    tau[i] ~ dscaled.gamma(2.5, 3)
    sigma[i] <- sqrt(1 / tau[i])
    for(j in 1:Nsp) {
      diag_omega[i, j] <- W[i, j] * pow(sigma[j], 2)
      rho[i, j] <- OMEGA[i, j] / sqrt(OMEGA[i, i] * OMEGA[j, j])
    }
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
    }
  }  
  
  ## state
  for (t in (1 + Q):Nyr) {
    log_d[t, 1:Nsp] ~ dmnorm(log_mu_d[t, ], TAU[ , ])
    log_mu_d[t, 1:Nsp] <- log_r[] + log_d[t - Q, ] + eps[t - Q, ]
  }
  
  eps[1:(Nyr - Q), 1:Nsp] <- nu[ , ] %*% xi[ , ]
  
}
