model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 3
  nu <- 4
  s_sq <- 2
  
  # prior -------------------------------------------------------------------
  
  ## low-level parameters ####
  for (i in 1:Nsp) {
    log_d[1, i] ~ dnorm(0, ninfo)
    log_r[i] ~ dnorm(0, ninfo)
    
    tau_obs[i] ~ dscaled.gamma(scale, df)
    sigma_obs[i] <- sqrt(1 / tau_obs[i])
  
    tau[i] ~ dscaled.gamma(scale, df)
    sigma[i] <- sqrt(1 / tau[i])
  }  
  
  ## sparse prior for alpha[i, j] for i != j ####
  ### c_sq stands for c-squared
  ### s_sq stands for s-squared
  
  for (j in 1:Nsp) {
    
    for (i in 1:Nsp) {
      log(alpha_prime[i, j]) <- log_alpha[i, j]
      log_alpha[i, j] ~ dnorm(a0[i], tau_alpha[j])
    }
    
    sigma_alpha[j] <- sqrt(1 / tau_alpha[j])
    tau_alpha[j] <- 1 / (phi * b0[j])
    b0[j] <- (sqrt(c_sq) * b[j]) / sqrt(c_sq + phi * pow(b[j], 2))
    b[j] <- inv_b[j]
    inv_b[j] ~ dscaled.gamma(1, 1)
  }
  
  phi <- 1 / inv_phi
  inv_phi ~ dscaled.gamma(1, 1)
  
  c_sq <- inv_c_sq
  inv_c_sq ~ dgamma(nu / 2, (nu * s_sq) / 2)
  
  for (i in 1:Nsp) {
    a0[i] ~ dnorm(-6, 1 / 3) 
  }
  
  ## prior for alpha[i, i]
  alpha[1:Nsp, 1:Nsp] <- diag_alpha[ , ] + (1 - W[ , ]) * alpha_prime[ , ]      
  
  for (i in 1:Nsp) {
    for (j in 1:Nsp) {
      diag_alpha[i, j] <- W[i, j] * alpha_hat[i]
    }
    log(alpha_hat[i]) <- log_alpha_hat[i]
    log_alpha_hat[i] ~ dnorm(-6, 1 / 3)
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
    for(i in 1:Nsp) {
      log_d[t, i] ~ dnorm(log_mu_d[t, i], tau[i])
      log_mu_d[t, i] <- log_d[t - Q, i] + log_r[i] - inprod(alpha[i, ], d[t - Q, ])
    }
  }
  
}
