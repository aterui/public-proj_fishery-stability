model {
  
  ninfo <- 0.1
  scale <- 2.5
  df <- 1
  
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
  alpha[1:Nsp, 1:Nsp] <- diag_alpha[ , ] + (1 - W[ , ]) * alpha_prime[ , ]      
  
  for (i in 1:Nsp) {
    for (j in 1:Nsp) {
      ### intra-specific
      diag_alpha[i, j] <- W[i, j] * alpha_hat[i]
      
      ### inter-specific
      alpha_prime[i, j] ~ dnorm(0, tau_alpha[i, j])T(0, )
      tau_alpha[i, j] <- (1 - z[i, j]) * q1 + z[i, j] * q2
      z[i, j] ~ dbern(p)
    }
    alpha_hat[i] ~ dnorm(mu_alpha_hat, tau_alpha_hat)
  }  
  
  mu_alpha_hat ~ dnorm(0, 1)T(0,)
  tau_alpha_hat ~ dscaled.gamma(scale, df)
  
  p ~ dunif(0, 1)
  q1 <- 100
  q2 <- 1
  
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
