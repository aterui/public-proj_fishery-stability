model {
  
  # prior -------------------------------------------------------------------
  
  ## residual sd
  tau_obs ~ dscaled.gamma(2.5, 1)
  sd_obs <- sqrt(1 / tau_obs)
  
  ## random effect sd
  tau_r ~ dscaled.gamma(2.5, 1)
  sd_r <- sqrt(1 / tau_r)
  
  for(k in 1:2) {
    b[k] ~ dnorm(0, 0.01)
  }
  
  for(j in 1:Nriver) {
    R[j] ~ dnorm(0, tau_r)
  }
  
  ## weight factors
  for(j in 1:Nriver) {
    p[j, 1:(Nsite[j] + 1)] ~ ddirch(alpha[j, 1:(Nsite[j] + 1)])
    alpha[j, 1:(Nsite[j] + 1)] <- rep(1, Nsite[j] + 1)
  }
  
  # likelihood --------------------------------------------------------------
  
  for(i in 1:Nsample) {
    Y[i] ~ dnorm(mu[i], tau_obs)
    mu[i] <- b[1] +
      b[2] * w_stock[i] + 
      R[River[i]]
    
    w_stock[i] <- p[River[i], Site[i]] * Stock[i]
  }
  
}