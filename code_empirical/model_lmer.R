model {
  
  scale <- 2.5
  df <- 1
  
  # prior -------------------------------------------------------------------
  
  ## residual sd ####
  tau_obs ~ dscaled.gamma(scale, df)
  sd_obs <- sqrt(1 / tau_obs)
  
  ## random effect sd ####
  tau_r ~ dscaled.gamma(scale, df)
  sd_r <- sqrt(1 / tau_r)
  
  for(k in 1:6) {
    b[k] ~ dnorm(0, 0.01)
  }
  
  for(j in 1:Nriver) {
    R[j] ~ dnorm(0, tau_r)
  }
  
  ## weight factors ####
  for(j in 1:Nriver) {
    p[j, 1:(Nsite[j] + 1)] ~ ddirch(alpha[j, 1:(Nsite[j] + 1)])
    alpha[j, 1:(Nsite[j] + 1)] <- rep(1, Nsite[j] + 1)
  }
  
  # likelihood --------------------------------------------------------------
  
  ## equation #### 
  for(i in 1:Nsample) {
    Y[i] ~ dnorm(mu[i], tau_obs)
    mu[i] <- b[1] +
             b[2] * w_stock[i] + 
             b[3] * Area[i] + 
             b[4] * Precipitation[i] + 
             b[5] * Temperature[i] +
             b[6] * Forest[i] +
             R[River[i]]
    
    w_stock[i] <- p[River[i], Site[i]] * Stock[i]
  }
  

  ## scaling ####
  scl_b[1] <- b[1] - (b[2] * mean(w_stock[]) +
                      b[3] * mean(Area[]) +
                      b[4] * mean(Precipitation[]) +
                      b[5] * mean(Temperature[]) +
                      b[6] * mean(Forest[]))
  scl_b[2] <- b[2] * sd(w_stock[])
  scl_b[3] <- b[3] * sd(Area[])
  scl_b[4] <- b[4] * sd(Precipitation[]) 
  scl_b[5] <- b[5] * sd(Temperature[])
  scl_b[6] <- b[6] * sd(Forest[])
  
  ## probability ####
  for(k in 1:6) {
    p_b[k] <- step(b[k])
  }
  
}