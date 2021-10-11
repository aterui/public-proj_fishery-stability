model {
  
  scale <- 2.5
  df <- 2
  ninfo <- 0.1
  
# prior -------------------------------------------------------------------
  
  tau ~ dscaled.gamma(scale, df)
  sigma <- sqrt(1 / tau)

  tau_r ~ dscaled.gamma(scale, df)
  sigma_r <- sqrt(1 / tau_r)
    
  for(i in 1:5) {
    a[i] ~ dnorm(0, ninfo)
  }
  
  for(i in 1:3) {
    b[i] ~ dnorm(0, ninfo)
  }
  

# likelihood --------------------------------------------------------------
  
  ## site-level
  for(i in 1:Nsite) {
    
    Y[i] ~ dnorm(mu[i], tau)
    
    mu[i] <-
      a0[River[i]] + 
      a[1] * scl_wsd_area[i] +
      a[2] * scl_temp[i] +
      a[3] * scl_ppt[i] +
      a[4] * scl_forest[i] +
      a[5] * scl_n_species[i]
      
  }
  
  ## watershed-level
  for(j in 1:Nriver) {
    
    a0[j] ~ dnorm(theta[j], tau_r)
    
    theta[j] <- 
      b[1] +
      b[2] * scl_stock[j] +
      b[3] * scl_chr_a[j]
    
  }

  
# standardization ---------------------------------------------------------
  
  for(i in 1:Nsite) {
    scl_n_species[i] <- (N_species[i] - mean(N_species[])) / sd(N_species[])
    scl_wsd_area[i] <- (Wsd_area[i] - mean(Wsd_area[])) / sd(Wsd_area[])
    scl_temp[i] <- (Temp[i] - mean(Temp[])) / sd(Temp[])
    scl_ppt[i] <- (Ppt[i] - mean(Ppt[])) / sd(Ppt[])
    scl_forest[i] <- (Forest[i] - mean(Forest[])) / sd(Forest[])
  }
  
  for(j in 1:Nriver) {
    scl_stock[j] <- (Stock[j] - mean(Stock[])) / sd(Stock[])
    scl_chr_a[j] <- (Chr_a[j] - mean(Chr_a[])) / sd(Chr_a[])
  }
  
  b_raw[1] <- b[1] - b[2] * mean(Stock[])
  b_raw[2] <- b[2] / sd(Stock[])
    
}