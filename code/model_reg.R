model {
  
  # likelihood --------------------------------------------------------------
  
  for (i in 1:Nsite) {
    ## regression for sigma (p = 1) and mu (p = 2)
    for (g in 1:Ngroup) {
      ## site-level
      log_y[i, g, 1:(Np - 1)] ~ dmnorm(mu[i, g, ], TAU[g, , ])
      
      for (p in 1:(Np - 1)) {
        mu[i, g, p] <-
          (a[1, g, p] + r[River[i], g, p]) +
          a[2, g, p] * scl_ef_stock[i] +
          a[3, g, p] * scl_log_wsd_area[i] +
          a[4, g, p] * scl_temp[i] +
          a[5, g, p] * scl_ppt[i] +
          a[6, g, p] * scl_forest[i]
      }
    }
    
    ## regression for species richness (p = 3)
    ## group for "all" (g = 1)
    y[i, 1, 3] ~ dpois(lambda[i])
    log(lambda[i]) <- 
      (a[1, 1, 3] + r[River[i], 1, 3]) +
      a[2, 1, 3] * scl_ef_stock[i] +
      a[3, 1, 3] * scl_log_wsd_area[i] +
      a[4, 1, 3] * scl_temp[i] +
      a[5, 1, 3] * scl_ppt[i] +
      a[6, 1, 3] * scl_forest[i] +
      eps[i]
    
    eps[i] ~ dnorm(0, tau_eps)
        
    ## effective release    
    scl_ef_stock[i] <- (ef_stock[i] - mean(ef_stock[])) / sd(ef_stock[])
    ef_stock[i] <- w[River[i], Reach[i]] * Stock[River[i]]
  }
    
  for (j in 1:Nriver) {
    ## weight factor
    w[j, 1:(Ngs[j] + 1)] ~ ddirch(alpha[1:(Ngs[j] + 1)])
    
    for (g in 1:Ngroup) {
      for (p in 1:Np) {
        ## watershed-level random effect
        r[j, g, p] ~ dnorm(theta[j, g, p], tau_r[g, p])
        theta[j, g, p] <- b[1, g, p] * scl_chr_a[j]
      }
    }
  }
  
  for (g in 1:Ngroup) {
    ## parameters for CV
    for (k in 1:6) {
      a[k, g, 4] <- a[k, g, 1] - a[k, g, 2]
    }
    
    for (q in 1) {
      b[q, g, 4] <- b[q, g, 1] - b[q, g, 2]
    }
    
    ## unstandardized coef
    for (p in 1:(Np + 1)) {
      beta_raw[1, g, p] <- a[1, g, p] - beta_raw[2, g, p] * mean(ef_stock[])
      beta_raw[2, g, p] <- a[2, g, p] / sd(ef_stock[])
    }
  }
  
  
  # prior -------------------------------------------------------------------
  
  scale0 <- 2.5
  df0 <- 4
  tau0 <- 0.1
  
  ## overdispersion parameter for poisson
  tau_eps ~ dscaled.gamma(scale0, df0)
  sigma[1, 3] <- sqrt(1 / tau_eps)
  
  for (g in 1:Ngroup) {
    TAU[g, 1:(Np - 1), 1:(Np - 1)] ~ dscaled.wishart(rep(scale0, 2), 2)
    OMEGA[g, 1:(Np - 1), 1:(Np - 1)] <- inverse(TAU[g, , ])
    
    for (x in 1:(Np - 1)) {
      for (y in 1:(Np - 1)) {
        RHO[g, x, y] <- OMEGA[g, x, y] / sqrt(OMEGA[g, x, x] * OMEGA[g, y, y])
      }
    }
    
    for (p in 1:(Np - 1)) {
      ## residual SD for sigma (p = 1) and mu (p = 2)
      sigma[g, p] <- sqrt(OMEGA[g, p, p])
    }
    
    for (p in 1:Np) {
      ## random SD
      tau_r[g, p] ~ dscaled.gamma(scale0, df0)
      sigma_r[g, p] <- sqrt(1 / tau_r[g, p])
      
      ## regression intercepts/slopes
      for (k in 1:6) {
        a[k, g, p] ~ dnorm(0, tau0)
      }
      
      for (q in 1) {
        b[q, g, p] ~ dnorm(0, tau0)
      }
    }
  }
  
}


data {
  
  ## convert to matrix form
  # P = 1, sigma; P = 2, mu; P = 3, species richness
  # Group = 1, all; Group = 2, masu_salmon; Group = 3, other
  
  for (n in 1:Nsample) {
    y[Site[n], Group[n], P[n]] <- Y[n] # for species richness
    log_y[Site[n], Group[n], P[n]] <- log(Y[n]) # for sigma & mu
  }
  
  ## standardize
  for (i in 1:Nsite) {
    log_wsd_area[i] <- log(Wsd_area[i])
    scl_log_wsd_area[i] <- (log_wsd_area[i] - mean(log_wsd_area[])) / sd(log_wsd_area[])
    scl_temp[i] <- (Temp[i] - mean(Temp[])) / sd(Temp[])
    scl_ppt[i] <- (Ppt[i] - mean(Ppt[])) / sd(Ppt[])
    scl_forest[i] <- (Forest[i] - mean(Forest[])) / sd(Forest[])
  }
  
  for (j in 1:Nriver) {
    scl_chr_a[j] <- (Chr_a[j] - mean(Chr_a[])) / sd(Chr_a[])
  }
  
  ## alphas for dirichlet distributions
  alpha <- rep(1, max(Ngs[]) + 1)  
}

