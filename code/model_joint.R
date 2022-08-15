model {
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    N[n] ~ dpois(n_hat[n])
    n_hat[n] <- lambda[Site[n], Year[n], Group[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      for (g in 1:Ng) {
        lambda[i, t, g] <- max(0, lambda0[i, t, g])
        lambda0[i, t, g] <- d_obs[i, t, g] + Psi[g] * b[i] * stock[i, t]
        log(d_obs[i, t, g]) <- log_d_obs[i, t, g]
        log_d_obs[i, t, g] ~ dnorm(log_d[i, t, g], tau_obs[i, g])
        
        log_d_prime[i, t, g] <- log_d[i, t, g]
      }
      
      log_d_prime[i, t, 3] <- log_d[i, t, 3]
    }  
  }
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      for (g in 1:Ng) {
        log_d[i, t, g] ~ dnorm(log_mu_d[i, t, g], tau_r_time[i, g])
        log_mu_d[i, t, g] <- 
          xi[i, 1, g] + 
          xi[i, 2, g] * w_log_d[i, t, g]
        
        w_log_d[i, t, g] <- inprod(nu[1:Q, g], log_d[i, (t - Q):(t - 1), g]) / sum(nu[1:Q, g])
      }
    }
    
    for (t in St_year[i]:End_year[i]) {
      log_d[i, t, 3] <- log(exp(log_d[i, t, 1]) + exp(log_d[i, t, 2]))
    }
  }
  
  
  # Bayesian p-value --------------------------------------------------------
  
  for (n in 1:Nsample) {
    rs[n] <- N[n] - n_hat[n]
    sqr[n] <- pow(rs[n], 2)
    
    y0[n] ~ dpois(n_hat[n])
    rs0[n] <- y0[n] - n_hat[n]
    sqr0[n] <- pow(rs0[n], 2)
  }
  
  ssqr <- sum(sqr[])
  ssqr0 <- sum(sqr0[])
  bp_value <- step(ssqr0 - ssqr)
  
  
  # prior -------------------------------------------------------------------
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, 2)
  
  ## local parameters ####
  
  ### population parameters
  for (i in 1:Nsite) {
    for (g in 1:Ng) {
      tau_r_time[i, g] ~ dscaled.gamma(scale0, df0)
      sd_r_time[i, g] <- sqrt(1 / tau_r_time[i, g])
      tau_obs[i, g] ~ dscaled.gamma(scale0, df0)
      sd_obs[i, g] <- sqrt(1 / tau_obs[i, g])
      
      xi[i, 1:2, g] ~ dmnorm(mu_xi[ , g], TAU[ , , g])
    }
  }
  
  for (i in 1:Nsite) {
    for (g in 1:Ng) {
      for(t in St_year[i]:(St_year[i] + Q - 1)) {
        log_d[i, t, g] ~ dt(log_d1[i, g], tau_t1[i, g], 5)
      }
      
      tau_t1[i, g] <- pow(log_max_d[i, g] - log_d1[i, g], -2)
    }
  }
  
  ### regression parameterS
  for (i in 1:Nsite) {
    b[i] ~ dnorm(mu_b, tau_b)
  }
  
  ## hyper parameters ####
  for (g in 1:Ng) {
    mu_xi[1, g] ~ dnorm(0, tau0)
    mu_xi[2, g] ~ dnorm(1, tau0)
    
    TAU[1:2, 1:2, g] ~ dscaled.wishart(v_scale0[], 2)
    OMEGA[1:2, 1:2, g] <- inverse(TAU[ , , g])
    
    nu0[g] ~ dunif(0, 1)
    for (q in 1:Q) {
      nu[q, g] <- pow(nu0[g], Q - q + 1)
    }
  }  
  
  mu_b ~ dnorm(0, tau0)
  tau_b ~ dscaled.gamma(scale0, df0)
  sd_b <- sqrt(1 / tau_b)
  
}


data {
  
  ## stock data conversion
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
  ## initial population size
  for (n in 1:N_t1) {
    log_d1[Site_t1[n], Group_t1[n]] <- Log_d1[n]
    log_max_d[Site_t1[n], Group_t1[n]] <- Log_max_d[n]
  }
  
}
