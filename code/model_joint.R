model {
  
  # likelihood --------------------------------------------------------------
  
  ## observation ####
  for (n in 1:Nsample) {
    N[n] ~ dpois(n_hat[n])
    n_hat[n] <- lambda[Site[n], Year[n], Group[n]] * Area[n]
  }
  
  for (i in 1:Nsite) {
    for (t in St_year[i]:End_year[i]) {
      for (g in 1:(Ng - 1)) {
        lambda[i, t, g] <- max(lambda0[i, t, g], 0) # remove negative values
        lambda0[i, t, g] <- d_obs[i, t, g] + Psi[g] * b[i] * stock[i, t]
        log(d_obs[i, t, g]) <- log_d_obs[i, t, g]
        log_d_obs[i, t, g] ~ dnorm(log_d[i, t, g], tau_obs[i, g])
      }
      
      lambda[i, t, 3] <- sum(lambda[i, t, 1:2])
    }  
  }
  
  ## state ####
  for (i in 1:Nsite) {
    for (t in (St_year[i] + Q):End_year[i]) {
      for (g in 1:(Ng - 1)) {
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
    residual[n] <- N[n] - n_hat[n]
    sq[n] <- pow(residual[n], 2)

    y_new[n] ~ dpois(n_hat[n])
    residual_new[n] <- y_new[n] - n_hat[n]
    sq_new[n] <- pow(residual_new[n], 2)
  }

  fit <- sum(sq[])
  fit_new <- sum(sq_new[])
  bp_value <- step(fit_new - fit)
  
  
  # prior -------------------------------------------------------------------
  
  tau0 <- 1 / 50
  tau100 <- 100
  scale0 <- 2.5
  df0 <- 3
  v_scale0 <- rep(scale0, 2)
  
  ## local parameters ####
  
  ### population parameters
  for (g in 1:(Ng - 1)) {
    for (i in 1:Nsite) {
      # log_r[i, g] ~ dmnorm(log_mu_r[g], tau_r_space[g])
      tau_r_time[i, g] ~ dscaled.gamma(scale0, df0)
      sd_r_time[i, g] <- sqrt(1 / tau_r_time[i, g])
      tau_obs[i, g] ~ dscaled.gamma(scale0, df0)
      sd_obs[i, g] <- sqrt(1 / tau_obs[i, g])
      
      xi[i, 1:2, g] ~ dmnorm(mu_xi[ , g], TAU[ , , g])
    }
  }
  
  for (i in 1:Nsite) {
    for(t in St_year[i]:(St_year[i] + Q - 1)) {
      for (g in 1:(Ng - 1)) {
        log_d[i, t, g] ~ dnorm(log_d1[i, g], 0.1)T(, log_max_d[i, g])
      }
    }
  }
  
  ### regression parameterS
  for (i in 1:Nsite) {
    b[i] ~ dnorm(mu_b, tau_b)
  }
  
  ## hyper parameters ####
  for (g in 1:(Ng - 1)) {
    # log_mu_r[g] ~ dnorm(0, tau0)
    # tau_r_space[g] ~ dscaled.gamma(scale0, df0)
    # sd_r_space[g] <- sqrt(1 / tau_r_space[g])
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
  
  # for (t in 1:(max(St_year[]) + Q - 1)) {
  #   for (g in 1:(Ng - 1)) {
  #     mu_t1[t, g] ~ dnorm(0, tau0)
  #     tau_t1[t, g] ~ dscaled.gamma(scale0, df0) 
  #     sd_t1[t, g] <- sqrt(1 / tau_t1[t, g])
  #   }    
  # }
}


data {
  
  ## stock data conversion
  for (n in 1:Nsample_stock) {
    stock[Site_stock[n], Year_stock[n]] <- Stock[n]
  }
  
  for (n in 1:N_t1) {
    log_d1[Site_t1[n], Group_t1[n]] <- Log_d1[n]
    log_max_d[Site_t1[n], Group_t1[n]] <- Log_max_d[n]
  }
    
}
