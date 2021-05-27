model {

  # constant ----------------------------------------------------------------
  
  ninfo <- 1.0E-3
  u <- 2.5
  df <- 1
  
  # prior -------------------------------------------------------------------
  
  ## site-level random effect
  tau_eps_site ~ dscaled.gamma(u, df)
  sigma_eps_site <- 1 / sqrt(tau_eps_site)
  
  for (i in 1:N_river) {
    ## mean growth rate r
    log_r_mean[i] ~ dnorm(log_r_global, tau_r_global)
    tau_eps_r[i] ~ dscaled.gamma(u, df)
    sigma_eps_r[i] <- 1 / sqrt(tau_eps_r[i])
    
    ## initial density
    log_d_mean[1, i] ~ dnorm(0, 0.1)
  }
  
  ## priors for global r
  log_r_global ~ dnorm(0, ninfo)
  tau_r_global ~ dscaled.gamma(u, df)
  sigma_r_global <- 1 / sqrt(tau_r_global)
  
  # observation -------------------------------------------------------------
  
  for (n in 1:N_sample) {
    Y[n] ~ dpois(n_obs[Year[n], River[n], Site[n]])
    log(n_obs[Year[n], River[n], Site[n]]) <- log_d_obs[Year[n], River[n], Site[n]] +
                                              log_Area[n]
  }

  
  # state -------------------------------------------------------------------
  
  ## metapopulation-level dynamics
  for (t in 1:(N_year - 1)) {
    for (i in 1:N_river) {
      log_d_mean[t + 1, i] <- log_r[t, i] + log_d_mean[t, i]
      log_r[t, i] ~ dnorm(log_r_mean[i], tau_eps_r[i])
    } 
  }
  
  
  ## site-level random effect
  for (i in 1:N_river) {
    for (j in 1:N_site[i]) {
      for (t in 1:N_year) {
          log_d_obs[t, i, j] <- log_d_mean[t, i] + eps_site[i, j]
      }
      eps_site[i, j] ~ dnorm(0, tau_eps_site)
    }
  }
    
}



