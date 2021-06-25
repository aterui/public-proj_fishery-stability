
# setup -------------------------------------------------------------------

  rm(list = ls())
  pacman::p_load(tidyverse, runjags, foreach)
  setwd(here::here("code_empirical"))
  
  
# data --------------------------------------------------------------------

  source("data_fmt_fishdata.R")
  
  d_jags <- list(N = dat$abundance,
                 Site = dat$site_id_numeric,
                 Year = dat$year - min(dat$year) + 1,
                 St_year = dat_year$St_year,
                 End_year = dat_year$End_year,
                 Area = dat$area,
                 Nsample = nrow(dat),
                 Nsite = n_distinct(dat$site_id))
                 
  para <- c("log_global_r",
            "sd_r_space",
            "cv")
  
  m <- read.jagsfile("model_ssm.R")


# mcmc setup --------------------------------------------------------------

  n_ad <- 100
  n_iter <- 1.0E+4
  n_thin <- 50
  n_burn <- ceiling(max(10, n_iter/2))
  n_sample <- ceiling(n_iter/n_thin)
  
  inits <- replicate(3,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = FALSE)
  
  for (i in 1:3) inits[[i]]$.RNG.seed <- i + 100

    
# run jags ----------------------------------------------------------------

  post <- run.jags(m$model,
                   monitor = para,
                   data = d_jags,
                   n.chains = 3,
                   inits = inits,
                   method = "parallel",
                   burnin = n_burn,
                   sample = n_sample,
                   adapt = n_ad,
                   thin = n_thin,
                   n.sims = 3,
                   module = "glm")
  
  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
