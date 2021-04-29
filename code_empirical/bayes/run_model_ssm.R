
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, runjags, foreach)
  source(here::here("data_fmt_fishdata.R"))
  dat_full <- do.call(rbind, dat_list)
  
  n_ad <- 100
  n_iter <- 1.0E+4
  n_thin <- 50
  n_burn <- ceiling(max(10, n_iter/2))
  n_sample <- ceiling(n_iter/n_thin)

# define function ---------------------------------------------------------

  rm_bracket <- function(string) {
    x <- str_extract(string, "\\[.{1,}\\]")
    y <- str_remove_all(x, "\\[|\\]")
    return(y)
  }
  

# run analysis ------------------------------------------------------------

  joint_summary <- foreach(i = 1:length(dat_list), .combine = bind_rows) %do% {
    # prepare data ------------------------------------------------------------
    
    dat <- dat_list[[i]]
    
    # print species
    print(unique(dat$LatinName))
      
    dat <- dat %>% 
      mutate(river_id = as.numeric(as.factor(river)))
    
    # data
    ## data
    Y <- dat$abundance
    log_Area <- log(dat$area)
    
    ## year, river, site indicators
    Year <- dat$year + 1 - min(dat$year, na.rm = T)
    River <- dat$river_id
    Site <- dat$site
    
    ## n iterators
    N_sample <- nrow(dat)
    N_year <- n_distinct(dat_full$year)
    N_river <- n_distinct(dat$river)
    N_site <- dat %>% 
      group_by(river_id) %>% 
      arrange(river_id) %>% 
      summarize(n_site = n_distinct(site)) %>% 
      pull(n_site)
    
  
    # jags setup --------------------------------------------------------------
  
    inits <- replicate(3,
                       list(
                         log_r_mean = rep(0, N_river),
                         tau_eps_r = rep(0.1, N_river),
                         tau_eps_site = 0.1,
                         .RNG.name = "base::Mersenne-Twister",
                         .RNG.seed = NA
                         ),
                       simplify = FALSE)
    
    for (i in 1:3) inits[[i]]$.RNG.seed <- i + 100
    
    d_jags <- list(Y = Y,
                   log_Area = log_Area,
                   Year = Year,
                   River = River,
                   Site = Site,
                   N_sample = N_sample,
                   N_year = N_year,
                   N_river = N_river,
                   N_site = N_site)
    
    para <- c("log_r_global",
              "sigma_r_global",
              "log_r_mean",
              "sigma_eps_r",
              "sigma_eps_site",
              "log_d_mean")
    
    m <- read.jagsfile("bayes/model_ssm.R")
  
    
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
    
    while(any(mcmc_summary$Rhat > 1.1)) {
      print(max(mcmc_summary$Rhat))
      message("estimates do not converge - extend simulations")
      post <- extend.jags(post,
                          burnin = 0,
                          sample = n_sample,
                          adapt = n_ad,
                          thin = n_thin,
                          n.sims = 3,
                          combine = TRUE)

      mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
    }
    
    mcmc_iter <- (post$sample / n_sample) * n_iter + n_burn
    
    # format results
    mcmc_tibble <- mcmc_summary %>% 
      as_tibble() %>% 
      mutate(mcmc_iter = mcmc_iter,
             n_thin = n_thin,
             n_burn = n_burn,
             species = unique(dat$LatinName),
             param = rownames(mcmc_summary)) %>% 
      mutate(param_id = str_remove_all(param, "\\[.{1,}\\]")) %>% 
      mutate(river_id = case_when(param_id != "log_d_mean" ~ rm_bracket(param),
                                  param_id == "log_d_mean" ~ str_remove(rm_bracket(param),
                                                                        "(.{1,},)"))
             ) %>% 
      mutate(year_id = case_when(param_id == "log_d_mean" ~ str_remove(rm_bracket(param),
                                                                       "(,.{1,})"),
                                 TRUE ~ as.character(NA))
             ) %>% 
      mutate(river_id = as.numeric(river_id),
             year_id = as.numeric(year_id))
    
    mcmc_tibble <- dat %>% 
      group_by(river) %>% 
      summarize(river_id = unique(river_id)) %>% 
      right_join(mcmc_tibble, by = "river_id")
    
    return(mcmc_tibble)
  }
  
  write_csv(joint_summary, here::here("data_fmt/data_ssm_est.csv"))
  
  
  