
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, runjags, foreach)
setwd(here::here("code_empirical"))
  

# analysis ----------------------------------------------------------------

## fish data
## "data_fmt_stock.R" calls `df_fish` through "data_fmt_fishdata.R"
source("data_fmt_stock.R")
group <- c("all", "masu_salmon", "other")

list_est <- foreach(i = seq_len(length(group))) %do% {
  
  fish_group <- group[i]
  
  df_subset <- filter(df_fish, group == fish_group)

  
  # jags --------------------------------------------------------------------
  
  ## data for jags ####
  d_jags <- list(N = df_subset$abundance,
                 Site = df_subset$site_id_numeric,
                 Year = df_subset$year - min(df_subset$year) + 1,
                 St_year = df_year$St_year,
                 End_year = df_year$End_year,
                 Area = df_subset$area,
                 Nsample = nrow(df_subset),
                 Nsite = n_distinct(df_subset$site_id),
                 
                 # Stock = 0 for fish group "other" because no stocking effect would be expected
                 Stock = case_when(fish_group == "other" ~ 0,
                                   fish_group != "other" ~ df_fry$stock),
                 Year_stock = df_fry$year_release - min(df_fry$year_release) + 1,
                 Site_stock = df_fry$site_id_numeric,
                 Nsample_stock = nrow(df_fry))
                 
  ## parameters ####
  para <- c("log_global_r",
            "sd_r_space",
            "sd_obs",
            "log_mu_r",
            "sd_r_time",
            "mu_b",
            "log_d")
  
  ## model file ####
  m <- read.jagsfile("model_ssm.R")
  
  ## mcmc setup ####
  
  n_ad <- 100
  n_iter <- 1.0E+4
  n_thin <- max(3, ceiling(n_iter / 500))
  n_burn <- ceiling(max(10, n_iter/2))
  n_sample <- ceiling(n_iter / n_thin)
  
  inits <- replicate(3,
                     list(.RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = FALSE)
  
  for (i in 1:3) inits[[i]]$.RNG.seed <- i
  
      
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
  
  # cv, mu, sigma are derived parameters
  # exclude from convergence check
  r_hat <- filter(mcmc_summary,
                  !str_detect(rownames(mcmc_summary),
                              pattern = "(^cv)|(^mu)|(^sigma)"))
  print(max(r_hat$Rhat))
  
  while(max(r_hat$Rhat) > 1.09) {
    post <- extend.jags(post,
                        burnin = 0,
                        sample = n_sample,
                        adapt = n_ad,
                        thin = n_thin,
                        n.sims = 3,
                        combine = TRUE)
    
    mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
    r_hat <- filter(mcmc_summary,
                    !str_detect(rownames(mcmc_summary),
                                pattern = "(^cv)|(^mu)|(^sigma)"))
    print(max(r_hat$Rhat))
  }
  
  n_total_mcmc <- (post$sample / n_sample) * n_iter + n_burn
    
    
  # format output -----------------------------------------------------------
    
  fn_brrm <- function(x) {
    y <- lapply(str_extract_all(x, pattern = "\\[.{1,}\\]"),
                FUN = function(z) ifelse(identical(z, character(0)),
                                         NA,
                                         z))
    str_remove_all(y, pattern = "\\[|\\]")
  }
  
  param <- rownames(mcmc_summary)
  param_name <- str_remove(param,
                           pattern = "\\[.{1,}\\]")
  
  df_site <- df_subset %>% 
    distinct(site_id_numeric,
             river,
             site,
             site_id)
  
  est <- mcmc_summary %>% 
    as_tibble() %>% 
    mutate(n_total_mcmc = n_total_mcmc,
           n_sample = post$sample,
           n_thin = n_thin,
           n_burn = n_burn,
           param_name = param_name,
           param = param,
           character_id = fn_brrm(param)) %>% 
    separate(character_id, into = c("site_id", "year_id")) %>% 
    mutate(site_id_numeric = suppressWarnings(as.numeric(site_id)),
           year_id = as.numeric(year_id),
           year = year_id + 1998) %>% 
    select(-site_id) %>% 
    left_join(df_site, by = "site_id_numeric") %>% 
    left_join(df_subset, by = c("year",
                                "river",
                                "site",
                                "site_id",
                                "site_id_numeric"))
  

  # export ------------------------------------------------------------------

  write_csv(est,
            paste0("data_fmt/data_ssm_est_", fish_group, ".csv"))
  
  return(est)
}