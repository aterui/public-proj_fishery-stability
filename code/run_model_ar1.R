
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/function_set.R"))


# common setup ------------------------------------------------------------

## fish data ####
## "data_fmt_stock.R" calls `df_fish` through "data_fmt_fishdata.R"
source("code/data_fmt_stock.R")
group <- c("all", "masu_salmon", "other")
Order <- 1

## mcmc setup ####
n_ad <- 1000
n_iter <- 1E+4
n_thin <- max(3, ceiling(n_iter / 250))
n_burn <- ceiling(max(10, n_iter/2))
n_chain <- 4
n_sample <- ceiling(n_iter / n_thin)
sj <- F

inits <- replicate(n_chain,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (j in 1:n_chain) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 2

## model file ####
m <- read.jagsfile("code/model_ar.R")

## parameters ####
para <- c("bp_value",
          "mu_zeta",
          "OMEGA",
          "zeta",
          "sd_r_time",
          "sd_river",
          "sd_obs",
          "mu_b",
          "b",
          "sd_b",
          "log_d",
          "loglik")

# jags --------------------------------------------------------------------

## loop over all, masu, and other fish groups
list_est <- foreach(i = seq_len(length(group))) %do% {
  
  fish_group <- group[i]
  df_subset <- filter(df_fish, group == fish_group)
  
  ## data for jags ####
  d_jags <- list(N = df_subset$abundance,
                 Site = df_subset$site_id_numeric,
                 Year = df_subset$year - min(df_subset$year) + 1,
                 River = as.numeric(factor(df_subset$river)),
                 St_year = df_year$St_year,
                 End_year = df_year$End_year,
                 Area = df_subset$area,
                 Nsample = nrow(df_subset),
                 Nsite = n_distinct(df_subset$site_id),
                 Nyear = n_distinct(df_subset$year),
                 Nriver = n_distinct(df_subset$river),
                 
                 # Psi = 0 for fish group "other" because no stocking effect would be expected
                 Psi = ifelse(fish_group == "other", 0, 1),
                 Stock = df_fry$stock,
                 Year_stock = df_fry$year_release - min(df_fry$year_release) + 1,
                 Site_stock = df_fry$site_id_numeric,
                 Nsample_stock = nrow(df_fry),
                 
                 # order of auto-regressive process
                 Q = Order)
  
  ## run jags ####
  post <- run.jags(m$model,
                   monitor = para,
                   data = d_jags,
                   n.chains = n_chain,
                   inits = inits,
                   method = "parallel",
                   burnin = n_burn,
                   sample = n_sample,
                   adapt = n_ad,
                   thin = n_thin,
                   n.sims = n_chain,
                   module = "glm",
                   silent.jags = sj)
  
  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
  print(paste(max(mcmc_summary$Rhat, na.rm = T),
              rownames(mcmc_summary)[which.max(mcmc_summary$Rhat)]))
  
  while(max(mcmc_summary$Rhat, na.rm = T) >= 1.1) {
    post <- extend.jags(post,
                        burnin = 0,
                        sample = n_sample,
                        adapt = n_ad,
                        thin = n_thin,
                        n.sims = n_chain,
                        combine = TRUE,
                        silent.jags = sj)
    
    mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
    print(paste(max(mcmc_summary$Rhat, na.rm = T),
                rownames(mcmc_summary)[which.max(mcmc_summary$Rhat)]))
  }
  
  saveRDS(post,
          file = here::here(paste0("result/post_ar",
                                   Order,
                                   "_",
                                   fish_group, ".rds")))
  
  MCMCvis::MCMCtrace(post$mcmc,
                     params = para[-which(para %in% c("bp_value",
                                                      "log_d",
                                                      "loglik"))],
                     filename = paste0("result/mcmc_trace_ar",
                                       Order,
                                       "_",
                                       fish_group))
  
  ## format output ####
  n_total_mcmc <- (post$sample / n_sample) * n_iter + post$burnin
  
  df_site <- df_subset %>% 
    distinct(site_id_numeric,
             river,
             site,
             site_id)
  
  est <- mcmc_summary %>% 
    mutate(n_total_mcmc = n_total_mcmc,
           n_sample = post$sample,
           n_thin = n_thin,
           n_burn = n_burn,
           param = rownames(.),
           param_name = str_remove(param,
                                   pattern = "\\[.{1,}\\]"),
           character_id = fn_brrm(param)) %>% 
    as_tibble() %>% 
    separate(character_id,
             into = c("site_id_numeric", "year_id_numeric"),
             convert = TRUE,
             fill = "right") %>% 
    mutate(year = year_id_numeric + 1998) %>% 
    left_join(df_site, by = "site_id_numeric") %>% 
    left_join(df_subset, by = c("year",
                                "river",
                                "site",
                                "site_id",
                                "site_id_numeric")) %>% 
    relocate(param_name,
             param,
             site_id,
             site_id_numeric,
             year_id_numeric) %>% 
    mutate(year_id_numeric = ifelse(param_name == "theta",
                                    NA,
                                    year_id_numeric))
  
  print(fish_group)  
  return(est)
}

names(list_est) <- group


# export ------------------------------------------------------------------

saveRDS(list_est, here::here(paste0("data_fmt/data_ar", Order, ".rds")))
