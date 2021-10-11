
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               MuMIn,
               glmmTMB,
               foreach)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")

df_ssm <- read_csv("data_fmt/data_ssm_est_masu_salmon.csv") %>% 
  filter(param_name %in% c("cv", "mu", "sigma")) %>% 
  rename(median = '50%',
         high = '97.5%',
         low = '2.5%') %>% 
  select(river,
         site,
         site_id,
         param_name,
         median,
         high,
         low)

df_m <- df_ssm %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_sp, by = c("river", "site", "site_id")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         scl_mean_stock = ifelse(is.na(scl_mean_stock), 0, scl_mean_stock))


# analysis ----------------------------------------------------------------

param <- c("cv", "mu", "sigma")

list_fit <- foreach(i = seq_len(length(param))) %do% {
  dat <- df_m %>% 
    filter(param_name == param[i])
  
  fit <- lme4::lmer(median ~ scl_n_species +
                      scl_mean_stock +
                      scl_chr_a +
                      scl_wsd_area + 
                      scl_temp + 
                      scl_forest + 
                      scl_ppt + 
                      (1 | river),
                    REML = FALSE,
                    data = dat)
  
  return(fit)    
}

names(list_fit) <- param

lapply(list_fit, confint)
