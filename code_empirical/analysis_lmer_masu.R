
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               MuMIn,
               glmmTMB,
               foreach)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")
df_m <- list_ssm$masu


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
