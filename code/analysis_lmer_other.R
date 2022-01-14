
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               glmmTMB,
               foreach)


# data --------------------------------------------------------------------

source("code/data_fmt_analysis.R")
df_m <- list_ssm$other


# analysis ----------------------------------------------------------------

param <- c("cv", "mu", "sigma")

list_fit <- foreach(i = seq_len(length(param))) %do% {
  dat <- df_m %>% 
    filter(response == param[i])

  fit <- glmmTMB::glmmTMB(value ~ 
                            scl_mean_stock +
                            scl_chr_a +
                            scl_wsd_area + 
                            scl_temp + 
                            scl_ppt + 
                            scl_forest + 
                            (1 | river),
                          REML = FALSE,
                          data = dat)
  
  return(fit)    
}

names(list_fit) <- param

lapply(list_fit, summary)
lapply(list_fit, confint)
