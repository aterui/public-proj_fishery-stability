
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               brms,
               foreach)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")
df_m <- list_ssm$other

# analysis ----------------------------------------------------------------

param <- c("cv", "mu", "sigma")

list_fit <- foreach(i = seq_len(length(param))) %do% {
  dat <- df_m %>% 
    filter(param_name == param[i])
  
  fit <- brm(median ~
               scl_mean_stock +
               scl_chr_a +
               scl_n_species +
               scl_wsd_area + 
               scl_temp + 
               scl_ppt + 
               scl_forest + 
               (1 | river),
             prior = c(prior(student_t(3, 0, 2.5), class = Intercept),
                       prior(student_t(3, 0, 2.5), class = b),
                       prior(student_t(3, 0, 2.5), class = sigma)),
             data = dat,
             core = 4)
  
  return(fit)    
}

