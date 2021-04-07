  
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, broom)
  

# read data ---------------------------------------------------------------

  d0 <- read_csv("data_fmt/data_ssm_est.csv")
  
  dat <- d0 %>% 
    filter(param_id == "log_d_mean") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(abundance = exp(median)) %>% 
    group_by(river, species) %>%
    do(mean_abundance = mean(.$abundance),
       var_abundance = var(resid(loess(abundance ~ year_id, data = .)))) %>% 
    mutate(mean_abundance = as.numeric(mean_abundance),
           var_abundance = as.numeric(var_abundance))
    
  dat %>% 
    group_by(river) %>% 
    do(taylors_z = coef(lm(log(var_abundance) ~ log(mean_abundance),
                           data = .))[2]) %>% 
    mutate(taylors_z = as.numeric(taylors_z))
    
  d0 %>% 
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    filter(param_id == "log_d_mean") %>% 
    ggplot() + 
    geom_line(aes(y = exp(median), x = year_id, color = species)) +
    facet_wrap(~ river, nrow = 2)
  
  ggplot(dat) + 
    geom_point(aes(x = log(mean_abundance), y = log(var_abundance))) +
    facet_wrap(~ river, nrow = 2)
  