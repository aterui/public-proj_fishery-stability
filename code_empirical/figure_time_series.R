
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, broom)
  setwd(here::here("code_empirical"))

  
# read data ---------------------------------------------------------------

  d0 <- read_csv("data_fmt/data_ssm_est.csv") %>% 
    filter(param_name == "log_d") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(est_density = exp(median))
           
  

# plot --------------------------------------------------------------------

  d0 %>% 
    ggplot() + 
    geom_line(aes(x = year_id,
                  y = est_density,
                  color = factor(site))) +
    geom_point(aes(x = year_id,
                   y = density,
                   color = factor(site)),
               alpha = 0.5) +
    facet_wrap(facets = ~ river,
               ncol = 6,
               scales = "free_y") +
    theme_bw()
  