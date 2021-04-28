
# setup -------------------------------------------------------------------

  rm(list = ls())
  pacman::p_load(tidyverse, broom)


# read data ---------------------------------------------------------------

  d0 <- read_csv("data_fmt/data_ssm_est.csv") %>% 
    filter(param_id == "log_d_mean") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(abundance = exp(median)*100)
  

# plot --------------------------------------------------------------------

  d0 %>% 
    ggplot() + 
    geom_line(aes(x = year_id, y = abundance, color = species)) +
    geom_point(aes(x = year_id, y = abundance, color = species),
               alpha = 0.3) +
    facet_wrap(facets = ~ river, ncol = 6, scales = "free_y") +
    theme_bw()
  