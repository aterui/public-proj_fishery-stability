
# setup -------------------------------------------------------------------

# remove site 4 at usubetsu - no coordinates data

rm(list = ls())
pacman::p_load(tidyverse, foreach)
setwd(here::here("code_empirical"))


# read data ---------------------------------------------------------------

source("data_fmt_fishdata.R")

# plot --------------------------------------------------------------------

d0 %>% 
  ggplot() +
  geom_point(aes(x = year,
                 y = density,
                 color = factor(site))) + 
  geom_line(aes(x = year,
                y = density,
                color = factor(site))) + 
  facet_wrap(facets = ~ river,
             ncol = 5) +
  theme_bw()
