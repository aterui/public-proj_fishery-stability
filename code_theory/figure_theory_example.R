
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

pacman::p_load(cdyns,
               tidyverse)


# simulation example ------------------------------------------------------

re <- cdynsim(n_species = 2,
              stock = 0,
              r = 1.5,
              k = 100,
              phi = 1,
              int_type = "random",
              alpha = 0.2,
              sd_env = 0)

re$df_dyn %>% 
  filter(timestep < 101) %>% 
  ggplot(aes(x = timestep,
             y = density,
             color = factor(species))) +
  geom_line()
