
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

pacman::p_load(cdyns,
               tidyverse)


# simulation example ------------------------------------------------------

re <- cdynsim(n_species = 3,
              stock = 150,
              int_type = "manual",
              alpha = rbind(c(1.0, 0.1, 0.1),
                            c(0.4, 1.0, 0.1),
                            c(0.3, 0.1, 1.0)))

re$df_dyn %>% 
  filter(timestep < 101) %>% 
  ggplot(aes(x = timestep,
             y = density,
             color = factor(species))) +
  geom_line()
