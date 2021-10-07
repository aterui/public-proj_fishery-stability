
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(here,
               tidyverse)
setwd(here::here("code_theory"))
source("function_sim.R")
source("theme_ggplot.R")


# run simulation ----------------------------------------------------------

dyn0 <- dynsim(stock = 0)
dyn100 <- dynsim(stock = 100)

dyn <- bind_rows(mutate(dyn0$df_dyn, stock = "Release = 0"),
          mutate(dyn100$df_dyn, stock = "Release = 100")) %>% 
  filter(timestep < 101)

# figure ------------------------------------------------------------------

theme_set(plt_theme)

dyn %>% 
  filter(timestep < 101) %>% 
  ggplot() +
  geom_line(aes(x = timestep,
                y = density,
                color = factor(species))) +
  facet_wrap(facets = ~ stock,
             ncol = 2) + 
  labs(y = "Density",
       x = "Time") +
  scale_color_discrete(name = "Species")

ggsave("figure/sample_dynamics.pdf",
       width = 6,
       height = 3)