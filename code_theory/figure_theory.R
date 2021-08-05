
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)
setwd(here::here("code_theory"))


# data --------------------------------------------------------------------

df0 <- read_csv("result/result_ricker.csv") %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "param",
               values_to = "value")


# plot --------------------------------------------------------------------

g1 <- df0 %>% 
  ggplot() +
  geom_point(aes(y = value,
                 x = stock,
                 color = factor(alpha_max)),
             alpha = 0.1,
             size = 1) +
  geom_smooth(aes(y = value,
                  x = stock,
                  color = factor(alpha_max),
                  fill = factor(alpha_max))) +
  facet_wrap(facets = ~ param,
             ncol = 2,
             scales = "free_y") +
  labs(color = expression(alpha[max]),
       fill = expression(alpha[max])) +
  xlab("Fish stocking (individuals)") +
  theme_bw()

print(g1)