
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)


# data --------------------------------------------------------------------

## call `sim_result`
load(file = here::here("result/result_ricker.RData"))

df0 <- sim_result %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "response",
               values_to = "value") %>% 
  filter(r_max == 2,
         r1 == 1,
         sd_env == 0.5,
         alpha == 0.5,
         phi == 0.8,
         k == 100,
         response %in% c("n_sp_persist", "cv", "mean_density", "sd_density")) %>% 
  filter(!(response == "cv" & status != "all")) %>%
  filter(!(response == "n_sp_persist" & status != "all")) %>%
  mutate(response_name = case_when(response == "n_sp_persist" ~ "Number~of~species~persist",
                                   response == "cv" ~ "CV~sigma/mu",
                                   response == "mean_density" ~ "Mean~mu~(ind.)",
                                   response == "sd_density" ~ "SD~sigma~(ind.)"),
         group_id = case_when(status == "all" ~ "a",
                              status == "enhanced" ~ "b",
                              status == "unenhanced" ~ "c")) %>% 
  mutate(response_name = factor(response_name,
                                levels = c("CV~sigma/mu",
                                           "Number~of~species~persist",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))


# plot --------------------------------------------------------------------

source(here::here("code/figure_set_theme.R"))
theme_set(plt_theme)

g_theory <- df0 %>% 
  ggplot(aes(y = value,
             x = stock,
             color = group_id,
             fill = group_id)) +
  geom_point(data = filter(df0, status == "enhanced"),
             size = 0.25,
             color = "darkseagreen2") +
  geom_point(data = filter(df0, status == "unenhanced"),
             size = 0.25,
             color = "lightskyblue2") +
  geom_point(data = filter(df0, status == "all"),
             size = 0.25,
             color = "pink") +
  geom_smooth(size= 0.1,
              method = "loess") +
  scale_color_hue(labels = c("Whole", "Enhanced", "Unenhanced")) +
  labs(x = "Number of releases (individuals)",
       y = "Value",
       color = "Species group") +
  facet_wrap(facets = ~ response_name,
             nrow = 4,
             scales = "free_y",
             labeller = label_parsed) +
  guides(color = "none",
         fill = "none") +
  theme(axis.title.y = element_blank())
