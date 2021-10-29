
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)
setwd(here::here("code_theory"))

# data --------------------------------------------------------------------

df0 <- read_csv("result/result_ricker.csv") %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "param",
               values_to = "value") %>% 
  filter(r_max == 2,
         r_min == 0.5,
         sd_env == 0.5,
         alpha == 0.5,
         param %in% c("cv", "mean_density", "sd_density"),
         n_species == 10) %>% 
  filter(!(param == "cv" & status != "all")) %>%
  mutate(param_name = case_when(param == "cv" ~ "CV~sigma/mu",
                                param == "mean_density" ~ "Mean~mu~(ind.)",
                                param == "sd_density" ~ "SD~sigma~(ind.)"),
         status_name = case_when(status == "all" ~ "All",
                                 status == "stocked" ~ "Enhanced",
                                 status == "unstocked" ~ "Unenhanced"))


# plot --------------------------------------------------------------------

source("figure_set_theme.R")
theme_set(plt_theme)

# cv plot ####
g_theory <- df0 %>% 
  ggplot(aes(y = value,
             x = stock,
             color = factor(status_name),
             fill = factor(status_name))) +
  geom_smooth(size= 0.1,
              method = "loess") +
  geom_point(size = 0.75,
             alpha = 0.125) +
  labs(x = "Number of release (individuals)",
       y = "Value") +
  scale_color_hue(name = "Species group") +
  facet_wrap(facets = ~ param_name,
             nrow = 3,
             scales = "free_y",
             labeller = label_parsed) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none")

#ggsave("figure/figure_theory.pdf",
#       width = 4.5,
#       height = 8)

setwd(here::here())

