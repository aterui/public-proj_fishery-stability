
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)
setwd(here::here("code_theory"))
source("theme_ggplot.R")

# data --------------------------------------------------------------------

df0 <- read_csv("result/result_ricker.csv") %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "param",
               values_to = "value") %>% 
  filter(r_max == 2,
         r_min == 0.5,
         sd_env == 0.1,
         param %in% c("cv", "mean_density", "sd_density"),
         n_species == 10) %>% 
  mutate(param_name = case_when(param == "cv" ~ "CV",
                                param == "mean_density" ~ "Mean~(mu)",
                                param == "sd_density" ~ "SD~(sigma)"),
         status_name = case_when(status == "all" ~ "All~species",
                                 status == "stocked" ~ "Enhanced~species",
                                 status == "unstocked" ~ "Unenhanced~species~(summed)"))


# plot --------------------------------------------------------------------

theme_set(plt_theme)

# outliers
df_outlier <- df0 %>% 
  filter(status == "all",
         param == "cv",
         value > 0.15)

# cv plot ####
g1 <- df0 %>% 
  filter(status == "all",
         param == "cv") %>% 
  ggplot(aes(y = value,
             x = stock,
             color = factor(alpha),
             fill = factor(alpha))) +
  geom_smooth(size= 0.1,
              method = "loess") +
  geom_point(size = 0.5,
             alpha = 0.2) +
  labs(x = "No. of release (individuals)",
       y = "Community CV") +
  scale_color_hue(name = expression(mu[alpha])) +
  coord_cartesian(ylim = c(NA, 0.15)) +
  geom_point(data = df_outlier,
             aes(x = stock,
                 y = 0.15),
             shape = 24,
             size = 0.5,
             show.legend = FALSE) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none")

ggsave("figure/figure01_theory.pdf",
       width = 4,
       height = 3)

# cv plot ####
g2 <- df0 %>% 
  filter(param_name != "CV") %>% 
  ggplot(aes(y = value,
             x = stock,
             color = factor(alpha),
             fill = factor(alpha))) +
  geom_point(size = 0.5,
             alpha = 0.2) +
  geom_smooth(size = 0.1,
              method = "loess") +
  facet_grid(rows = vars(param_name),
             cols = vars(status_name),
             scales = "free_y",
             labeller = label_parsed) +
  labs(color = expression(alpha),
       fill = expression(alpha)) +
  scale_color_hue(name = expression(mu[alpha])) +
  labs(x = "No. of release (individuals)",
       y = "Value") +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none")


ggsave("figure/figure02_theory.pdf",
       width = 9,
       height = 5)
