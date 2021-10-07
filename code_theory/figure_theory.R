
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)
setwd(here::here("code_theory"))
source("theme_ggplot.R")


# data --------------------------------------------------------------------

df0 <- read_csv("result/result_ricker.csv") %>% 
  mutate(cv = sd_density / mean_density) %>% 
  group_by(r_min,
           r_max,
           alpha_max,
           n_species) %>% 
  mutate(scaled_mean = c(scale(mean_density)),
         scaled_sd = c(scale(sd_density))) %>% 
  ungroup() %>% 
  filter(r_max >= 1,
         r_min == 0) %>% 
  pivot_longer(cols = mean_density:scaled_sd,
               names_to = "param",
               values_to = "value") %>% 
  mutate(growth = recode(r_max,
                         `1` = sprintf('"Slow growth"~(r[max]=="%.2f")',
                                       r_max),
                         `2` = sprintf('"Rapid growth"~(r[max]=="%.2f")',
                                       r_max)),
         competition = recode(alpha_max,
                             `0.2` = sprintf('"Weak competition"~(alpha[max]=="%.2f")',
                                              alpha_max),
                              `0.8` = sprintf('"Strong competition"~(alpha[max]=="%.2f")',
                                              alpha_max)),
         param_name = case_when(param == "cv" ~ "CV",
                                param == "mean_density" ~ "Mean",
                                param == "sd_density" ~ "SD",
                                param == "scaled_mean" ~ "Mean[scale]",
                                param == "scaled_sd" ~ "SD[scale]"),
         status_name = case_when(status == "stocked" ~ "Enhanced",
                                 status == "unstocked" ~ "Unenhanced"))



# plot --------------------------------------------------------------------

theme_set(plt_theme)

# cv plot ####
g1 <- df0 %>% 
  filter(status == "all",
         param %in% c("cv", "mean_density", "sd_density"),
         n_species == 10,
         r_max == 2) %>% 
  ggplot() +
  #geom_point(aes(y = value,
  #               x = stock,
  #               color = factor(alpha_max)),
  #           alpha = 0.1,
  #           size = 1) +
  geom_smooth(aes(y = value,
                  x = stock,
                  color = factor(alpha_max),
                  fill = factor(alpha_max)),
              method = "loess") +
  facet_wrap(facets = ~ param_name,
             scales = "free_y",
             labeller = label_parsed) +
  labs(color = expression(alpha_max),
       fill = expression(alpha_max)) +
  scale_color_hue(name = expression(alpha[max])) +
  xlab("No. of release (individuals)") +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = FALSE)

ggsave("figure/figure01_theory.pdf",
       width = 10,
       height = 3)

# cv plot ####
g2 <- df0 %>% 
  filter(status != "all",
         param == "mean_density",
         n_species == 10,
         r_max == 2) %>% 
  ggplot() +
  #geom_point(aes(y = value,
  #               x = stock,
  #               color = factor(alpha_max)),
  #           alpha = 0.1,
  #           size = 1) +
  geom_smooth(aes(y = value,
                  x = stock,
                  color = factor(alpha_max),
                  fill = factor(alpha_max)),
              method = "gam") +
  facet_wrap(facets = ~ status_name,
             scales = "free_y",
             labeller = label_parsed) +
  labs(color = expression(alpha_max),
       fill = expression(alpha_max)) +
  scale_color_hue(name = expression(alpha[max])) +
  labs(x = "No. of release (individuals)",
       y = "Mean") +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = FALSE)


ggsave("figure/figure02_theory.pdf",
       width = 7,
       height = 3)
