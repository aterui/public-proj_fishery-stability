
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")


# plot --------------------------------------------------------------------

## filename
x <- list.files(path = "data_fmt") %>% 
  str_detect(pattern = "data_ssm")

figure_name <- paste0("figure/figure_cv_mu_sd",
                      str_remove(list.files(path = "data_fmt")[x],
                                 "data_ssm_est")) %>% 
  str_replace(".csv", ".pdf")

## plot
g1 <- df_m %>% 
  group_by(river, param_name) %>% 
  summarize(value = mean(value),
            mean_stock = unique(mean_stock)) %>% 
  ggplot(aes(x = mean_stock, y = value)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              color = grey(0.3)) +
  facet_wrap(facets = ~ param_name,
             ncol = 3,
             scales = "free_y") + 
  xlab("Fish stock (thousand fish)") +
  ylab("value") +
  theme_bw()

## export
ggsave(figure_name[i],
       width = 10,
       height = 3)
