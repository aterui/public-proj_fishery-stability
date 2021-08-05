
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

df_sp <- d0 %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon)) %>% 
  ungroup() %>% 
  mutate(scl_n_species = c(scale(n_species)))

df_ssm <- read_csv("data_fmt/data_ssm_est.csv") %>% 
  filter(param_name %in% c("cv", "mu", "sigma")) %>% 
  rename(median = '50%',
         high = '97.5%',
         low = '2.5%') %>% 
  select(river,
         site,
         site_id,
         param_name,
         median,
         high,
         low)

df_env <- read_csv("data_fmt/data_env_fmt.csv") %>% 
  rename(wsd_area = area) %>% 
  mutate(scl_wsd_area = c(scale(wsd_area)),
         scl_ppt = c(scale(ppt)),
         scl_temp = c(scale(temp)),
         scl_forest = c(scale(frac_forest)))

df_ocean <- read_csv("data_fmt/data_ocean_fmt.csv") %>% 
  mutate(scl_chr_a = c(scale(chr_a)),
         scl_sst = c(scale(sst)))

df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(river) %>% 
  summarize(mean_stock = sum(abundance) / (2019 - 1999 + 1)) %>% 
  ungroup() %>% 
  mutate(scl_mean_stock = c(scale(mean_stock)))

df_m <- df_ssm %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_sp, by = c("river", "site", "site_id")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         scl_mean_stock = ifelse(is.na(scl_mean_stock), 0, scl_mean_stock))


# plot --------------------------------------------------------------------

g1 <- df_m %>% 
  group_by(river, param_name) %>% 
  summarize(median = mean(median),
            mean_stock = unique(mean_stock)) %>% 
  ggplot() +
  geom_point(aes(x = mean_stock,
                 y = median),
             size = 2) +
  #geom_point(aes(x = mean_stock,
  #               y = median),
  #           data = df_m,
  #           alpha = 0.2,
  #           size = 1) +
  facet_wrap(facets = ~ param_name,
             ncol = 2,
             scales = "free_y") + 
  xlab("Fish stock (thousand fish)") +
  ylab("value") +
  theme_bw()

print(g1)