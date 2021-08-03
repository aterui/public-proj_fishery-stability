
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach,
               glmmTMB)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

df0 <- d0 %>% 
  mutate(status = ifelse(taxon == "Oncorhynchus_masou_masou",
                         yes = "stocked_species",
                         no = "unstocked_species")) %>% 
  group_by(year, river, site, site_id, status) %>% 
  summarize(abundance = sum(abundance),
            area = unique(area))

df_env <- read_csv("data_fmt/data_env_fmt.csv") %>% 
  rename(wsd_area = area) %>% 
  mutate(scl_wsd_area = c(scale(wsd_area)),
         scl_ppt = c(scale(ppt)),
         scl_temp = c(scale(temp)),
         scl_forest = c(scale(frac_forest)))

df_ocean <- read_csv("data_fmt/data_ocean_fmt.csv") %>% 
  mutate(pc1_ocean = prcomp(select(., sst, chr_a), scale. = TRUE)$x[,1],
         scl_pc1_ocean = c(scale(pc1_ocean)),
         scl_chr_a = c(scale(chr_a)),
         scl_sst = c(scale(sst)))

df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(river) %>% 
  summarize(mean_stock = sum(abundance) / (2019 - 1999 + 1)) %>% 
  mutate(scl_mean_stock = c(scale(mean_stock)))

df_m <- df0 %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         scl_mean_stock = ifelse(is.na(scl_mean_stock), 0, scl_mean_stock))


# data selection ----------------------------------------------------------

v_status <- unique(df_m$status)

## taking average for watersheds ####
df_river <- df_m %>% 
  group_by(river, status) %>% 
  summarize(mean_density = mean(abundance / area),
            mean_stock = unique(mean_stock))

## taking average for site ####
df_site <- df_m %>% 
  group_by(site_id, status) %>% 
  summarize(mean_density = mean(abundance / area),
            mean_stock = unique(mean_stock))


# analysis ----------------------------------------------------------------

list_m <- foreach(i = seq_len(n_distinct(df_m$status))) %do% {
  
  re <- df_m %>% 
    filter(status == v_status[i]) %>% 
    glmmTMB(abundance ~ scl_mean_stock +
              scl_chr_a +
              scl_wsd_area + 
              scl_temp + 
              scl_forest + 
              scl_ppt +
              offset(log(area)) + 
              (1 | river) +
              (1 | site:river),
            family = "nbinom2",
            data = .)
  
  return(re)  
}

names(list_m) <- v_status


# plot --------------------------------------------------------------------

df_river %>% 
  ggplot() +
  geom_point(aes(y = mean_density,
                 x = mean_stock)) +
  geom_point(aes(y = mean_density,
                 x = mean_stock),
             alpha = 0.1,
             size = 1,
             data = df_site) +
  facet_wrap(facets = ~status,
             scales = "free",
             ncol = 4) +
  theme_bw()

