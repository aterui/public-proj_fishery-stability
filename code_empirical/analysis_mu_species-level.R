
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach,
               glmmTMB)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

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
  mutate(scl_mean_stock = c(scale(mean_stock)))

df_m <- d0 %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         scl_mean_stock = ifelse(is.na(scl_mean_stock), 0, scl_mean_stock))


# data selection ----------------------------------------------------------

## taxon selection ####
selected_taxa <- df_m %>% 
  group_by(taxon) %>% 
  summarize(n_obs = n(),
            n_present = sum(abundance > 0),
            prop_present = n_present / n_obs) %>% 
  filter(prop_present > 0.10) %>% 
  pull(taxon)

df_m <- df_m %>% 
  filter(taxon %in% selected_taxa) %>% 
  mutate(site = factor(site))

## taking average for watersheds ####
df_river <- df_m %>% 
  group_by(river, taxon) %>% 
  summarize(mean_density = mean(abundance / area),
            mean_stock = unique(mean_stock))

## taking average for site ####
df_site <- df_m %>% 
  group_by(site_id, taxon) %>% 
  summarize(mean_density = mean(abundance / area),
            mean_stock = unique(mean_stock))

## watershed selection ####
#f_subset <- foreach(i = seq_len(n_distinct(df_m$taxon)),
#                     .combine = bind_rows) %do% {
#                       
#                       river_subset <- df_m %>% 
#                         filter(taxon == selected_taxa[i]) %>% 
#                         group_by(river) %>% 
#                         summarize(n = sum(abundance)) %>% 
#                         filter(n > 0) %>% 
#                         pull(river)
#                       
#                       df0 <- df_m %>% 
#                         filter(river %in% river_subset & taxon == selected_taxa[i])
#                       
#                       return(df0)  
#                     }

#df_subset <- df_subset %>% 
#  mutate(density = abundance / area)


# analysis ----------------------------------------------------------------

list_m <- foreach(i = seq_len(n_distinct(df_m$taxon))) %do% {
  
  re <- df_m %>% 
    filter(taxon == selected_taxa[i]) %>% 
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

names(list_m) <- selected_taxa


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
  facet_wrap(facets = ~taxon,
             scales = "free",
             ncol = 4) +
  theme_bw()
  
