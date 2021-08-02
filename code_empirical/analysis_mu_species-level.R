
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               foreach,
               lme4,
               brms)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

df_env <- read_csv("data_fmt/data_env_fmt.csv") %>% 
  rename(wsd_area = area)

df_ocean <- read_csv("data_fmt/data_ocean_fmt.csv")

df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(river) %>% 
  summarize(mean_stock = sum(abundance) / (2019 - 1999 + 1),
            sd_stock = sqrt(sum((abundance - mean_stock)^2) / (2019 - 1999)))

df_m <- d0 %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         sd_stock = ifelse(is.na(sd_stock), 0, sd_stock))


# data selection ----------------------------------------------------------

selected_taxa <- df_m %>% 
  group_by(taxon) %>% 
  summarize(n_obs = n(),
            n_present = sum(abundance > 0),
            prop_present = n_present / n_obs) %>% 
  filter(prop_present > 0.10) %>% 
  pull(taxon)

df_m <- df_m %>% 
  group_by(site_id, taxon) %>% 
  summarize(river = unique(river),
            site = factor(unique(site)),
            abundance = sum(abundance),
            area = sum(area),
            density = mean(abundance / area),
            mean_stock = unique(mean_stock),
            wsd_area = unique(wsd_area),
            chr_a = unique(chr_a),
            temp = unique(temp),
            ppt = unique(ppt),
            frac_forest = unique(frac_forest)) %>% 
  filter(taxon %in% selected_taxa)


# analysis ----------------------------------------------------------------

list_m <- foreach(i = seq_len(n_distinct(df_m$taxon))) %do% {
  
  river_subset <- df_m %>% 
    filter(taxon == selected_taxa[i]) %>% 
    group_by(river) %>% 
    summarize(n = sum(abundance)) %>% 
    filter(n > 0) %>% 
    pull(river)
  
  re <- df_m %>% 
    filter(river %in% river_subset) %>% 
    lmer(density ~ scale(mean_stock) +
                   scale(chr_a) +
                   scale(wsd_area) + 
                   scale(temp) + 
                   scale(frac_forest) + 
                   scale(ppt) +
                   (1 | river),
         REML = FALSE,
         data = .)
  
  return(re)  
}

names(list_m) <- selected_taxa

# plot --------------------------------------------------------------------

df_m %>% 
  mutate(density = abundance / area) %>% 
  ggplot() +
  geom_point(aes(y = density,
                 x = mean_stock)) +
  facet_wrap(facets = ~taxon,
             scales = "free",
             ncol = 4)
