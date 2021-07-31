
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, foreach)
setwd(here::here("code_empirical"))


# read data ---------------------------------------------------------------

d0 <- read_csv("data_fmt/data_hkd_prtwsd_fmt.csv") %>% 
  mutate(taxon = case_when(genus == "Cottus" ~ "Cottus_spp",
                           genus == "Pungitius" ~ "Pungitius_spp",
                           genus == "Tribolodon" ~ "Tribolodon_spp",
                           genus == "Gymnogobius" ~ "Gymnogobius_spp",
                           genus == "Lethenteron" ~ "Lethenteron_spp",
                           genus == "Rhinogobius" ~ "Rhinogobius_spp",
                           TRUE ~ as.character(latin)),
         site_id = paste0(river, site)
  ) %>% 
  group_by(year, river, site, site_id, taxon) %>% 
  summarize(abundance = sum(abundance, na.rm = T),
            area = unique(area),
            genus = unique(genus),
            taxon = unique(taxon)) %>% 
  ungroup() %>% 
  filter(!(site_id %in% c("usubetsu4",
                          "atsuta6",
                          "kokamotsu4")))


# select by number of observations ----------------------------------------

n_obs_threshold <- 5
range_threshold <- 10

site_selected <- d0 %>% 
  group_by(site_id) %>% 
  summarize(range_obs = max(year) - min(year) + 1,
            n_obs = n_distinct(year)) %>% 
  filter(range_obs > range_threshold & n_obs > n_obs_threshold) %>% 
  pull(site_id)

d0 <- d0 %>% 
  filter(site_id %in% site_selected)


# summarize data to community level ---------------------------------------

df_fish <- d0 %>% 
  group_by(year,
           river,
           site,
           site_id) %>% 
  summarize(abundance = sum(abundance, na.rm = TRUE),
            area = unique(area),
            density = abundance / area) %>% 
  ungroup() %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id)))

df_year <- d0 %>% 
  group_by(site_id) %>% 
  summarize(St_year = min(year) - min(.$year) + 1,
            End_year = max(year) - min(.$year) + 1) %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id))) %>% 
  relocate(site_id_numeric)
