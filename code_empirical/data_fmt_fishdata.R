
# setup -------------------------------------------------------------------

#rm(list = ls())
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
         site_id = paste0(river, site)) %>% 
  group_by(year, river, site, site_id, taxon) %>% 
  summarize(abundance = sum(abundance, na.rm = T),
            area = unique(area),
            genus = unique(genus),
            taxon = unique(taxon)) %>% 
  ungroup()

# select by number of observations ----------------------------------------

## selection criteria
## 6 years of observations
## at least 10 years of range
## at least 2 observations of masu salmon

n_obs_threshold <- 5
n_obs_masu_threshold <- 1
range_threshold <- 10

site_selected <- d0 %>% 
  group_by(site_id) %>% 
  summarize(range_obs = max(year) - min(year) + 1,
            n_obs = n_distinct(year),
            n_obs_masu = sum(abundance[taxon == "Oncorhynchus_masou_masou"] > 0)) %>% 
  filter(range_obs > range_threshold &
         n_obs > n_obs_threshold &
         n_obs_masu > n_obs_masu_threshold) %>% 
  pull(site_id)

d0 <- d0 %>% 
  filter(site_id %in% site_selected)


# summarize data to community level ---------------------------------------

df_group <- d0 %>% 
  mutate(group = case_when(taxon == "Oncorhynchus_masou_masou" ~ "masu_salmon",
                           taxon != "Oncorhynchus_masou_masou" ~ "other")) %>% 
  group_by(year,
           river,
           site,
           site_id,
           group) %>% 
  summarize(abundance = sum(abundance, na.rm = TRUE),
            area = unique(area),
            density = abundance / area) %>% 
  ungroup() %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id)))
       
df_fish <- d0 %>% 
  group_by(year,
           river,
           site,
           site_id) %>% 
  summarize(abundance = sum(abundance, na.rm = TRUE),
            area = unique(area),
            density = abundance / area) %>% 
  ungroup() %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id)),
         group = "all") %>% 
  bind_rows(df_group)

df_year <- d0 %>% 
  group_by(site_id) %>% 
  summarize(St_year = min(year) - min(.$year) + 1,
            End_year = max(year) - min(.$year) + 1) %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id))) %>% 
  relocate(site_id_numeric)
