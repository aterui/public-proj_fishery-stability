
# setup -------------------------------------------------------------------

#rm(list = ls())
pacman::p_load(tidyverse, foreach)


# read data ---------------------------------------------------------------

## six genera were aggregated as spp.
## species under these genera were recorded as either species-level or as spp.
## extra-care must be taken for species-level analysis for these species
## remove sites with no coordinates - "usubetsu4", "atsuta6", "kokamotsu4"

d0 <- read_csv(here::here("data_fmt/data_hkd_prtwsd_fmt.csv")) %>% 
  mutate(taxon = case_when(genus == "Cottus" ~ "Cottus_spp",
                           genus == "Gymnogobius" ~ "Gymnogobius_spp",
                           genus == "Lethenteron" ~ "Lethenteron_spp",
                           genus == "Pungitius" ~ "Pungitius_spp",
                           genus == "Rhinogobius" ~ "Rhinogobius_spp",
                           genus == "Tribolodon" ~ "Pseudaspius_spp",
                           # update latin name
                           latin == "Gasterosteus_aculeatus" ~ "Gasterosteus_spp",
                           latin == "Hucho_perryi" ~ "Parahucho_perryi",
                           latin == "Noemacheilus_barbatulus" ~ "Barbatula_oreas",
                           latin == "Phoxinus_lagowskii_steindachneri" ~ "Rhynchocypris_lagowskii_steindachneri",
                           latin == "Phoxinus_percnurus_sachalinensis" ~ "Rhynchocypris_percnura_sachalinensis",
                           latin == "Salvelinus_malma" ~ "Salvelinus_malma_krascheninnikovi",
                           TRUE ~ as.character(latin)),
         site_id = paste0(river, site)) %>% 
  group_by(year, river, site, site_id, taxon) %>% 
  summarize(abundance = sum(abundance, na.rm = T),
            area = unique(area),
            genus = unique(genus),
            taxon = unique(taxon)) %>% 
  ungroup() %>% 
  filter(!(site_id %in% c("usubetsu4", "atsuta6", "kokamotsu4")))

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

df_selected <- d0 %>% 
  filter(site_id %in% site_selected)


# summarize data to community level ---------------------------------------

## summarize data by group - stocked or unstocked
df_group <- df_selected %>% 
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
       
## summarize data as community
## combine with group data
df_fish <- df_selected %>% 
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

## year information for state-space model
df_year <- df_selected %>% 
  group_by(site_id) %>% 
  summarize(St_year = min(year) - min(.$year) + 1,
            End_year = max(year) - min(.$year) + 1) %>% 
  mutate(site_id_numeric = as.numeric(factor(site_id))) %>% 
  relocate(site_id_numeric)
