
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

df_sp_list <- df_selected %>% 
  filter(abundance > 0) %>% 
  group_by(taxon) %>% 
  summarise(n_site = n_distinct(site_id)) %>% 
  mutate(taxon = str_replace_all(taxon,
                                 pattern = "_",
                                 replacement = " "),
         taxon = str_replace(taxon,
                             pattern = "\\sspp",
                             replacement = " spp."),
         taxon = paste0("*", taxon, "*"),
         taxon = str_replace(taxon,
                             pattern = "\\sspp.*",
                             replacement = "* spp.")) %>% 
  rename(Taxon = taxon,
         'Number of sites observed' = n_site)
