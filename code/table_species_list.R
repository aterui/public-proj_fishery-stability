
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))
df_trait <- readRDS(here::here("data_fmt/data_trait.rds"))

df_sp_list <- df_selected %>% 
  filter(abundance > 0) %>% 
  distinct(taxon) %>% 
  left_join(df_trait, by = "taxon") %>% 
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
  mutate(across(.fns = str_replace_all,
                pattern = "_",
                replace = " ")) %>% 
  rename_with(.fn = function(x) str_to_sentence(x) %>%
                str_replace_all(pattern = "_",
                                replace = " ")) %>% 
  rename("Current preference" = Current,
         "Max total length (mm)" = "Max total length")

  
