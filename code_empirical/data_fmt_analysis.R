
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

df_sp <- d0 %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon),
            n_species_unstock = n_distinct(taxon[.data$taxon != "Oncorhynchus_masou_masou"])) %>% 
  ungroup() %>% 
  mutate(scl_n_species = c(scale(n_species)),
         scl_n_species_unstock = c(scale(n_species_unstock)))

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


# ssm data ----------------------------------------------------------------

id_ssm <- list.files("data_fmt") %>% 
  str_detect("data_ssm")

file_name <- paste0("data_fmt/",
                    list.files("data_fmt")[id_ssm])

list_ssm <- foreach(i = seq_len(length(file_name))) %do% {
  
  df_ssm <- read_csv(file_name[i]) %>% 
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
  
  df_m <- df_ssm %>% 
    left_join(df_env, by = c("river", "site")) %>% 
    left_join(df_sp, by = c("river", "site", "site_id")) %>% 
    left_join(df_stock, by = "river") %>% 
    left_join(df_ocean, by = "river") %>% 
    mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
           scl_mean_stock = ifelse(is.na(scl_mean_stock), 0, scl_mean_stock))
  
  return(df_m)
}

names(list_ssm) <- str_extract(file_name, c("all", "masu", "other"))
