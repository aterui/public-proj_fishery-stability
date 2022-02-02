
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

river_id <- pull(distinct(df_fish, river))

## df for species richness
df_sp <- d0 %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon),
            n_species_unstock = n_distinct(taxon[.data$taxon != "Oncorhynchus_masou_masou"])) %>% 
  ungroup() %>% 
  mutate(scl_n_species = c(scale(n_species)),
         scl_n_species_unstock = c(scale(n_species_unstock)))

## df for environmental covariates
df_env <- read_csv(here::here("data_fmt/data_env_fmt.csv")) %>% 
  rename(wsd_area = area) %>% 
  filter(river %in% river_id) %>% 
  mutate(scl_wsd_area = c(scale(wsd_area)),
         scl_ppt = c(scale(mean.ppt)),
         scl_temp = c(scale(mean.temp)),
         scl_forest = c(scale(frac_forest)))

## df for ocean environments
df_ocean <- read_csv(here::here("data_fmt/data_ocean_fmt.csv")) %>% 
  filter(river %in% river_id) %>% 
  mutate(scl_chr_a = c(scale(chr_a)),
         scl_sst = c(scale(sst)))

## df for stock data
source(here::here("code/data_fmt_stock.R"))

## group name
group <- c("all", "masu", "other")

# ssm data ----------------------------------------------------------------

file_name <- list.files(path = here::here("data_fmt"), full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value, "data_ssm")) %>% 
  pull()

list_ssm <- foreach(i = seq_len(length(file_name))) %do% {
  
  df_ssm <- read_csv(file_name[i]) %>% 
    rename(median = '50%',
           high = '97.5%',
           low = '2.5%') %>% 
    filter(str_detect(param_name, "log_d")) %>% 
    group_by(river, site, site_id) %>% 
    summarize(mu = mean(exp(median)),
              sigma = sd(exp(median)),
              cv = sigma / mu) %>% 
    ungroup()
  
  df_m <- df_ssm %>% 
    left_join(df_env, by = c("river", "site")) %>% 
    left_join(df_sp, by = c("river", "site", "site_id")) %>% 
    left_join(df_stock_mu, by = "river") %>% 
    left_join(df_ocean, by = "river") %>% 
    pivot_longer(cols = c(mu, sigma, cv),
                 names_to = "response",
                 values_to = "value") %>%
    mutate(group = group[i])
  
  return(df_m)
}

names(list_ssm) <- str_extract(file_name, c("all", "masu", "other"))
