
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

## df for species richness
df_sp <- d0 %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon),
            n_species_unstock = n_distinct(taxon[.data$taxon != "Oncorhynchus_masou_masou"])) %>% 
  ungroup()

## df for environmental covariates
df_env <- read_csv("data_fmt/data_env_fmt.csv") %>% 
  rename(wsd_area = area)

## df for ocean environments
df_ocean <- read_csv("data_fmt/data_ocean_fmt.csv")

## df for stock data
df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(year_release, river) %>% 
  summarize(abundance = sum(abundance),
            unit = unique(abundance_unit))

df_ys <- tibble(year_release = rep(1999:2019,
                                   nrow(distinct(df_env, river))),
                river = rep(pull(distinct(df_env, river)),
                            each = length(1999:2019)))

df_stock_mu <- df_stock %>%
  right_join(df_ys, by = c("year_release", "river")) %>% 
  mutate(abundance = replace_na(abundance, 0),
         unit = replace_na(unit, "thousand_fish")) %>% 
  group_by(river) %>% 
  summarize(mean_stock = mean(abundance))


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
    left_join(df_stock_mu, by = "river") %>% 
    left_join(df_ocean, by = "river")
  
  return(df_m)
}

names(list_ssm) <- str_extract(file_name, c("all", "masu", "other"))
