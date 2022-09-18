
# setup -------------------------------------------------------------------

#rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

river_id <- pull(distinct(df_fish, river))

## df for species richness
df_sp <- df_selected %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon),
            n_species_unstock = n_distinct(taxon[.data$taxon != "Oncorhynchus_masou_masou"])) %>% 
  ungroup()

## df for environmental covariates
df_env <- readRDS(here::here("data_fmt/data_env_fmt.rds")) %>% 
  mutate(site_id = paste0(river, site),
         wsd_area = as.numeric(area)) %>% 
  filter(site_id %in% site_selected)

## df for ocean environments
df_ocean <- readRDS(here::here("data_fmt/data_ocean_fmt.rds")) %>% 
  filter(river %in% river_id)

## df for stock data
source(here::here("code/data_fmt_stock.R"))


# ssm data ----------------------------------------------------------------

df_ssm <- readRDS(file = here::here("data_fmt/data_joint3.rds")) %>% 
  rename(median = '50%',
         high = '97.5%',
         low = '2.5%') %>% 
  filter(str_detect(param_name, "log_d")) %>% 
  group_by(river, site, site_id, site_id_numeric, group) %>%
  summarize(mu = mean(exp(median)),
            sigma = sd(exp(median)),
            cv = sigma / mu) %>% 
  ungroup() %>% 
  left_join(df_env, by = c("river", "site", "site_id")) %>% 
  left_join(df_sp, by = c("river", "site", "site_id")) %>% 
  left_join(df_stock_mu, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  pivot_longer(cols = c(n_species, mu, sigma, cv),
               names_to = "response",
               values_to = "value")
