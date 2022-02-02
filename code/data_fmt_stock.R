
# setup -------------------------------------------------------------------

#rm(list = ls())
pacman::p_load(tidyverse)


# format data -------------------------------------------------------------

## base data ####
## df_site_id - 97 sites x 21 years = 2037 combo
## df_year_river - 31 rivers x 21 years = 651 combo
## df_stock - raw stock data

source(here::here("code/data_fmt_fishdata.R"))
river_id <- pull(distinct(df_fish, river))

df_site_id <- df_fish %>% 
  distinct(river, site_id, site_id_numeric) %>%
  left_join(tibble(year_release = rep(1999:2019,
                                      length(river_id)),
                   river = rep(river_id,
                               each = length(1999:2019))),
            by = "river")

df_year_river <- tibble(year_release = rep(1999:2019,
                                           length(river_id)),
                        river = rep(river_id,
                                    each = length(1999:2019)))

df_stock <- read_csv(here::here("data_fmt/data_hkd_prtwsd_stock_fmt.csv")) %>% 
  filter(between(year_release, 1999, 2019),
         river %in% river_id)
  

## df_fry ####
## fry data only - for state-space model
## 97 sites x 21 years = 2037 combo

df_fry <- df_stock %>% 
  filter(release_stage == "fry") %>% 
  right_join(df_site_id, by = c("river", "year_release")) %>% 
  mutate(abundance = replace_na(abundance, 0),
         stock = abundance * 0.001,
         abundance_unit = "thousand_fish",
         stock_unit = "million_fish",
         release_stage = replace_na(release_stage, "fry"))

#skimr::skim_without_charts(df_fry)

## mean stock ####
## mean stock for regression

df_stock_mu <- df_stock %>% 
  group_by(year_release, river) %>% 
  summarize(abundance = sum(abundance),
            unit = unique(abundance_unit)) %>% 
  right_join(df_year_river, by = c("year_release", "river")) %>% 
  mutate(abundance = replace_na(abundance, 0),
         unit = replace_na(unit, "thousand_fish")) %>% 
  group_by(river) %>% 
  summarize(mean_stock = mean(abundance)) %>% 
  ungroup() %>% 
  mutate(mean_stock = mean_stock * 0.001, # unit conversion to million
         scl_mean_stock = c(scale(mean_stock)),
         stock_unit = "million_fish")

#skimr::skim_without_charts(df_stock_mu)
