
# setup -------------------------------------------------------------------

rm(list = ls(all.names = T))
pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# combine data ------------------------------------------------------------

df1 <- read_csv("data_raw/data_hkd_ocean_1999-2016.csv")
df2 <- read_csv("data_raw/data_hkd_ocean_2017-2020.csv")

## export
bind_rows(df1, df2) %>% 
  write_csv("data_raw/data_hkd_ocean.csv")


# data formatting ---------------------------------------------------------

df0 <- read_csv("data_raw/data_hkd_ocean.csv")
skimr::skim_without_charts(df0)

df0 <- df0 %>% 
  filter(source == "modis",
         buffer == "30km",
         year < 2020) %>% 
  group_by(river, measure) %>% 
  summarize(value_mean = mean(value)) %>% 
  pivot_wider(id_cols = river,
              names_from = measure,
              values_from = value_mean) %>% 
  mutate(river = str_to_lower(river),
         across(.cols = c(chr_a, sst), .fns = function(x) round(x, 2)))

write_csv(df0,
          file = "data_fmt/data_ocean_fmt.csv")