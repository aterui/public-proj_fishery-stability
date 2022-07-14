
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

site_comp <- df_fish %>% 
  filter(group == "masu_salmon") %>% 
  group_by(site_id) %>% 
  tally() %>% 
  filter(n == 21) %>% 
  pull(site_id)

foreach(i = seq_len(length(site_comp))) %do% {
  
  df_fish %>% 
    filter(group == "all") %>% 
    filter(site_id == site_comp[i]) %>% 
    right_join(tibble(year = 1999:2019),
               by = "year") %>% 
    arrange(year) %>% 
    pull(density) %>% 
    acf()
  
}