
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = T))
  pacman::p_load(tidyverse)
  

# data formatting ---------------------------------------------------------

  d0 <- read_csv("data_raw/data_hkd_ocean.csv")  
  skimr::skim_without_charts(d0)
  
  dat <- d0 %>% 
    filter(source == "modis" & buffer == "30km") %>% 
    group_by(river, measure) %>% 
    summarize(value_mean = mean(value)) %>% 
    pivot_wider(id_cols = river,
                names_from = measure,
                values_from = value_mean) %>% 
    mutate(river = str_to_lower(river))
  
  write_csv(dat,
            file = "data_fmt/data_ocean_fmt.csv")