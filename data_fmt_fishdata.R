
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, foreach)
  d0 <- read_csv(here::here("data_fmt/data_hkd_prtwsd_fmt.csv")) %>% 
    mutate(LatinName = case_when(genus == "Cottus" ~ "Cottus_spp",
                                 genus == "Pungitius" ~ "Pungitius_spp",
                                 genus == "Tribolodon" ~ "Tribolodon_spp",
                                 genus == "Gymnogobius" ~ "Gymnogobius_spp",
                                 genus == "Lethenteron" ~ "Lethenteron_spp",
                                 genus == "Rhinogobius" ~ "Rhinogobius_spp",
                                 TRUE ~ as.character(LatinName))
           ) %>% 
    group_by(year, river, site, LatinName) %>% 
    summarize(abundance = sum(abundance),
              area = unique(area),
              genus = unique(genus),
              LatinName = unique(LatinName))

# select by number of observations ----------------------------------------

  obs_threshold <- 8
  
  river_selected <- d0 %>% 
    group_by(river) %>% 
    summarize(n_obs = n_distinct(year)) %>% 
    filter(n_obs > obs_threshold) %>% 
    pull(river)
    
  d0 <- d0 %>% 
    filter(river %in% river_selected)
  
  
# select by occurrence frequency ------------------------------------------

  freq_threshold <- 3
  
  ## occurrence frequency table
  df_freq <- d0 %>% 
    group_by(year, river, LatinName) %>% 
    summarize(summed_n = sum(abundance)) %>% 
    filter(summed_n > 0) %>% 
    group_by(river, LatinName) %>% 
    summarize(freq = n())
  
  ## species occurring  
  species <- df_freq %>% 
    filter(freq > freq_threshold) %>% 
    ungroup() %>% 
    distinct(LatinName) %>% 
    arrange(LatinName) %>% 
    pull()
  

# data list ---------------------------------------------------------------
  
  dat_list <- foreach(i = seq_len(length(species))) %do% {
    
    river_chosen <- df_freq %>% 
      filter(freq > freq_threshold & LatinName == species[i]) %>% 
      pull(river)
    
    print(species[i])
    
    dat <- d0 %>% 
      filter(river %in% river_chosen & LatinName == species[i]) %>% 
      mutate(n_river = length(river_chosen))
    
    return(dat)
  }
    