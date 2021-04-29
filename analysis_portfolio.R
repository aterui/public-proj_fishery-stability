  
# setup -------------------------------------------------------------------
  
  rm(list = ls())
  pacman::p_load(tidyverse, vegan)
  

# read data ---------------------------------------------------------------
  
  d0 <- read_csv("data_fmt/data_ssm_est.csv") %>% 
    filter(param_id == "log_d_mean") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(abundance = exp(median)*100)
  
  df_rho <- d0 %>% 
    pivot_wider(id_cols = c(river, year_id),
                names_from = species,
                values_from = abundance) %>% 
    group_by(river) %>% 
    do(var1 = data.frame(sum(var(.[,3:ncol(.)]), na.rm = TRUE)),
       var2 = data.frame(sum(apply(.[,3:ncol(.)], 2, sd), na.rm = TRUE)^2)) %>% 
    mutate(var1 = as.numeric(var1),
           var2 = as.numeric(var2),
           phi = var1 / var2)
  
  
# statistics --------------------------------------------------------------

  ## mean-variance
  df_sp <- d0 %>% 
    group_by(river, species) %>%
    do(mean_abundance = mean(.$abundance),
       var_abundance = var(.$abundance),
       var_abundance_resid = var(resid(loess(abundance ~ year_id,
                                             data = .)))) %>% 
    mutate(mean_abundance = as.numeric(mean_abundance),
           var_abundance = as.numeric(var_abundance),
           var_abundance_resid = as.numeric(var_abundance_resid),
           cv = sqrt(var_abundance) / mean_abundance)
  
  ## Relative abundance
  df_ra <- df_sp %>% 
    group_by(river) %>% 
    summarize(summed_abundance = sum(mean_abundance)) %>% 
    right_join(df_sp, by = "river") %>% 
    mutate(relative_abundance = mean_abundance / summed_abundance) %>% 
    select(river, species, relative_abundance)
  
  df_sp <- df_sp %>% 
    left_join(df_ra, by = c("river", "species"))
  
  ## community-wide variance
  df_com <- d0 %>% 
    group_by(river, year_id) %>% 
    summarize(summed_abundance = sum(abundance)) %>% 
    do(mean_abundance = mean(.$summed_abundance),
       var_abundance = var(.$summed_abundance),
       var_abundance_resid = var(resid(loess(summed_abundance ~ year_id,
                                             data = .)))) %>% 
    mutate(mean_abundance = as.numeric(mean_abundance),
           var_abundance = as.numeric(var_abundance),
           var_abundance_resid = as.numeric(var_abundance_resid),
           cv_ag = sqrt(var_abundance) / mean_abundance)


# portfolio ---------------------------------------------------------------
      
  ## predicted variance
  df_pe <- df_sp %>% 
    mutate(w_cv = relative_abundance * cv) %>% 
    group_by(river) %>% 
    summarize(cv_mean = mean(cv),
              cv_w_mean = sum(w_cv)) %>% 
    left_join(df_com, by = "river") %>% 
    mutate(pe = cv_mean / cv_ag,
           pe_w = cv_w_mean / cv_ag)
  
# explanatory variables ---------------------------------------------------
  
  ## diversity index
  df_dvindex <- df_ra %>% 
    group_by(river) %>% 
    summarize(n_species = n_distinct(species),
              simpson = vegan::diversity(relative_abundance, index = "simpson"),
              shannon = vegan::diversity(relative_abundance),
              shannon_max = log(n_species),
              evenness = shannon / shannon_max)
  
  ## fish stocking
  df_stk <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
    filter(between(year_release, 1999, 2016) & river != "ogawa") %>% 
    group_by(river) %>% 
    summarize(total_stock = sum(abundance),
              mean_stock = sum(abundance) / (max(year_release) - min(year_release) + 1),
    )
  
  ## environmental variables
  df_env <- read_csv("data_fmt/data_env_fmt.csv")
  
  ## merge pe
  df_pe <- df_pe %>% 
    left_join(df_stk, by = "river") %>% 
    left_join(df_env, by = "river") %>% 
    left_join(df_dvindex, by = "river") %>% 
    left_join(df_rho, by = "river") %>% 
    mutate(total_stock = ifelse(is.na(total_stock), 0, total_stock),
           mean_stock = ifelse(is.na(mean_stock), 0, mean_stock)) %>% 
    dplyr::select(-id)
    
  df_pe <- df_ra %>% 
    filter(species == "Oncorhynchus_masou_masou") %>% 
    right_join(df_pe, by = "river")