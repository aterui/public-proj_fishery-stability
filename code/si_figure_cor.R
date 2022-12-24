
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

source(here::here("code/data_fmt_reg.R"))


# plot --------------------------------------------------------------------

M <- df_env %>%
  left_join(df_stock_mu, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  left_join(df_n_obs, by = "river") %>% 
  dplyr::select('Forest fraction' = frac_forest,
                'Agriculture fraction' = frac_agri,
                'Urban fraction' = frac_urban,
                'Temperature' = temp,
                'Precipitation' = ppt,
                'Watershed area' = wsd_area,
                'Number of observation years' = n_obs,
                'Number of fish released' = mean_stock,
                'Chlorophyll a' = chr_a,
                'SD elevation' = sd_elev) %>% 
  cor(method = "spearman")

corrplot(M,
         type="upper",
         method = "circle",
         addCoef.col = grey(0.1, alpha = 0.8),
         diag = FALSE,
         tl.col = grey(0.1))  
