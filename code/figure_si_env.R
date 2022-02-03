
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_analysis.R"))
source(here::here("code/figure_set_theme.R"))


# plot --------------------------------------------------------------------

theme_set(plt_theme)

df_site <- df_env %>% 
  pivot_longer(cols = c(wsd_area,
                        temp,
                        ppt,
                        frac_agri,
                        frac_forest,
                        frac_urban),
               names_to = "variable",
               values_to = "value") %>% 
  dplyr::select(-starts_with("scl")) %>% 
  mutate(var_label = case_when(variable == "frac_agri" ~ "Fraction~agriculture",
                               variable == "frac_forest" ~ "Fraction~forest",
                               variable == "frac_urban" ~ "Fraction~urban",
                               variable == "ppt" ~ "Precipitation~(mm)",
                               variable == "temp" ~ "Temperature~(degree*C)",
                               variable == "wsd_area" ~ "Watershed~area~(km^2)"))

df_river <- df_stock_mu %>% 
  left_join(df_ocean, by = "river") %>% 
  pivot_longer(cols = c(mean_stock,
                        chr_a),
               names_to = "variable",
               values_to = "value") %>% 
  dplyr::select(-c(starts_with("scl"), "sst")) %>% 
  mutate(var_label = case_when(variable == "mean_stock" ~ "Number~of~releases~(million~fish)",
                               variable == "chr_a" ~ "Chlorophyll~a~(mg~m^-3)"))

df0 <- bind_rows(df_site, df_river) %>% 
  mutate(var_label = factor(var_label, levels = c("Watershed~area~(km^2)",
                                                  "Temperature~(degree*C)",
                                                  "Precipitation~(mm)",
                                                  "Fraction~forest",
                                                  "Fraction~agriculture",
                                                  "Fraction~urban",
                                                  "Number~of~releases~(million~fish)",
                                                  "Chlorophyll~a~(mg~m^-3)")))

g_env <- df0 %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(facets = ~var_label,
             scales = "free",
             labeller = label_parsed)
