
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_analysis.R"))

df_env %>% 
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
                               variable == "ppt" ~ "Cummulative~precipitation~(mm)",
                               variable == "temp" ~ "Annual~mean~temperature~(degreeC)",
                               variable == "wsd_area" ~ "Watershed~area~(km^2)"),
         var_label = factor(var_label, levels = c("Watershed~area~(km^2)",
                                                  "Annual~mean~temperature~(degreeC)",
                                                  "Cummulative~precipitation~(mm)",
                                                  "Fraction~forest",
                                                  "Fraction~agriculture",
                                                  "Fraction~urban"))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(facets = ~var_label,
             scales = "free",
             labeller = label_parsed)
