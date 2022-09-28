
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# a -----------------------------------------------------------------------

tbl_readme <- tibble(name = list.files(here::here("code")),
                     dir = "code") %>% 
  filter(name != "readme.R") %>% 
  mutate(class = case_when(str_detect(name, "analysis_") ~ "analysis",
                           str_detect(name, "^figure_|^si_figure_") ~ "figure",
                           str_detect(name, "data_fmt_") ~ "format",
                           str_detect(name, "gis_") ~ "gis",
                           str_detect(name, "model_|run_jags") ~ "jags",
                           str_detect(name, "run_simulation") ~ "simulation",
                           str_detect(name, "table_") ~ "table",
                           str_detect(name, "set_") ~ "set")
  ) %>% 
  bind_rows(tibble(name = list.files(here::here("data_fmt")),
                   dir = "data_fmt")) %>%
  # bind_rows(tibble(name = list.files(here::here("data_raw")),
  #                  dir = "data_raw")) %>% 
  relocate(dir, class) %>% 
  arrange(dir, class, name) %>% 
  mutate(description = case_when(str_detect(class, "analysis") ~ "correlation for sensitivity analysis of theoretical model",
                                 str_detect(class, "figure") & str_detect(name, "^figure_obs") ~ "figure codes for map (`_map`), coefficients (`_coef`), or stock effect (`_stock`)",
                                 str_detect(class, "figure") & str_detect(name, "theory") ~ "theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)",
                                 str_detect(class, "figure") & str_detect(name, "si_") ~ paste("supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`)"),
                                 str_detect(class, "figure") & str_detect(name, "layout") ~ "layout empirical and theoretical patterns",
                                 str_detect(class, "format") ~ "format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)",
                                 str_detect(class, "gis") ~ ifelse(str_detect(name, "watershed"),
                                                                   "delineate watersheds",
                                                                   paste("extract layer data for", str_extract(name, "environment|ocean"))
                                 ),
                                 str_detect(class, "jags") & str_detect(name, "model") ~ "jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)",
                                 str_detect(class, "jags") & str_detect(name, "run") ~ "run corresponding jags models",
                                 str_detect(class, "simulation") ~ "run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)",
                                 str_detect(class, "table") ~ "tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)",
                                 str_detect(class, "set") ~ "set defaults")
  )

n_dir <- tbl_readme %>% 
  group_by(dir) %>% 
  tally() %>% 
  pull(n)
