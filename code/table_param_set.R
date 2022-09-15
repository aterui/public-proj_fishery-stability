
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# table for simulation parameters -----------------------------------------

load(file = here::here("output/result_ricker.RData"))

df_param <- sim_result %>% 
  dplyr::select(n_species,
                k,
                r1,
                r_max,
                sd_env,
                phi,
                alpha) %>% 
  pivot_longer(cols = everything(),
               names_to = "Parameter",
               values_to = "Main") %>% 
  group_by(Parameter) %>% 
  summarize(Main = list(unique(Main))) %>% 
  ungroup() %>% 
  mutate(
    id = case_when(Parameter == "n_species" ~ "a",
                   Parameter == "r1" ~ "b",
                   Parameter == "r_max" ~ "c",
                   Parameter == "alpha" ~ "d",
                   Parameter == "k" ~ "e",
                   Parameter == "sd_env" ~ "f",
                   Parameter == "phi" ~ "g"),
    Interpretation = case_when(Parameter == "n_species" ~ "Number of species",
                               Parameter == "k" ~ "Carrying capacity",
                               Parameter == "r1" ~ "Intrinsic growth rate of an enhanced species",
                               Parameter == "r_max" ~ "Maximum intrinsic growth rate of unenhanced species",
                               Parameter == "sd_env" ~ "Environmental variability",
                               Parameter == "phi" ~ "Relative fitness of captive-bred individuals",
                               Parameter == "alpha" ~ "Average strength of interspecific competition"),
    Sensitivity = case_when(Parameter == "n_species" ~ "Unif(5, 20)",
                            Parameter == "k" ~ "Unif(100, 1000)",
                            Parameter == "r1" ~ "Unif(0.5, 2.5)",
                            Parameter == "r_max" ~ "Unif(0.5, 2.5)",
                            Parameter == "sd_env" ~ "Unif(0.05, 0.5)",
                            Parameter == "phi" ~ "Unif(0.5, 1)",
                            Parameter == "alpha" ~ "Unif(0.05, 0.5)"),
    Parameter = case_when(Parameter == "n_species" ~ "$S$",
                          Parameter == "k" ~ "$K$",
                          Parameter == "r1" ~ "$r_1$",
                          Parameter == "r_max" ~ "$r_{max}$",
                          Parameter == "sd_env" ~ "$\\sigma_{\\epsilon}$",
                          Parameter == "phi" ~ "$f_R$",
                          Parameter == "alpha" ~ "$\\bar{\\alpha}$")
  ) %>% 
  arrange(id) %>% 
  dplyr::select(-id) %>% 
  relocate(Parameter, Interpretation)
