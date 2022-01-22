
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)

load(file = here::here("result/result_ricker.RData"))

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
               values_to = "Value") %>% 
  group_by(Parameter) %>% 
  summarize(Value = list(unique(Value))) %>% 
  ungroup() %>% 
  mutate(
    id = case_when(Parameter == "n_species" ~ "a",
                   Parameter == "k" ~ "b",
                   Parameter == "r1" ~ "c",
                   Parameter == "r_max" ~ "d",
                   Parameter == "sd_env" ~ "e",
                   Parameter == "phi" ~ "f",
                   Parameter == "alpha" ~ "g"),
    Interpretation = case_when(Parameter == "n_species" ~ "Number of species",
                               Parameter == "k" ~ "Carrying capacity",
                               Parameter == "r1" ~ "Intrinsic growth rate of an enhanced species",
                               Parameter == "r_max" ~ "Maximum intrinsic growth rate of unenhanced species",
                               Parameter == "sd_env" ~ "Environmental variability",
                               Parameter == "phi" ~ "Relative fitness of captive-bred individuals",
                               Parameter == "alpha" ~ "Average strength of interspecific competition"),
    Parameter = case_when(Parameter == "n_species" ~ "$S$",
                          Parameter == "k" ~ "$K$",
                          Parameter == "r1" ~ "$r_1$",
                          Parameter == "r_max" ~ "$r_{max}$",
                          Parameter == "sd_env" ~ "$\\sigma_{env}$",
                          Parameter == "phi" ~ "$f_R$",
                          Parameter == "alpha" ~ "$\\bar{\\alpha}$")
  ) %>% 
  arrange(id) %>% 
  dplyr::select(-id) %>% 
  relocate(Parameter, Interpretation)

            