
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

## load results of sensitivity analysis 
load("output/result_ricker_for_stvy.RData")

## unique parameter set
df_param <- sim_stvy_result %>% 
  distinct(across(n_species:seed_interval)) %>% 
  mutate(group_id = seq_len(nrow(.)))

## effect size estimate
df_psi <- sim_stvy_result %>% 
  left_join(df_param,
            by = c("r1"),
            suffix = c("", ".y")) %>%
  select(-ends_with(".y")) %>% 
  pivot_wider(names_from = status,
              values_from = c(mean_density,
                              sd_density)) %>% 
  group_by(group_id) %>% 
  summarize(slope_mean_enhanced = cor(mean_density_enhanced, stock, method = "spearman"),
            slope_mean_unenhanced = cor(mean_density_unenhanced, stock, method = "spearman"),
            slope_mean_all = cor(mean_density_all, stock, method = "spearman"),
            slope_sd_enhanced = cor(sd_density_enhanced, stock, method = "spearman"),
            slope_sd_unenhanced = cor(sd_density_unenhanced, stock, method = "spearman"),
            slope_sd_all = cor(sd_density_all, stock, method = "spearman")) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  ungroup()

## combine
df_m <- df_psi %>% 
  left_join(df_param, by = "group_id") %>% 
  dplyr::select(-r_type,
                -r_min,
                -int_type,
                -model,
                -seed,
                -seed_interval) %>% 
  mutate(across(n_species:alpha,
                .fns = function(x) (x - mean(x)) / sd(x))
         ) %>% # standardize parameter values
  pivot_longer(cols = starts_with("slope"),
               names_to = "response",
               values_to = "slope")


# analysis ----------------------------------------------------------------

fit_sense <- df_m %>% 
  group_by(response) %>% 
  do(fit = lm(slope ~ 
                n_species +
                r1 +
                r_max +
                alpha +
                k +
                sd_env +
                phi,
              data = .))

save(fit_sense, file = "output/result_stvy_analysis.RData")
