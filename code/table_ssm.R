
# setup -------------------------------------------------------------------

rm(list = ls())
lapply(paste0("code/", c("library.R", "set_functions.R")),
       FUN = function(x) source(here::here(x)))


# data --------------------------------------------------------------------

p_level <- c("$\\mu_{\\beta}$",
             "$\\sigma_{\\beta}$",
             "$\\mu_{\\xi1}$",
             "$\\mu_{\\xi2}$",
             "$\\nu_{0}$")

df_ssm <- list.files(path = here::here("output"),
                     full.names = T,
                     pattern = "summary_ssm_ar") %>% 
  readRDS() %>%
  ungroup() %>% 
  filter(param_name %in% c("nu0",
                           "mu_xi",
                           "mu_b",
                           "sd_b")) %>% 
  dplyr::select(param_name,
                param,
                group,
                median = `50%`,
                lower = `2.5%`,
                upper = `97.5%`) %>% 
  mutate(group = replace_na(group, "masu_salmon"),
         group= case_when(group == "masu_salmon" ~ "Enhanced (masu salmon)",
                          group == "other" ~ "Unenhanced"),
         parameter = case_when(param_name == "nu0" ~ "$\\nu_{0}$",
                               str_detect(param, "mu_xi\\[1,.\\]") ~ "$\\mu_{\\xi1}$",
                               str_detect(param, "mu_xi\\[2,.\\]") ~ "$\\mu_{\\xi2}$",
                               param_name == "mu_b" ~ "$\\mu_{\\beta}$",
                               param_name == "sd_b" ~ "$\\sigma_{\\beta}$"),
         parameter = factor(parameter,
                            levels = p_level),
         estimate = paste0(op(median),
                           " [", op(lower),
                           " to ",
                           op(upper), "]")) %>%
  dplyr::select(group, parameter, estimate) %>% 
  arrange(group, parameter) %>% 
  mutate(group = replace(group, duplicated(group), values = NA)) %>% 
  rename_with(.fn = str_to_sentence)
