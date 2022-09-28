
# setup -------------------------------------------------------------------

lapply(paste0("code/", c("library.R", "set_functions.R")),
       FUN = function(x) source(here::here(x)))


# data --------------------------------------------------------------------

p_level <- c("$\\theta_{\\beta}$",
             "$\\sigma_{\\beta}$",
             "$\\theta_{\\xi1}$",
             "$\\theta_{\\xi2}$",
             "$\\tau$")

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
         parameter = case_when(param_name == "nu0" ~ "$\\tau$",
                               str_detect(param, "mu_xi\\[1,.\\]") ~ "$\\theta_{\\xi1}$",
                               str_detect(param, "mu_xi\\[2,.\\]") ~ "$\\theta_{\\xi2}$",
                               param_name == "mu_b" ~ "$\\theta_{\\beta}$",
                               param_name == "sd_b" ~ "$\\sigma_{\\beta}$"),
         parameter = factor(parameter,
                            levels = p_level),
         estimate = paste0("$",
                           op(median),
                           "$"),
         "95% CI" = paste0("$",
                           op(lower), "~\\text{to}~", op(upper),
                           "$")) %>%
  dplyr::select(group, parameter, estimate, "95% CI") %>% 
  arrange(group, parameter) %>% 
  mutate(group = replace(group, duplicated(group), values = NA)) %>% 
  rename_with(.cols = -"95% CI",
              .fn = str_to_sentence)
