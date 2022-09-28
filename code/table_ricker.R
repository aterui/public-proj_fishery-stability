
# setup -------------------------------------------------------------------

lapply(paste0("code/", c("library.R", "set_functions.R")),
       FUN = function(x) source(here::here(x)))


# data --------------------------------------------------------------------

df_ricker <- list.files(path = here::here("output"),
                        full.names = T,
                        pattern = "summary_multi_ricker") %>% 
  readRDS() %>%
  filter(param_name %in% c("p0")) %>% 
  arrange(param) %>% 
  transmute(Parameter = case_when(param == "p0[1]" ~ "$p_{\\alpha}^{\\text{intra}}$",
                                  param == "p0[2]" ~ "$p_{\\alpha}^{\\text{inter}}$"),
            Site = str_to_sentence(site),
            Estimate = paste0("$", op(median), "$"),
            "95% CI" = paste0("$", 
                              op(lower), "~\\text{to}~", op(upper),
                              "$")) %>% 
  mutate(Parameter = replace(Parameter, duplicated(Parameter), NA))
  
