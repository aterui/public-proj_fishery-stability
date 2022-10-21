
# setup -------------------------------------------------------------------

lapply(paste0("code/", c("library.R", "set_functions.R")),
       FUN = function(x) source(here::here(x)))


# data for tables ---------------------------------------------------------

df_reg <- readRDS(here::here("output/summary_reg.rds"))
group <- unique(df_reg$group) %>% 
  na.omit() %>% 
  c()

r_level <- c("CV",
             "Species richness",
             "Mean $\\mu$",
             "SD $\\sigma$")

list_reg <- lapply(group, FUN = function(x) {
  
  df_reg %>% 
    filter(param_name %in% c("a", "b"),
           group == x) %>% 
    mutate(variable = case_when(id1 == 1 & param_name == "a" ~ "Intercept",
                                id1 == 2 & param_name == "a" ~ "Effective release",
                                id1 == 3 & param_name == "a" ~ "Watershed area",
                                id1 == 4 & param_name == "a" ~ "Air temperature",
                                id1 == 5 & param_name == "a" ~ "Precipitation",
                                id1 == 6 & param_name == "a" ~ "Forest fraction",
                                id1 == 1 & param_name == "b" ~ "Ocean productivity"),
           response = case_when(response == "species_richness" ~ "Species richness",
                                response == "cv" ~ "CV",
                                response == "mu" ~ "Mean $\\mu$",
                                response == "sigma" ~ "SD $\\sigma$"),
           response = factor(response, r_level),
           pr_ne = 1 - pr_po) %>%
    mutate(across(.cols = where(is.numeric),
                  .fns = function(x) paste0("$", op(x), "$"))) %>% 
    arrange(response) %>% 
    transmute(Response = replace(response, duplicated(response), NA),
              Variable = variable,
              Estimate = median,
              SE= sd, 
              "Pr(> 0)" = pr_po, 
              "Pr(< 0)" = pr_ne)
  
})

names(list_reg) <- group