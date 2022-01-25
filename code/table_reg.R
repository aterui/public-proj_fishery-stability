
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
op <- function(x, d = 2) sprintf(paste0("%1.", d, "f"), x) 


# data for tables ---------------------------------------------------------

filename <- list.files(path = here::here("result"),
                       full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(string = .$value, pattern = "reg")) %>% 
  filter(!str_detect(string = .$value, pattern = "reg_rich")) %>% 
  pull()

name <- str_extract(filename, pattern = "all|masu|other")

df_est <- lapply(filename, function(x) {
  
  if (str_detect(x, pattern = "masu|other")) {
    df0 <- read_csv(x) %>% 
      filter(response != "cv")
  } else {
    df0 <- read_csv(x)
  }
  
  df_m <- df0 %>% 
    filter(!(str_detect(string = .$parameter,
                        pattern = "sigma|b_raw"))) %>% 
    dplyr::select(response,
                  parameter,
                  median,
                  sd,
                  prob_positive) %>% 
    mutate(prob_negative = 1 - prob_positive,
           Parameter = case_when(parameter == "a[1]" ~ "$\\gamma_1$",
                                 parameter == "a[2]" ~ "$\\gamma_2$",
                                 parameter == "a[3]" ~ "$\\gamma_3$",
                                 parameter == "a[4]" ~ "$\\gamma_4$",
                                 parameter == "b[1]" ~ "$\\delta_0$",
                                 parameter == "b[2]" ~ "$\\delta_1$",
                                 parameter == "b[3]" ~ "$\\delta_2$"),
           Effect = case_when(parameter == "a[1]" ~ "Watershed area",
                              parameter == "a[2]" ~ "Air temperature",
                              parameter == "a[3]" ~ "Precipitation",
                              parameter == "a[4]" ~ "Forest fraction",
                              parameter == "a[5]" ~ "Forest fraction",
                              parameter == "b[1]" ~ "Intercept",
                              parameter == "b[2]" ~ "Stock enhancement",
                              parameter == "b[3]" ~ "Ocean productivity"),
           Response = ifelse(parameter == "b[1]", response, NA),
           Response = case_when(Response == "richness" ~ "Species richness",
                                Response == "cv" ~ "CV",
                                Response == "mu" ~ "Mean $\\mu$",
                                Response == "sigma" ~ "SD $\\sigma$")) %>% 
    relocate(Response,
             Parameter,
             Effect) %>% 
    dplyr::select(-parameter, -response) %>% 
    mutate(across(median:prob_negative, op)) %>% 
    rename(Estimate = median,
           SE = sd,
           "Pr(> 0)" = prob_positive,
           "Pr(< 0)" = prob_negative)
  
  return(df_m)
})

names(df_est) <- name

