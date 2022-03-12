
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
op <- function(x, d = 2) sprintf(paste0("%1.", d, "f"), x) 


# data --------------------------------------------------------------------

df_ssm <- list.files(path = here::here("data_fmt"), full.names = T) %>% 
  as_tibble() %>% 
  filter(str_detect(string = .$value, pattern = "ssm")) %>% 
  pull() %>% 
  lapply(function(x) {
    df0 <- read_csv(x) %>% 
      filter(param %in% c("mu_b",
                          "log_global_r",
                          "sd_r_space",
                          "sd_b")) %>% 
      select(param, `50%`, `2.5%`, `97.5%`) %>% 
      mutate(group = str_extract(x, "all|masu|other"),
             param = factor(param, levels = c("log_global_r",
                                              "mu_b",
                                              "sd_r_space",
                                              "sd_b")),
             Parameter = case_when(param == "log_global_r" ~ "$\\mu_{r}$",
                                   param == "mu_b" ~ "$\\mu_{\\beta}$",
                                   param == "sd_r_space" ~ "$\\sigma_{r}$",
                                   param == "sd_b" ~ "$\\sigma_{\\beta}$"),
             Interpretation = case_when(param == "log_global_r" ~ "Rate of community change",
                                        param == "mu_b" ~ "Effect of spring release",
                                        param == "sd_r_space" ~ "SD of the rate of community change",
                                        param == "sd_b" ~ "SD of the effect of spring release"),
             `Species group` = case_when(group == "all" & param == "log_global_r" ~ "Whole",
                                         group == "masu" & param == "log_global_r" ~ "Enhanced",
                                         group == "other" & param == "log_global_r" ~ "Unenhanced")) %>% 
      arrange(param)
    
    if(str_detect(x, "other")) {
      df0 <- df0 %>% 
        filter(!(param %in% c("mu_b", "sd_b")))
    }
    
    df0 <- df0 %>%
      mutate(across(.cols = where(is.numeric),
                    .fns = op)) %>% 
      mutate(Estimate = paste0("$",
                               `50%`,
                               "~(", `2.5%`, ",~", `97.5%`, ")",
                               "$")) %>% 
      select(`Species group`,
             Parameter,
             Interpretation,
             Estimate) %>% 
    
    return(df0)
    
  }) %>% 
  bind_rows()
