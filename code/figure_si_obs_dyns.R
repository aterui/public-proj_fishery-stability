
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               foreach)

# time series data --------------------------------------------------------

## read data

file_name <- list.files(path = here::here("data_fmt"), full.names = TRUE) %>%
  as_tibble() %>% 
  filter(str_detect(value, pattern = "data_ssm")) %>% 
  pull()


## plot

source(here::here("code/figure_set_theme.R"))
theme_set(plt_theme)

list_g_dyns <- foreach(i = seq_len(length(file_name))) %do% {
  
  df0 <- read_csv(file_name[i]) %>% 
    filter(param_name == "log_d") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(est_density = exp(median),
           river = str_to_sentence(river))
  
  g <- df0 %>% 
    ggplot() + 
    geom_line(aes(x = year_id + 1998,
                  y = est_density,
                  color = factor(site))) +
    geom_point(aes(x = year_id + 1998,
                   y = density,
                   color = factor(site)),
               alpha = 0.5) +
    facet_wrap(facets = ~ river,
               ncol = 5,
               scales = "free_y") +
    ylab(expression("Density (ind."~m^-2*")")) +
    xlab("Year") +
    labs(color = "Site")
  
  return(g)
}

