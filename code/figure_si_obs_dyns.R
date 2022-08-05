
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               foreach)

# time series data --------------------------------------------------------

## read data
df0 <- readRDS(file = here::here("data_fmt/data_ssm_ar.rds")) %>% 
  lapply(function(x) mutate(x, group = c(na.omit(unique(x$group))))) %>% 
  bind_rows() %>% 
  rename(median = '50%',
         high = '97.5%',
         low = '2.5%') %>% 
  filter(str_detect(param_name, "log_d")) %>% 
  mutate(est_density = exp(median),
         river = str_to_sentence(river))

## plot
source(here::here("code/figure_set_theme.R"))
theme_set(plt_theme)

list_g_dyns <- foreach(i = seq_len(length(unique(df0$group)))) %do% {
  
  g <- df0 %>% 
    filter(group == unique(df0$group)[i]) %>% 
    ggplot() + 
    geom_line(aes(x = year_id_numeric + 1998,
                  y = est_density,
                  color = factor(site))) +
    geom_point(aes(x = year_id_numeric + 1998,
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

