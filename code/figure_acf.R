
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

site_comp <- df_fish %>% 
  filter(group == "masu_salmon") %>% 
  group_by(site_id) %>% 
  tally() %>% 
  filter(n == 21) %>% 
  pull(site_id)

df_acf <- foreach(i = seq_len(length(site_comp)),
                    .combine = bind_rows) %do% {
  
  x <- df_fish %>% 
    filter(group == "masu_salmon") %>% 
    filter(site_id == site_comp[i]) %>% 
    right_join(tibble(year = 1999:2019),
               by = "year") %>% 
    arrange(year) %>% 
    pull(density) %>% 
    acf(plot = FALSE,
        lag.max = 5)
  
  df0 <- tibble(site_id = site_comp[i],
                y = c(x$acf),
                lag = c(x$lag))  
  
  return(df0)
}

df_acf %>% 
  ggplot(aes(x = lag,
             y = y)) +
  geom_point() +
  geom_segment(aes(xend = lag, yend = 0)) +
  #geom_line() +
  geom_hline(yintercept = 0) + 
  facet_wrap(facets = ~site_id)
