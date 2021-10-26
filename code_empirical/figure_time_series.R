
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               foreach)
setwd(here::here("code_empirical"))


# read data ---------------------------------------------------------------

x <- list.files(path = "data_fmt") %>% 
  str_detect(pattern = "data_ssm")

file_name <- paste0("data_fmt/",
                    list.files(path = "data_fmt")[x])
figure_name <- paste0("figure/figure_timeseries",
                      str_remove(list.files(path = "data_fmt")[x],
                                 "data_ssm_est")) %>% 
  str_replace(".csv", ".pdf")


# plot --------------------------------------------------------------------

source("figure_set_theme.R")
theme_set(plt_theme)

foreach(i = seq_len(length(file_name))) %do% {
  
  d0 <- read_csv(file_name[i]) %>% 
    filter(param_name == "log_d") %>%
    rename(median = "50%",
           lower = "2.5%",
           upper = "97.5%") %>% 
    mutate(est_density = exp(median),
           river = str_to_sentence(river))
  
  g1 <- d0 %>% 
    ggplot() + 
    geom_line(aes(x = year_id,
                  y = est_density,
                  color = factor(site))) +
    geom_point(aes(x = year_id,
                   y = density,
                   color = factor(site)),
               alpha = 0.5) +
    facet_wrap(facets = ~ river,
               ncol = 6,
               scales = "free_y") +
    ylab("Community-wide density (ind/sq-m)") +
    xlab("Year since 1999") +
    labs(color = "Site")
  
  ggsave(figure_name[i],
         width = 14,
         height = 9)
  
}
