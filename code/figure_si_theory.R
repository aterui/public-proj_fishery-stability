
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               foreach)


# theoretical prediction --------------------------------------------------

## call `sim_result`
load(here::here(file = "result/result_ricker.RData"))

df0 <- sim_result %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "response",
               values_to = "value") %>% 
  filter(response %in% c("n_sp_persist", "cv", "mean_density", "sd_density")) %>% 
  filter(!(response == "cv" & status != "all")) %>%
  filter(!(response == "n_sp_persist" & status != "all")) %>%
  mutate(response_name = case_when(response == "n_sp_persist" ~ "Number~of~species~persist",
                                   response == "cv" ~ "CV~sigma/mu",
                                   response == "mean_density" ~ "Mean~mu~(ind.)",
                                   response == "sd_density" ~ "SD~sigma~(ind.)"),
         group_id = case_when(status == "all" ~ "a",
                              status == "enhanced" ~ "b",
                              status == "unenhanced" ~ "c")) %>% 
  mutate(response_name = factor(response_name,
                                levels = c("Number~of~species~persist",
                                           "CV~sigma/mu",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))

df_param <- df0 %>%
  distinct(r1, sd_env, k) %>% 
  arrange(k, r1, sd_env)

## plot

source(here::here("code/figure_set_theme.R"))
theme_set(plt_theme)

list_g_theory <- foreach(i = seq_len(nrow(df_param))) %do% {
  
  ## choose one parameter set
  df_set <- df0 %>% 
    filter(r1 == df_param$r1[i],
           k == df_param$k[i],
           sd_env == df_param$sd_env[i]) %>% 
    mutate(alpha_label = case_when(alpha == 0.1 ~ "bar(alpha)==0.1",
                                   alpha == 0.5 ~ "bar(alpha)==0.5"),
           phi_label = case_when(phi == 0.8 ~ "f[R]==0.8",
                                 phi == 1 ~ "f[R]==1.0"))
  
  g <- df_set %>% 
    ggplot(aes(y = value,
               x = stock,
               color = group_id,
               fill = group_id)) +
    geom_smooth(size= 0.1,
                method = "loess") +
    scale_color_hue(labels = c("Whole", "Enhanced", "Unenhanced")) +
    labs(x = "Number of release (individuals)",
         color = "Species group") +
    facet_grid(cols = vars(alpha_label, phi_label),
               rows = vars(response_name),
               scales = "free_y",
               labeller = label_parsed) +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = "none") +
    theme(axis.title.y = element_blank())
  
  return(g)
}


# time series data --------------------------------------------------------

## read data

file_name <- list.files(path = "data_fmt", full.names = TRUE) %>%
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
               ncol = 6,
               scales = "free_y") +
    ylab("Density (ind/sq-m)") +
    xlab("Year") +
    labs(color = "Site")
  
  return(g)
}

