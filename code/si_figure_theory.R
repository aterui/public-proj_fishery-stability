
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# theoretical prediction --------------------------------------------------

## call `sim_result`
sim_result <- readRDS(here::here("output/result_ricker.rds"))

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
                                levels = c("CV~sigma/mu",
                                           "Number~of~species~persist",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))

df_param <- df0 %>%
  distinct(r1, k, sd_env, n_species, r_max) %>% 
  arrange(k, r1)

## plot

source(here::here("code/set_figure_theme.R"))
theme_set(plt_theme)

list_g_theory <- foreach(i = seq_len(nrow(df_param))) %do% {
  
  ## choose one parameter set
  df_set <- df0 %>% 
    filter(r1 == df_param$r1[i],
           k == df_param$k[i]) %>% 
    mutate(alpha_label = case_when(alpha == min(df0$alpha) ~ sprintf('bar(alpha)=="%.2f"',
                                                                     alpha),
                                   alpha == max(df0$alpha) ~ sprintf('bar(alpha)=="%.2f"',
                                                                     alpha)),
           phi_label = case_when(phi == min(df0$phi) ~ sprintf('f[R]=="%.1f"', phi),
                                 phi == max(df0$phi) ~ sprintf('f[R]=="%.1f"', phi))
           )
  
  g <- df_set %>% 
    ggplot(aes(y = value,
               x = stock,
               color = group_id,
               fill = group_id)) +
    geom_smooth(size= 1,
                method = "loess") +
    scale_color_hue(labels = c("Whole", "Enhanced", "Unenhanced")) +
    labs(x = "Number of releases (individuals)",
         color = "Species group") +
    facet_grid(cols = vars(alpha_label, phi_label),
               rows = vars(response_name),
               scales = "free_y",
               labeller = label_parsed, 
               switch = "y") +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = "none") +
    theme(axis.title.y = element_blank(),
          strip.placement = "outside")
  
  return(g)
}

