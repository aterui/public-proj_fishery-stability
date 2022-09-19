
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

## call `sim_result`
sim_result <- readRDS(file = here::here("output/result_ricker.rds"))

df0 <- sim_result %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "response",
               values_to = "value") %>% 
  filter(r1 == 0.5,
         sd_env == 0.75,
         alpha == 0.1,
         phi == 0.5,
         k == 100,
         response %in% c("n_sp_last", "cv", "mean_density", "sd_density")) %>% 
  filter(!(response == "cv" & status != "all")) %>%
  filter(!(response == "n_sp_last" & status != "all")) %>%
  mutate(response_name = case_when(response == "n_sp_last" ~ "Number~of~species~persist",
                                   response == "cv" ~ "CV~sigma/mu",
                                   response == "mean_density" ~ "Mean~mu~(ind.)",
                                   response == "sd_density" ~ "SD~sigma~(ind.)"),
         group_id = case_when(status == "all" ~ "a",
                              status == "enhanced" ~ "b",
                              status == "unenhanced" ~ "c")) %>% 
  bind_rows(tibble(group_id = "a",
                   status = "dummy",
                   response_name = "SD~sigma~(ind.)",
                   value = max(.$value[.$response == "mean_density"]),
                   stock = 0)) %>% 
  mutate(response_name = factor(response_name,
                                levels = c("CV~sigma/mu",
                                           "Number~of~species~persist",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))


# plot --------------------------------------------------------------------

source(here::here("code/set_figure_theme.R"))
theme_set(plt_theme)

g_theory <- df0 %>% 
  ggplot(aes(y = value,
             x = stock,
             color = group_id,
             fill = group_id)) +
  geom_point(data = . %>% filter(status == "enhanced"),
             size = pt_size,
             color = hue_pal(h.start = hs[2], l = lum, c = con)(1)) +
  geom_point(data = . %>% filter(status == "unenhanced"),
             size = pt_size,
             color = hue_pal(h.start = hs[3], l = lum, c = con)(1)) +
  geom_point(data = . %>% filter(status == "all"),
             size = pt_size,
             color = hue_pal(h.start = hs[1], l = lum, c = con)(1)) +
  geom_point(data = filter(df0, status == "dummy"),
             color = NA) +
  geom_smooth(size= 0.5,
              method = "loess") +
  scale_color_hue(h = c(hs[1], hs[3]),
                  l = 70,
                  labels = c("Whole", "Enhanced", "Unenhanced")) +
  scale_fill_hue(h = c(hs[1], hs[3]),
                 l = 70) +
  labs(x = "Number of releases (individuals)",
       y = "Value") +
  facet_wrap(facets = ~ response_name,
             nrow = 4,
             scales = "free_y",
             labeller = label_parsed,
             strip.position = "left") +
  guides(color = "none",
         fill = "none") +
  theme(axis.title.y = element_blank(),
        strip.placement = "outside")

