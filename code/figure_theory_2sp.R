
# setup -------------------------------------------------------------------

rm(list = ls())

pacman::p_load(here,
               cdyns,
               tidyverse,
               foreach)

df_sim <- readRDS(here::here("result/result_ricker_2sp.rds"))

# plot --------------------------------------------------------------------

df_sim %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = c(mean_density, sd_density),
               names_to = "metric",
               values_to = "value") %>% 
  filter(r1 %in% unique(r1)[c(5, 15)],
         k %in% unique(k)[c(5, 15)],
         alpha == 0.5,
         phi == 0.5,
         sd_env == 0,
         status == "all") %>% 
  ggplot(aes(x = stock,
             y = value,
             color = metric)) +
  geom_line(alpha = 0.8) +
  facet_wrap(facets = ~r1 + k,
             scales = "free_y")
  

df_sim %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = c(mean_density,
                        sd_density,
                        cv),
               names_to = "metric",
               values_to = "value") %>%
  filter(status == "all",
         sd_env == 0) %>% 
  group_by(r1, k, alpha, phi, metric) %>% 
  mutate(rho = cor(x = value,
                   y = stock,
                   method = "spearman")) %>% 
  ggplot(aes(x = r1,
             y = k,
             fill = rho)) +
  geom_raster(alpha = 0.8) +
  facet_grid(rows = vars(alpha, phi),
             cols = vars(metric)) +
  MetBrewer::scale_fill_met_c("Hiroshige",
                              direction = -1)
