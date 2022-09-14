
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

## raw data for cv, mean, sd
source(here::here("code/data_fmt_analysis.R"))

df_m <- df_ssm %>%
  mutate(river_id = as.numeric(factor(river))) %>% 
  group_by(river) %>% 
  mutate(site0 = as.numeric(factor(site)),
         p_id = as.numeric(factor(response, levels = c("sigma", "mu")))) %>% 
  ungroup() %>% 
  relocate(river, river_id, site, site0, site_id)

# plot --------------------------------------------------------------------

df_m %>% 
  ggplot(aes(x = mean_stock,
             y = value,
             color = river)) +
  geom_point(alpha = 0.5) +
  facet_grid(cols = vars(group),
             rows = vars(response),
             scales = "free_y") +
  scale_y_continuous(trans = "log10") +
  theme_bw()
