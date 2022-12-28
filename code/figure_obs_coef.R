
# setup -------------------------------------------------------------------

pacman::p_load(foreach,
               tidyverse,
               ggridges)


# data --------------------------------------------------------------------

## mcmc samples from the regression model
df_mcmc <- list.files(path = here::here("output"),
                      full.names = TRUE,
                      pattern = "mcmc_reg") %>%
  readRDS() %>% 
  MCMCvis::MCMCchains() %>% # combine 4 chains
  as_tibble() %>% 
  dplyr::select(starts_with("a[2,")) %>% # a[2,.,.], standardized coef for release
  pivot_longer(cols = starts_with("a[2,"),
               names_to = "param") %>% 
  mutate(param_numeric = str_remove_all(param, "a\\[|\\]")) %>% 
  separate(param_numeric,
           into = c("k", "g", "p"),
           sep = ",",
           convert = T) %>% 
  mutate(group = case_when(g == 1 ~ "Whole",
                           g == 2 ~ "Enhanced", #masu salmon
                           g == 3 ~ "Unenhanced"), #other
         response = case_when(p == 1 ~ "SD",
                              p == 2 ~ "Mean",
                              p == 3 ~ "Taxonomic richness",
                              p == 4 ~ "CV")) %>% 
  filter(!(group != "Whole" & response %in% c("Taxonomic richness", "CV"))) %>% 
  mutate(group = factor(group,
                        levels = c("Whole",
                                   "Enhanced",
                                   "Unenhanced")),
         response = factor(response,
                           levels = c("SD",
                                      "Mean",
                                      "Taxonomic richness",
                                      "CV"))) %>% 
  dplyr::select(group, response, value)

df_q <- df_mcmc %>% 
  group_by(group, response) %>% 
  summarize(median = median(value),
            upper = quantile(value, 0.975),
            lower = quantile(value, 0.025))

# plot --------------------------------------------------------------------

source(here::here("code/set_figure_theme.R"))

g_coef <- df_mcmc %>% 
  ggplot(aes(x = value,
             y = response,
             color = group,
             fill = group)) +
  geom_vline(xintercept = 0,
             color = grey(0.5),
             linetype = "dashed") +
  geom_density_ridges(alpha = 0.5,
                      scale = 0.9,
                      size = 0.25) +
  geom_point(data = df_q,
             pch = 2,
             aes(x = median,
                 y = response)) +
  labs(y = "Response",
       x = "Standardized release effect",
       fill = "Species group",
       color = "Species group") +
  scale_color_hue(h = c(hs[1], hs[3]),
                  l = 50,
                  labels = c("Whole", "Enhanced", "Unenhanced")) +
  scale_fill_hue(h = c(hs[1], hs[3]),
                 l = lum,
                 c = con,
                 labels = c("Whole", "Enhanced", "Unenhanced")) +
  theme_ridges() +
  scale_y_discrete(expand = expansion(add = c(0.5, 1)))

ggsave(g_coef,
       filename = here::here("output/figure_coef.pdf"),
       width = 7,
       height = 4.5)
