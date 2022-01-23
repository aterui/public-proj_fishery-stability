
# setup -------------------------------------------------------------------

#rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# data --------------------------------------------------------------------

## raw data for cv, mean, sd
source("code/data_fmt_analysis.R")

list_df <- lapply(list_ssm, function(x) {
  x %>% 
    pivot_wider(id_cols = c(river,
                            site,
                            site_id,
                            group,
                            mean_stock,
                            n_species),
                names_from = response,
                values_from = value) %>% 
    pivot_longer(cols = c(n_species, cv, mu, sigma),
                 names_to = "response",
                 values_to = "value")
})


df_m <- do.call(bind_rows, list_df) %>%
  filter(!(group != "all" & response == "cv")) %>%
  filter(!(group != "all" & response == "n_species")) %>%
  mutate(response = case_when(response == "n_species" ~ "Species~richness",
                              response == "cv" ~ "CV~sigma/mu",
                              response == "mu" ~ "Mean~mu~(ind.~m^-2)",
                              response == "sigma" ~ "SD~sigma~(ind.~m^-2)"),
         group_id = case_when(group == "all" ~ "a",
                              group == "masu" ~ "b",
                              group == "other" ~ "c")) %>% 
  mutate(response = factor(response,
                           levels = c("CV~sigma/mu",
                                      "Species~richness",
                                      "Mean~mu~(ind.~m^-2)",
                                      "SD~sigma~(ind.~m^-2)")))

## parameter estimates
file_name <- list.files(path = "result", full.names = TRUE) %>%
  as_tibble() %>%
  filter(str_detect(value, ".csv")) %>%
  filter(!str_detect(value, "reg_rich")) %>% 
  pull()

df_beta <- lapply(file_name, 
                  FUN = function(x) {
                    mutate(read_csv(x),
                           group = str_extract(x, "all|masu|other"))
                  }) %>%
  do.call(bind_rows, .) %>%
  filter(!(group != "all" & response == "cv")) %>%
  mutate(response = case_when(response == "richness" ~ "Species~richness",
                              response == "cv" ~ "CV~sigma/mu",
                              response == "mu" ~ "Mean~mu~(ind.~m^-2)",
                              response == "sigma" ~ "SD~sigma~(ind.~m^-2)"),
         group_id = case_when(group == "all" ~ "a",
                              group == "masu" ~ "b",
                              group == "other" ~ "c"),
         pp = prob_positive,
         pn = 1 - prob_positive) %>%
  mutate(response = factor(response,
                           levels = c("CV~sigma/mu",
                                      "Species~richness",
                                      "Mean~mu~(ind.~m^-2)",
                                      "SD~sigma~(ind.~m^-2)"))) %>% 
  filter(str_detect(parameter, "b_raw")) %>%
  pivot_wider(id_cols = c(response, group, group_id),
              names_from = parameter,
              values_from = c(median, pp, pn)) %>%
  mutate(prob = ifelse(`pp_b_raw[2]` > `pn_b_raw[2]`,
                       `pp_b_raw[2]`,
                       `pn_b_raw[2]`)) %>%
  rename(b1 = `median_b_raw[1]`,
         b2 = `median_b_raw[2]`) %>%
  dplyr::select(response, group, group_id, b1, b2, prob)


# plot --------------------------------------------------------------------

source("code/figure_set_theme.R")
theme_set(plt_theme)

## predicted values
df_y <- df_beta %>%
  slice(rep(1:n(), each = 100)) %>% #duplicate for prediction
  group_by(response, group, group_id) %>%
  summarize(x = seq(min(df_m$mean_stock),
                    max(df_m$mean_stock),
                    length = 100),
            y = exp(b1 + b2 * x),
            prob = prob,
            lty = case_when(prob > 0.95 ~ "a",
                            between(prob, 0.90, 0.95) ~ "b",
                            prob < 0.90 ~ "c"))
  
## plot
df_plot <- df_m %>% 
  group_by(river, response, group) %>% 
  summarize(value = exp(mean(log(value))), # geometric mean
            mean_stock = unique(mean_stock))

g_obs <- df_plot %>% 
  ggplot(aes(x = mean_stock,
             y = value)) +
  facet_wrap(facets = ~ response,
             nrow = 3,
             scales = "free_y",
             labeller = label_parsed) + 
  geom_point(data = filter(df_plot, group == "masu"),
             color = "darkseagreen2") +
  geom_point(data = filter(df_plot, group == "other"),
             color = "lightskyblue2") +
  geom_point(data = filter(df_plot, group == "all"),
             color = "pink") +
  geom_line(aes(y = y,
                x = x,
                color = group_id,
                linetype = lty),
            data = df_y) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = expression("Number of release (thousand fish year"^-1*")"),
       y = "Value",
       color = "Species group") +
  scale_color_hue(labels = c("Whole", "Enhanced", "Unenhanced")) +
  guides(linetype = "none") +
  theme(axis.title.y = element_blank())


# export ------------------------------------------------------------------

source("code/figure_map.R")

g <- g_hkd + g_obs + 
  plot_annotation(tag_levels = 'A')

ggsave(g,
       filename = here::here("figure/figure_obs.pdf"),
       width = 13,
       height = 6)
