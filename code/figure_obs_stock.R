
# setup -------------------------------------------------------------------

#rm(list = ls())
pacman::p_load(tidyverse)


# data --------------------------------------------------------------------

## raw data for cv, mean, sd
source("code/data_fmt_analysis.R")

df_m <- do.call(bind_rows, list_ssm) %>%
  filter(!(group != "all" & response == "cv")) %>%
  mutate(response = case_when(response == "cv" ~ "CV~sigma/mu",
                              response == "mu" ~ "Mean~mu~(ind.~m^-2)",
                              response == "sigma" ~ "SD~sigma~(ind.~m^-2)"),
         group = case_when(group == "all" ~ "All",
                           group == "masu" ~ "Enhanced",
                           group == "other" ~ "Unenhanced"))

## parameter estimates
file_name <- list.files(path = "result", full.names = TRUE) %>%
  as_tibble() %>%
  filter(str_detect(value, ".csv")) %>%
  pull()

df_beta <- lapply(file_name,
                  FUN = function(x) {
                    mutate(read_csv(x),
                           group = str_extract(x, "all|masu|other"))
                  }) %>%
  do.call(bind_rows, .) %>%
  filter(!(group != "all" & response == "cv")) %>%
  mutate(response = case_when(response == "cv" ~ "CV~sigma/mu",
                              response == "mu" ~ "Mean~mu~(ind.~m^-2)",
                              response == "sigma" ~ "SD~sigma~(ind.~m^-2)"),
         group = case_when(group == "all" ~ "All",
                           group == "masu" ~ "Enhanced",
                           group == "other" ~ "Unenhanced"),
         pp = prob_positive,
         pn = 1 - prob_positive) %>%
  filter(str_detect(parameter, "b_raw")) %>%
  pivot_wider(id_cols = c(response, group),
              names_from = parameter,
              values_from = c(median, pp, pn)) %>%
  mutate(prob = ifelse(`pp_b_raw[2]` > `pn_b_raw[2]`,
                       `pp_b_raw[2]`,
                       `pn_b_raw[2]`)) %>%
  rename(b1 = `median_b_raw[1]`,
         b2 = `median_b_raw[2]`) %>%
  dplyr::select(response, group, b1, b2, prob)


# plot --------------------------------------------------------------------

source("code/figure_set_theme.R")
theme_set(plt_theme)

## regression estimates
x <- seq(min(df_m$mean_stock),
         max(df_m$mean_stock),
         length = 100)

df_y <- df_beta %>%
  slice(rep(1:n(), each = 100)) %>% #duplicate for prediction
  group_by(response, group) %>%
  summarize(y = exp(b1 + b2 * x),
            x = x,
            prob = prob,
            lty = case_when(prob > 0.95 ~ "a",
                            between(prob, 0.90, 0.95) ~ "b",
                            prob < 0.90 ~ "c"))
  
## plot
df_plot <- df_m %>% 
  group_by(river, response, group) %>% 
  summarize(value = exp(mean(log(value))),
            mean_stock = unique(mean_stock))

g_obs <- df_plot %>% 
  ggplot(aes(x = mean_stock,
             y = value)) +
  facet_wrap(facets = ~ response,
             nrow = 3,
             scales = "free_y",
             labeller = label_parsed) + 
  geom_point(data = filter(df_plot, group == "Enhanced"),
             color = "darkseagreen2") +
  geom_point(data = filter(df_plot, group == "Unenhanced"),
             color = "lightskyblue2") +
  geom_point(data = filter(df_plot, group == "All"),
             color = "pink") +
  geom_line(aes(y = y,
                x = x,
                color = group,
                linetype = lty),
            lineend = "round",
            linejoin = "round",
            data = df_y) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  labs(x = expression("Number of release (thousand fish year"^-1*")"),
       color = "Species group") +
  guides(linetype = "none") +
  theme(axis.title.y = element_blank())
