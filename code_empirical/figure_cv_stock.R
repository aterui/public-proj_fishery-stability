
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")
df_m <- do.call(bind_rows, list_ssm) %>%
  mutate(response = case_when(response == "cv" ~ "CV",
                              TRUE ~ as.character(response)))

## raw data
figure_name <- paste0("figure/figure_cv_mu_sd_",
                      c("all", "masu", "other"),
                      ".pdf")

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
  mutate(response = case_when(response == "cv" ~ "CV",
                              TRUE ~ as.character(response)),
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
  select(response, group, b1, b2, prob)


# plot --------------------------------------------------------------------

source("figure_set_theme.R")
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
            prob = prob)
  
## plot
g_obs <- df_m %>% 
  group_by(river, response, group) %>% 
  summarize(value = mean(value),
            mean_stock = unique(mean_stock)) %>% 
  ggplot(aes(x = mean_stock,
             y = value,
             color = group)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = y,
                x = x,
                alpha = prob),
            size = 1,
            lineend = "round",
            linejoin = "round",
            data = df_y) +
  facet_wrap(facets = ~ response,
             nrow = 3,
             scales = "free_y",
             labeller = label_parsed) + 
  labs(x = "Annual fish stock (thousand fish)",
       y = "Value",
       color = "Fish group",
       alpha = "Prob.")
  
## export
ggsave("figure/figure_cv_mu_sd.pdf",
       width = 4,
       height = 8)
