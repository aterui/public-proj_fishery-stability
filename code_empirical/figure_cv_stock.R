
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_analysis.R")
df_m <- do.call(bind_rows, list_ssm)

## raw data
file_id <- list.files(path = "data_fmt") %>% 
  str_detect(pattern = "data_ssm")

figure_name <- paste0("figure/figure_cv_mu_sd",
                      str_remove(list.files(path = "data_fmt")[file_id],
                                 "data_ssm_est")) %>% 
  str_replace(".csv", ".pdf")

## parameter estimates
file_name <- list.files(path = "result", full.names = TRUE)

df_beta <- lapply(file_name, FUN = function(x) {
    mutate(read_csv(x), group = str_extract(x, "all|masu|other"))
  }) %>%
  do.call(bind_rows, .) %>%
  filter(str_detect(parameter, "b_raw")) %>%
  pivot_wider(id_cols = c(response, group),
              names_from = parameter,
              values_from = median) %>%
  rename(b1 = `b_raw[1]`,
         b2 = `b_raw[2]`)

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
            x = x)
  
## plot
g <- df_m %>% 
  group_by(river, response, group) %>% 
  summarize(value = mean(value),
            mean_stock = unique(mean_stock)) %>% 
  ggplot(aes(x = mean_stock,
             y = value,
             color = group)) +
  geom_point(size = 1.5,
             alpha = 0.3) +
  geom_line(aes(y = y,
                x = x),
            data = df_y) +
  facet_wrap(facets = ~ response,
             ncol = 3,
             scales = "free_y",
             labeller = label_parsed) + 
  scale_color_manual(values = c("all" = "black",
                                "masu" = "salmon",
                                "other" = "steelblue"),
                     aesthetics = "color") +
  xlab("Fish stock (thousand fish)") +
  ylab("value")# +
  #scale_y_continuous(trans = "log10")
  
## export
ggsave("figure/figure_cv_mu_sd.pdf",
       width = 10,
       height = 3)
