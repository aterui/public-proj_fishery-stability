
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

source(here::here("code/figure_set_theme.R"))

# read data ---------------------------------------------------------------

df_sim <- readRDS(here::here("result/result_ricker_2sp.rds")) %>% 
  filter(status == "all",
         phi == 1) %>% 
  mutate(cv = sd_density / mean_density,
         sd_type = case_when(sd_env == 0 ~ "Deterministic",
                             sd_env == 0.5 ~ "Stochastic"),
         alpha_label = case_when(alpha == 0.1 ~ sprintf('"Weak competition"~(alpha=="%.2f")',
                                                        alpha),
                                 alpha == 0.5 ~ sprintf('"Strong competition"~(alpha=="%.2f")',
                                                        alpha))) %>% 
  pivot_longer(cols = c(mean_density,
                        sd_density,
                        cv),
               names_to = "metric",
               values_to = "value") %>% 
  mutate(metric_label = case_when(metric == "cv" ~ "CV~sigma/mu",
                                  metric == "mean_density" ~ "Mean~mu",
                                  metric == "sd_density" ~ "SD~sigma"))



# plot --------------------------------------------------------------------

## deterministic
g_d <- df_sim  %>%
  filter(sd_env == 0) %>% 
  group_by(r1, k, alpha, sd_env, metric) %>% 
  mutate(rho = cor(x = value,
                   y = stock,
                   method = "spearman")) %>% 
  ggplot(aes(x = r1,
             y = k,
             fill = rho)) +
  geom_raster(alpha = 0.8) +
  geom_vline(aes(xintercept = 1.43),
             color = grey(1),
             linetype = "dashed") +
  facet_grid(cols = vars(alpha_label),
             rows = vars(metric_label),
             labeller = label_parsed) +
  MetBrewer::scale_fill_met_c("Hiroshige",
                              direction = -1) +
  geom_point(data = expand.grid(r1 = seq(0.5, 3.5, by = 1),
                                k = c(100, 400)),
             aes(x = r1,
                 y = k),
             shape = 21,
             color = "salmon",
             fill = grey(1, 0.5)) +
  labs(y = "Carrying capacity K",
       x = "Intrinsic growth rate r",
       fill = expression("Correlation"~rho)) +
  theme_classic() +
  theme(strip.background = element_blank())

## example time-series
df_para <- expand.grid(r = seq(0.5, 3.5, by = 1),
                       alpha = c(0.1, 0.5),
                       stock = seq(0, 500, by = 250),
                       k = c(100, 400))

df_g <- foreach(i = seq_len(nrow(df_para)),
                .combine = bind_rows) %do% {
                  
                  set.seed(123)
                  list_ex <- cdynsim(n_timestep = 50,
                                     n_warmup = 0,
                                     n_burnin = 0,
                                     n_species = 2,
                                     n_stock_start = 11,
                                     stock = df_para$stock[i],
                                     sd_env = 0,
                                     alpha = df_para$alpha[i],
                                     r = df_para$r[i],
                                     k = df_para$k[i],
                                     seed = 5,
                                     phi = 1)
                  
                  df_g0 <- list_ex$df_dyn %>% 
                    group_by(timestep) %>% 
                    summarize(density = sum(density),
                              species = "Whole") %>% 
                    bind_cols(df_para[i,])
                  
                  
                  return(df_g0)
                  
                }

g_ex <- df_g %>% 
  mutate(alpha_label = case_when(alpha == 0.1 ~ sprintf('"Weak competition"~(alpha=="%.1f")',
                                                        alpha),
                                 alpha == 0.5 ~ sprintf('"Strong competition"~(alpha=="%.1f")',
                                                        alpha)),
         r_label = case_when(r == 0.5 ~ sprintf('r=="%.1f"', r),
                             r == 1.5 ~ sprintf('r=="%.1f"', r),
                             r == 2.5 ~ sprintf('r=="%.1f"', r),
                             r == 3.5 ~ sprintf('r=="%.1f"', r))) %>% 
  ggplot(aes(x = timestep,
             y = density,
             color = factor(stock),
             linetype = factor(k))) +
  geom_line() +
  geom_vline(aes(xintercept = 11),
             col = grey(0.5),
             linetype = "dashed") +
  facet_grid(rows = vars(r_label),
             cols = vars(alpha_label),
             labeller = label_parsed,
             scales = "free_y") +
  MetBrewer::scale_color_met_d("Hiroshige",
                               direction = -1) +
  labs(y = "Community density",
       x = "Time step",
       color = "Release level",
       linetype = "Carrying capacity K")


# combine plot ------------------------------------------------------------

g_d + g_ex + plot_annotation(tag_levels = "A")
