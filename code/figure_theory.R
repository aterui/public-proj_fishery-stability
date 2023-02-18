
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))


# data --------------------------------------------------------------------

## call `sim_result`
sim_result <- readRDS(file = here::here("output/result_ricker.rds"))

df_sim <- sim_result %>% 
  mutate(cv = sd_density / mean_density) %>% 
  pivot_longer(cols = mean_density:cv,
               names_to = "response",
               values_to = "value") %>% 
  filter(r1 == 1.5,
         sd_env == max(sim_result$sd_env), # sd_env = 0.5
         alpha == min(sim_result$alpha), # alpha = 0.25
         phi == max(sim_result$phi), # phi = 1
         k == min(sim_result$k), # k= 100
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
  bind_rows() %>% 
  mutate(response_name = factor(response_name,
                                levels = c("CV~sigma/mu",
                                           "Number~of~species~persist",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))

## loess prediction with prediction intervals
df1 <- df_sim %>% 
  filter(status != "dummy") %>% 
  group_by(status,
           group_id,
           response, 
           response_name) %>% 
  do(fit = loess.sd(dplyr::select(., stock, value), # fit loess by group
                    nsigma = 1.96, # 95% prediction interval multiplier
                    span = 0.75)) %>%
  ungroup()

df_plot <- lapply(seq_len(nrow(df1)),
                  FUN = function(i) {
                    x <- df1 %>% slice(i)
                    y <- bind_cols(dplyr::select(x,
                                                 status,
                                                 group_id, 
                                                 response,
                                                 response_name),
                                   stock = x$fit[[1]]$x,
                                   value = x$fit[[1]]$y,
                                   lower = x$fit[[1]]$lower,
                                   upper = x$fit[[1]]$upper)
                    
                    return(y)
                  }) %>% 
  bind_rows()

# empirical data ----------------------------------------------------------

suppressMessages(source(here::here("code/data_fmt_reg.R")))

df_m <- df_ssm %>%
  filter(!(group != "all" & response == "cv")) %>%
  filter(!(group != "all" & response == "n_species")) %>%
  mutate(group = factor(group),
         response = ifelse(response == "n_species",
                           "species_richness",
                           response),
         response = fct_relevel(response, "cv", "species_richness"),
         dummy = 0) %>% 
  bind_rows(tibble(dummy = 1,
                   group = factor("all", levels(.$group)),
                   response = factor("sigma", levels(.$response)),
                   value = max(.$value[.$response == "mu"]),
                   site_id_numeric = 1))

df_max <- df_m %>% 
  group_by(response) %>% 
  summarize(value = max(value))

df_dummy <- tibble(group_id = "a",
                   status = "dummy",
                   response = c("cv",
                                "species_richness",
                                "mu",
                                "sigma"),
                   response_name = c("CV~sigma/mu",
                                     "Number~of~species~persist",
                                     "Mean~mu~(ind.)",
                                     "SD~sigma~(ind.)"),
                   stock = 0) %>% 
  left_join(df_max,
            by = "response") %>% 
  mutate(value = case_when(response == "sigma" ~ max(df_plot %>% 
                                                       filter(response == "mean_density") %>% 
                                                       pull(value) %>% 
                                                       max()),
                           TRUE ~ value)) %>% 
  filter(response == "sigma") %>% 
  mutate(response_name = factor(response_name,
                                levels = c("CV~sigma/mu",
                                           "Number~of~species~persist",
                                           "Mean~mu~(ind.)",
                                           "SD~sigma~(ind.)")))

# plot --------------------------------------------------------------------

source(here::here("code/set_figure_theme.R"))
theme_set(plt_theme)


g_theory <- df_sim %>% 
  ggplot(aes(y = value,
             x = stock,
             color = group_id,
             fill = group_id)) +
  geom_point(data = df_sim %>% filter(status == "enhanced"),
             size = pt_size,
             color = hue_pal(h.start = hs[2], l = lum, c = con)(1)) +
  geom_point(data = df_sim %>% filter(status == "unenhanced"),
             size = pt_size,
             color = hue_pal(h.start = hs[3], l = lum, c = con)(1)) +
  geom_point(data = df_sim %>% filter(status == "all"),
             size = pt_size,
             color = hue_pal(h.start = hs[1], l = lum, c = con)(1)) +
  geom_ribbon(data = df_plot, # for prediction interval
              aes(x = stock,
                  ymin = lower,
                  ymax = upper),
              alpha = 0.4,
              color = grey(0, alpha = 0)) +
  geom_smooth(method = "loess",
              se = TRUE,
              size = lwd,
              span = 0.75) +
  geom_point(data = df_dummy,
             aes(y = value,
                 x = stock),
             color = grey(0, 0)) +
  scale_color_hue(h = c(hs[1], hs[3]),
                  l = 60,
                  labels = c("Whole", "Enhanced", "Unenhanced")) +
  scale_fill_hue(h = c(hs[1], hs[3]),
                 l = lum,
                 c = con) +
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

