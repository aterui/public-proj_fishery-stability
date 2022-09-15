
# setup -------------------------------------------------------------------

#rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

## raw data for cv, mean, sd
suppressMessages(source(here::here("code/data_fmt_reg.R")))

df_m <- df_ssm %>%
  filter(!(group != "all" & response == "cv")) %>%
  filter(!(group != "all" & response == "n_species")) %>%
  mutate(group = factor(group),
         response = ifelse(response == "n_species",
                           "species_richness",
                           response),
         response = fct_relevel(response, "cv", "species_richness"))

## regression data
df_reg <- list.files(path = here::here("result"),
                     full.names = TRUE,
                     pattern = "summary_reg") %>% 
  readRDS() %>% 
  mutate(y = response)

## join weighted stock
df_m <- df_reg %>% 
  filter(str_detect(parameter, "ef_stock")) %>%
  select(site_id_numeric,
         ef_stock = median) %>% 
  right_join(df_m,
             by = "site_id_numeric")

ef_stock <- seq(min(df_m$ef_stock), max(df_m$ef_stock), length = 100)

## slope
df_mcmc <- readRDS(here::here("output/mcmc_reg.rds")) %>% 
  MCMCvis::MCMCchains(param = "beta_raw") %>% 
  t() %>% 
  as_tibble(rownames = "param") %>% 
  mutate(x = str_extract(param, "\\d{1,},\\d{1,},\\d{1,}")) %>% 
  separate(x,
           into = c("k", "g", "p"),
           remove = TRUE) %>%
  mutate(group = case_when(g == 1 ~ "all",
                           g == 2 ~ "masu_salmon",
                           g == 3 ~ "other"),
         response = case_when(p == 1 ~ "sigma",
                              p == 2 ~ "mu",
                              p == 3 ~ "species_richness",
                              p == 4 ~ "cv")) %>% 
  filter(group == "all", response %in% c("species_richness", "cv")) %>% 
  relocate(param, group, response, k, g, p)

df_beta <- df_reg %>%
  mutate(param_k = paste0(param_name, k),
         pp = pr_po,
         pn = 1 - pr_po) %>%
  filter(str_detect(parameter, "beta_raw")) %>%
  pivot_wider(id_cols = c(response, group),
              names_from = param_k,
              values_from = c(median, pp, pn)) %>%
  mutate(prob = ifelse(pp_beta_raw2 > pn_beta_raw2,
                       pp_beta_raw2,
                       pn_beta_raw2)) %>%
  rename(beta1 = median_beta_raw1,
         beta2 = median_beta_raw2) %>%
  dplyr::select(response, group, beta1, beta2, prob)


# predicted values --------------------------------------------------------

df_y <- df_beta %>%
  slice(rep(1:n(), each = 100)) %>% #duplicate for prediction
  group_by(response, group) %>%
  summarize(x = ef_stock,
            y = exp(beta1 + beta2 * x),
            prob = prob,
            lty = case_when(prob > 0.95 ~ "a",
                            between(prob, 0.90, 0.95) ~ "b",
                            prob < 0.90 ~ "c")) %>% 
  mutate(response = factor(response),
         response = fct_relevel(response, "cv", "species_richness"))

df_fit <- lapply(c("cv", "species_richness"),
                 FUN = function(i) {
                   m_beta <- df_mcmc %>%
                     filter(response == i) %>%
                     select(where(is.numeric)) %>% 
                     data.matrix()
                   
                   y <- exp(model.matrix(~ef_stock) %*% m_beta) %>% 
                     apply(MARGIN = 1,
                           quantile, c(0.025, 0.5, 0.975)) %>% 
                     t() %>% 
                     as_tibble() %>% 
                     rename(low = `2.5%`,
                            median = `50%`,
                            high = `97.5%`) %>% 
                     mutate(x = ef_stock,
                            response = factor(i,
                                              levels = levels(df_m$response)),
                            group = factor("all",
                                           levels = levels(df_m$group)))
                   
                   return(y)
                 }) %>% 
  bind_rows()


# plot --------------------------------------------------------------------

source(here::here("code/set_figure_theme.R"))
theme_set(plt_theme)

## plot
ylab <- c(`cv` = "CV~sigma/mu",
          `species_richness` = "Species~richness",
          `mu` = "Mean~mu~(ind.~m^-2)",
          `sigma` = "SD~sigma~(ind.~m^-2)")

g_obs <- ggplot(data = df_m,
                aes(x = ef_stock,
                    y = value)) +
  facet_wrap(facets = ~ response,
             nrow = 4,
             scales = "free_y",
             strip.position = "left",
             labeller = labeller(response = as_labeller(ylab, label_parsed))) + 
  geom_point(data = . %>% filter(group == "masu_salmon"),
             color = hue_pal(h.start = hs[2], l = lum, c = con)(1),
             size = pt_size) +
  geom_point(data = . %>% filter(group == "other"),
             color = hue_pal(h.start = hs[3], l = lum, c = con)(1),
             size = pt_size) +
  geom_point(data = . %>% filter(group == "all"),
             color = hue_pal(h.start = hs[1], l = lum, c = con)(1),
             size = pt_size) +
  geom_ribbon(aes(x = x,
                  ymin = low,
                  ymax = high,
                  fill = group),
              y = NA,
              color = NA,
              alpha = 0.3,
              data = df_fit) +
  geom_line(aes(y = y,
                x = x,
                color = group,
                linetype = lty),
            data = df_y) +
  scale_color_hue(h = c(hs[1], hs[3]),
                  l = 70,
                  labels = c("Whole", "Enhanced", "Unenhanced")) +
  scale_fill_hue(h = c(hs[1], hs[3]),
                 l = 70,
                 labels = c("Whole", "Enhanced", "Unenhanced")) +
  scale_linetype_manual(values = c(`a` = "solid",
                                   `b` = "dashed",
                                   `c` = "dotted"),
                        labels = c(`a` = "> 0.95",
                                   `b` = "0.90-0.95",
                                   `c` = "< 0.90")) +
  labs(x = expression("Effective release (million fish year"^-1*")"),
       y = "Value",
       color = "Species group",
       linetype = "Posterior prob.") +
  guides(fill = "none") +
  theme(axis.title.y = element_blank(),
        strip.placement = "outside")

