
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

df0 <- readRDS(here::here("output/summary_multi_ricker_sparse.rds"))

df_est <- df0 %>% 
  mutate(taxon.y = str_replace_all(taxon.y, pattern = "_", replacement = " "),
         taxon.x = str_replace_all(taxon.x, pattern = "_", replacement = " "),
         lbs.y = ifelse(test = str_detect(taxon.y, "\\sspp"),
                        yes = paste0('italic("',
                                     str_remove(taxon.y, "\\sspp"),
                                     '")~spp.'),
                        no = paste0('italic("', taxon.y, '")')),
         lbs.x = ifelse(test = str_detect(taxon.x, "\\sspp"),
                        yes = paste0('italic("',
                                     str_remove(taxon.x, "\\sspp"),
                                     '")~spp.'),
                        no = paste0('italic("', taxon.x, '")'))
  )


# join functional distance ------------------------------------------------

## intra-specific competition
df_alpha0 <- df_est %>% 
  filter(param_name == "alpha",
         x1 == x2) %>% 
  dplyr::select(site,
                param_name,
                alpha0 = median,
                x1,
                taxon.x)

## join
df_pd <- df_est %>% 
  filter(param_name == "alpha") %>% 
  left_join(df_alpha0,
            by = c("site",
                   "param_name",
                   "x1",
                   "taxon.x")) %>% 
  dplyr::select(site,
                param_name,
                median,
                alpha0,
                taxon.x,
                taxon.y,
                lbs.y,
                lbs.x) %>%
  mutate(alpha_prime = median / alpha0,
         value = ifelse(param_name == "alpha", alpha_prime, median),
         site_id = str_replace(site,
                               "\\d{1,}",
                               paste("", str_extract(site, "\\d{1}"))),
         site_id = str_to_sentence(site_id)) %>% 
  filter(alpha_prime != 1)

## df for plot
df_plot <- df_pd %>%
  group_by(taxon.y, site) %>%
  summarize(alpha50 = median(value)) %>%
  arrange(alpha50) %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  right_join(df_pd,
             by = c("taxon.y",
                    "site"))


# figure ------------------------------------------------------------------

## by taxa ####
taxon_ordered <- df_plot %>%
  group_by(lbs.y) %>% 
  summarize(alpha50 = median(alpha_prime)) %>% 
  arrange(alpha50) %>% 
  pull(lbs.y)

g_alpha <- df_plot %>% 
  mutate(lbs.y = factor(lbs.y,
                        levels = taxon_ordered)) %>% 
  ggplot(aes(y = lbs.y,
             x = value)) +
  geom_vline(aes(xintercept = median(value)),
             col = grey(0.8),
             size = 0.5,
             linetype = "dashed") +
  geom_jitter(height = 0.1,
              width = 0,
              size = 1,
              color = grey(0.5)) +
  geom_boxplot(aes(fill = lbs.y),
               color = grey(0.4),
               alpha = 0.75,
               outlier.colour = NA) +
  geom_xsidedensity(aes(fill = lbs.y),
                    color = NA,
                    alpha = 0.8,
                    position = "stack") +
  labs(y = expression("Taxon"~italic("j")~"affecting taxon"~italic("i")),
       x = expression("Competition coefficient"~alpha[ij])) +
  guides(color = "none",
         fill = "none") +
  scale_y_discrete(labels = label_parse()) +
  scale_x_continuous(breaks = seq(0, max(df_plot$value), by = 0.25)) +
  MetBrewer::scale_fill_met_d("Hiroshige", direction = -1) +
  ggridges::theme_ridges() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "gray"),
        axis.line = element_line(color = "gray"))

## add fish image
fish <- list.files(here::here("data_raw/image"),
                   pattern = ".png",
                   full.names = TRUE)

yp <- seq(0.34, length = 10, by = -0.075)
xp <- -0.1

g_alpha_img <- ggdraw()

for(i in 1:length(fish)) {
  
  g_alpha_img <- g_alpha_img +
    draw_image(fish[i], x = xp, y = yp[i], scale = 0.06)
    
}

g_alpha_img <- g_alpha_img +
  draw_plot(g_alpha)


# export ------------------------------------------------------------------

ggsave(g_alpha_img,
       filename = here::here("output/figure_alpha_dist.pdf"),
       height = 5,
       width = 7)
