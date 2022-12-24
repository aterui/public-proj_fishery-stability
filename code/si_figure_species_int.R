
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

df0 <- readRDS(here::here("output/summary_multi_ricker_sparse.rds"))

df_est <- df0 %>% 
  mutate(taxon.y = spabb(taxon.y, sep = "_"),
         taxon.x = spabb(taxon.x, sep = "_"),
         lbs.y = ifelse(test = str_detect(taxon.y, "\\sspp."),
                        yes = paste0('italic("',
                                     str_remove(taxon.y, "\\sspp."),
                                     '")~spp.'),
                        no = paste0('italic("', taxon.y, '")')),
         lbs.x = ifelse(test = str_detect(taxon.x, "\\sspp."),
                        yes = paste0('italic("',
                                     str_remove(taxon.x, "\\sspp."),
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

## by site ####
g_alpha_site <- df_plot %>%
  filter(alpha_prime != 1) %>% 
  ggplot(aes(y = factor(lbs.x, levels = rev(sort(unique(lbs.x)))),
             x = lbs.y,
             fill = value,
             label = sprintf("%.2f", value))) +
  geom_tile(alpha = 0.8) +
  geom_text(size = 2) +
  facet_wrap(facets = ~site_id,
             scales = "free",
             nrow = 3,
             ncol = 3) +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "italic",
                                   angle = 90,
                                   vjust = 0.2,
                                   hjust = 0.95)) +
  scale_x_discrete(labels = label_parse()) +
  scale_y_discrete(labels = label_parse()) +
  MetBrewer::scale_fill_met_c("Hiroshige", direction = -1) +
  labs(y = expression("Taxon"~italic("i")),
       x = expression("Taxon"~italic("j")),
       fill = expression(alpha[ij]))


# export ------------------------------------------------------------------

ggsave(g_alpha_site,
       filename = here::here("output/figure_alpha_site.pdf"),
       height = 9,
       width = 10)
