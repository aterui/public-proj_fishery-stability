
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/function_set.R"))

list_df0 <- readRDS(here::here("result/est_ssm_sparse1_fa.rds"))
df_trait <- readRDS(here::here("data_fmt/data_trait.rds"))
df_trait_cat <- df_trait %>% 
  select(where(is.factor),
         max_total_length,
         mouth_width)

df_est <- list_df0[[1]] %>% 
  mutate(taxon.y = spabb(taxon.y, sep = "_"),
         taxon.x = spabb(taxon.x, sep = "_"),
         lbs.y = ifelse(test = str_detect(taxon.y, "\\sspp\\."),
                        yes = paste0('italic("',
                                     str_remove(taxon.y, "\\sspp\\."),
                                     '")~spp.'),
                        no = paste0('italic("', taxon.y, '")')),
         lbs.x = ifelse(test = str_detect(taxon.x, "\\sspp\\."),
                        yes = paste0('italic("',
                                     str_remove(taxon.x, "\\sspp\\."),
                                     '")~spp.'),
                        no = paste0('italic("', taxon.x, '")'))
  )

# join functional distance ------------------------------------------------

## read functional distance matrix
m_fd <- readRDS(here::here("data_fmt/data_fd.rds"))
df_fd <- m2v(m_fd) %>% 
  rename(fd = value) %>% 
  mutate(taxon.x = spabb(row, sep = "_"),
         taxon.y = spabb(col, sep = "_"))

## intra-specific competition
df_alpha0 <- df_est %>% 
  filter(param_name == "alpha",
         x1 == x2) %>% 
  select(site,
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
  left_join(df_fd,
            by = c("taxon.x",
                   "taxon.y")) %>% 
  select(site,
         param_name,
         median,
         alpha0,
         fd,
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

## ordered taxon by site
site_order <- df_pd %>%
  group_by(site_id) %>%
  summarize(alpha_site = median(value)) %>%
  arrange(desc(alpha_site)) %>%
  pull(site_id)

df_plot <- df_pd %>%
  group_by(taxon.y, site) %>%
  summarize(alpha50 = median(value)) %>%
  arrange(alpha50) %>%
  ungroup() %>%
  mutate(order = row_number()) %>%
  right_join(df_pd,
             by = c("taxon.y",
                    "site"))# %>%
#mutate(site_id = factor(site_id, levels = site_order))


# figure ------------------------------------------------------------------

## by site ####
g_alpha_site <- df_plot %>%
  filter(alpha_prime != 1) %>% 
  ggplot(aes(y = factor(lbs.x, levels = rev(sort(unique(lbs.x)))),
             x = lbs.y,
             fill = value,
             label = sprintf("%.2f", value))) +
  geom_tile() +
  geom_text(size = 3) +
  facet_wrap(facets = ~site_id,
             scales = "free",
             nrow = 3,
             ncol = 3) +
  theme_classic() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(face = "italic",
                                   angle = 75,
                                   vjust = 0.5,
                                   hjust = 0.5)) +
  scale_x_discrete(labels = label_parse()) +
  scale_y_discrete(labels = label_parse()) +
  MetBrewer::scale_fill_met_c("Hiroshige", direction = -1) +
  labs(y = expression("Taxon"~italic("i")),
       x = expression("Taxon"~italic("j")),
       fill = expression(alpha[ij]))

print(g_alpha_site)

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
  geom_boxplot(aes(fill = lbs.y),
               color = grey(0.4),
               alpha = 0.75,
               outlier.colour = NA) +
  geom_jitter(height = 0.1,
              width = 0,
              size = 1,
              color = grey(0.2)) +
  geom_vline(aes(xintercept = median(value)),
             col = grey(0.5),
             linetype = "dashed") +
  geom_xsidedensity(aes(fill = lbs.y),
                    color = NA,
                    alpha = 0.8,
                    position = "stack") +
  labs(y = expression("Taxon"~italic("j")~"affecting taxon"~italic("i")),
       x = expression("Competition coefficient"~alpha[ij])) +
  guides(color = "none",
         fill = "none") +
  scale_y_discrete(labels = label_parse()) +
  MetBrewer::scale_fill_met_d("Hiroshige", direction = -1) +
  ggridges::theme_ridges() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "gray"),
        axis.line = element_line(color = "gray"))

print(g_alpha)


# export ------------------------------------------------------------------

ggsave(g_alpha_site,
       filename = here::here("figure/figure_alpha_site.pdf"),
       height = 10,
       width = 11)

ggsave(g_alpha,
       filename = here::here("figure/figure_alpha_dist.pdf"),
       height = 7,
       width = 8)
