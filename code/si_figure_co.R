
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))


# data --------------------------------------------------------------------

source(here::here("code/data_fmt_fishdata.R"))

m_co <- df_selected %>% 
  group_by(taxon, site_id) %>% 
  summarize(count = sum(abundance)) %>% 
  ungroup() %>% 
  mutate(psi = ifelse(count > 0, 1, 0)) %>% 
  dplyr::select(-count) %>% 
  pivot_wider(names_from = taxon,
              values_from = psi,
              names_prefix = "sp_") %>% 
  dplyr::select(starts_with("sp_")) %>% 
  data.matrix() %>% 
  crossprod()

m_co[upper.tri(m_co)] <- NA
m_cop <- m_co / n_distinct(df_selected$site_id)

df_cop <- m_cop %>% 
  as_tibble() %>% 
  mutate(species_1 = colnames(.)) %>% 
  pivot_longer(cols = !starts_with("species"),
               names_to = "species_2",
               values_to = "p") %>% 
  drop_na(p) %>% 
  mutate(across(.col = where(is.character),
                .fns = function(x) str_remove(x, "sp_") %>% 
                  spabb(sep = "_"))) %>% 
  mutate(lbs1 = ifelse(test = str_detect(species_1, "\\sspp\\."),
                       yes = paste0('italic("',
                                    str_remove(species_1, "\\sspp\\."),
                                    '")~spp.'),
                       no = paste0('italic("', species_1, '")')),
         lbs2 = ifelse(test = str_detect(species_2, "\\sspp\\."),
                       yes = paste0('italic("',
                                    str_remove(species_2, "\\sspp\\."),
                                    '")~spp.'),
                       no = paste0('italic("', species_2, '")')))

lbs1_level <- df_cop %>% 
  group_by(lbs1) %>% 
  tally() %>% 
  arrange(n) %>% 
  pull(lbs1)

lbs2_level <- df_cop %>% 
  group_by(lbs2) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  pull(lbs2)

g_co <- df_cop %>% 
  ggplot(aes(y = factor(lbs1, levels = rev(lbs1_level)),
             x = factor(lbs2, levels = lbs2_level),
             fill = p)) +
  geom_tile(color = grey(0, 0),
            alpha = 0.8) +
  geom_text(aes(label = round(p, 2)),
            size = 2) +
  scale_x_discrete(labels = label_parse()) +
  scale_y_discrete(labels = label_parse()) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(face = "italic",
                                   angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5)) +
  labs(y = expression("Taxon"~italic("i")),
       x = expression("Taxon"~italic("j")),
       fill = "Proportion") +
  MetBrewer::scale_fill_met_c("Hiroshige", direction = -1)

ggsave(g_co,
       filename = here::here("output/figure_co.pdf"),
       height = 6,
       width = 7)

  
