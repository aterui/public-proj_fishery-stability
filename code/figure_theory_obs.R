
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)

# data --------------------------------------------------------------------

suppressMessages(source(here::here("code/figure_theory.R")))
suppressMessages(source(here::here("code/figure_obs_stock.R")))
suppressMessages(source(here::here("code/figure_map.R")))

layout <- "
ABC
ABD
ABE
ABF
"

g <- (g_theory + ggtitle("Theoretical prediction")) + 
  (g_obs + ggtitle("Empirical observation")) +
  g_hkd + 
  g_example + 
  g_masu + 
  guide_area() +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave(g,
       filename = here::here("figure/figure_theory_obs.pdf"),
       width = 10,
       height = 10)

