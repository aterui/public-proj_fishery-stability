
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)

# data --------------------------------------------------------------------

source("code/figure_theory.R")
source("code/figure_cv_stock.R")
source("code/figure_map.R")

g1 <- (g_theory + ggtitle("Theoretical prediction") |
  g_obs + ggtitle("Empirical observation") + theme(legend.position = "bottom"))
  
g2 <- g_example / g_hkd / g_masu

g <- (g1 | g2) + plot_annotation(tag_levels = 'A')

ggsave(g,
       filename = here::here("figure/figure_theory_obs.pdf"),
       width = 9.5,
       height = 9)
