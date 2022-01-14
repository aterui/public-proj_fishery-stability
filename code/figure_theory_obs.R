
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)

setwd(here::here())

# data --------------------------------------------------------------------

source("code/figure_theory.R")
source("code/figure_cv_stock.R")

g <- g_theory + ggtitle("Theoretical prediction") +
  g_obs + ggtitle("Empirical observation") + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect")

ggsave(g,
       filename = here::here("figure/figure_theory_obs.pdf"),
       width = 8,
       height = 9)
