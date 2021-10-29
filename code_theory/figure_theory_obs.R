
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse,
               patchwork)
setwd(here::here())

# data --------------------------------------------------------------------

source("code_theory/figure_theory.R")
source("code_empirical/figure_cv_stock.R")
source("code_empirical/figure_coef.R")

g <- g_theory + ggtitle("Theoretical prediction") +
  g_obs + ggtitle("Empirical observation") + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect")

ggsave(g,
       filename = "document_output/figure02.pdf",
       width = 8,
       height = 9)
