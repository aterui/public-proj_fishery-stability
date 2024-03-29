
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))


# data --------------------------------------------------------------------

suppressMessages(source(here::here("code/figure_theory.R")))
suppressMessages(source(here::here("code/figure_obs_stock.R")))
suppressMessages(source(here::here("code/figure_obs_map.R")))

layout <- "
ABC
ABD
ABE
ABF
"

g <- (g_theory + ggtitle("Theoretical prediction")) + 
  (g_obs + ggtitle("Empirical observation")) +
  g_hkd + 
  #g_example + 
  g_masu + 
  plot_spacer() +
  guide_area() +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave(g,
       filename = here::here("output/figure_theory_obs.pdf"),
       width = 10,
       height = 10)

