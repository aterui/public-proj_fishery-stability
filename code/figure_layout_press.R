
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))


# data --------------------------------------------------------------------

suppressMessages(source(here::here("code/figure_theory_press.R")))
suppressMessages(source(here::here("code/figure_obs_stock_press.R")))

layout <- "
AB
AB
"

g <- (g_theory + ggtitle("シミュレーション")) + 
  (g_obs + ggtitle("実証")) +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(tag_levels = 'A')

ggsave(g,
       filename = here::here("output/figure_press.png"),
       width = 10,
       height = 7)