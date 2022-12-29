
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))
source(here::here("code/data_fmt_reg.R"))


# plot --------------------------------------------------------------------

df_plot <- df_ssm %>% 
  pivot_longer(cols = c(starts_with("frac"),
                        temp,
                        ppt,
                        sd_elev,
                        wsd_area,
                        n_obs,
                        mean_stock),
               names_to = "x_name",
               values_to = "x")

## all
df_plot %>% 
  filter(group == "all") %>% 
  ggplot(aes(x = x,
             y = value)) +
  geom_point() +
  facet_grid(rows = vars(response),
             cols = vars(x_name),
             scales = "free") +
  scale_y_continuous(trans = "log10")

## masu_salmon
df_plot %>% 
  filter(group == "masu_salmon",
         response != "n_species") %>% 
  ggplot(aes(x = x,
             y = value)) +
  geom_point() +
  facet_grid(rows = vars(response),
             cols = vars(x_name),
             scales = "free") +
  scale_y_continuous(trans = "log10")

## other
df_plot %>% 
  filter(group == "other",
         response != "n_species") %>% 
  ggplot(aes(x = x,
             y = value)) +
  geom_point() +
  facet_grid(rows = vars(response),
             cols = vars(x_name),
             scales = "free") +
  scale_y_continuous(trans = "log10")
