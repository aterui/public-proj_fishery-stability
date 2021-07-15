
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

d0 <- read_csv("data_fmt/data_ssm_est.csv") %>% 
  filter(param_name == "cv") %>% 
  select(cv = '50%',
         river,
         site,
         site_id)


df_env <- read_csv("data_fmt/data_env_fmt.csv")
df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(river) %>% 
  summarize(mean_stock = sum(abundance) / (2019 - 1999 + 1))

df <- d0 %>% 
  left_join(df_env, by = "river") %>% 
  left_join(df_stock, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock))


# plot --------------------------------------------------------------------

df %>% 
  ggplot() +
  geom_point(aes(x = mean_stock, y = cv)) +
  scale_y_continuous(trans = "log")


fit <- lme4::lmer(log(cv) ~ scale(mean_stock) +
                    scale(frac_forest) + 
                    scale(mean_ppt) + 
                    scale(mean_temp) + 
                    (1 | river),
                  df)
summary(fit)
confint(fit)
