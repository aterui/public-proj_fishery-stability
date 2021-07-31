
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               MuMIn)
setwd(here::here("code_empirical"))


# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

df_sp <- d0 %>% 
  filter(abundance > 0) %>% 
  group_by(river, site, site_id) %>% 
  summarize(n_species = n_distinct(taxon))

df_ssm <- read_csv("data_fmt/data_ssm_est.csv") %>% 
  filter(param_name %in% c("cv", "mu", "sigma")) %>% 
  rename(median = '50%',
         high = '97.5%',
         low = '2.5%') %>% 
  select(river,
         site,
         site_id,
         param_name,
         median,
         high,
         low)

df_env <- read_csv("data_fmt/data_env_fmt.csv")

df_ocean <- read_csv("data_fmt/data_ocean_fmt.csv")

df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019)) %>% 
  group_by(river) %>% 
  summarize(mean_stock = sum(abundance) / (2019 - 1999 + 1),
            sd_stock = sqrt(sum((abundance - mean_stock)^2) / (2019 - 1999)))

df_m <- df_ssm %>% 
  left_join(df_env, by = c("river", "site")) %>% 
  left_join(df_sp, by = c("river", "site", "site_id")) %>% 
  left_join(df_stock, by = "river") %>% 
  left_join(df_ocean, by = "river") %>% 
  mutate(mean_stock = ifelse(is.na(mean_stock), 0, mean_stock),
         sd_stock = ifelse(is.na(sd_stock), 0, sd_stock))


# analysis ----------------------------------------------------------------

dat <- df_m %>% 
  filter(param_name == "cv")

fit <- lme4::lmer(median ~ scale(n_species) +
                           scale(mean_stock) +
                           scale(chr_a) +
                           scale(area) + 
                           scale(temp) + 
                           scale(frac_forest) + 
                           scale(ppt) + 
                           (1 | river),
                  REML = FALSE,
                  data = dat)

summary(fit)
confint(fit)

options(na.action = "na.fail")
mavg <- dredge(fit, rank = AICc) %>% 
  get.models(delta < 2) %>%
  model.avg(m_subset) %>% 
  summary()


# plot --------------------------------------------------------------------

df_m %>% 
  group_by(river, param_name) %>% 
  summarize(median = mean(median),
            mean_stock = unique(mean_stock),
            sd_stock = unique(sd_stock),
            chr_a = unique(chr_a),
            sst = unique(sst)) %>% 
  ggplot() +
  geom_point(aes(x = mean_stock, y = median)) +
  facet_wrap(facets = ~ param_name,
             ncol = 3)# + 
#scale_y_continuous(trans = "log")

