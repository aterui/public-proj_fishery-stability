
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, runjags, foreach)
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

Nsite <- df %>% 
  group_by(river) %>% 
  summarize(n_site = n_distinct(site_id)) %>% 
  pull(n_site)

Site <- df %>% 
  group_by(river) %>% 
  summarize(site_cor = as.numeric(factor(site))) %>% 
  pull(site_cor)

d_jags <- list(Y = log(df$cv),
               Stock = df$mean_stock,
               River = as.numeric(factor(df$river)),
               Site = Site,
               Nsample = nrow(df),
               Nriver = n_distinct(df$river),
               Nsite = Nsite)

para <- c("w_stock",
          "b",
          "p")

m <- read.jagsfile("model_lmer.R")


# mcmc setup --------------------------------------------------------------

n_ad <- 1000
n_iter <- 1.0E+4
n_thin <- max(3, ceiling(n_iter / 500))
n_burn <- ceiling(max(10, n_iter/2))
n_sample <- ceiling(n_iter / n_thin)

inits <- replicate(3,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (i in 1:3) inits[[i]]$.RNG.seed <- i + 1


# run jags ----------------------------------------------------------------

post <- run.jags(m$model,
                 monitor = para,
                 data = d_jags,
                 n.chains = 3,
                 inits = inits,
                 method = "parallel",
                 burnin = n_burn,
                 sample = n_sample,
                 adapt = n_ad,
                 thin = n_thin,
                 n.sims = 3,
                 module = "glm")

mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
max(mcmc_summary$Rhat)


# plot --------------------------------------------------------------------

df %>% 
  ggplot() +
  geom_point(aes(x = mcmc_summary$mean[1:nrow(df)],
                 y = cv)) +
  scale_y_continuous(trans = "log")

