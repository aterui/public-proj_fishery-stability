
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, runjags, foreach)
setwd(here::here("code_empirical"))
  
  
# data --------------------------------------------------------------------

## fish data ####
source("data_fmt_fishdata.R")

df_site_id <- df_fish %>% 
  distinct(river, site_id, site_id_numeric)

## stock data ####
df_stock <- read_csv("data_fmt/data_hkd_prtwsd_stock_fmt.csv") %>% 
  filter(between(year_release, 1999, 2019) & release_stage == "fry")

unstocked_river <- pull(setdiff(distinct(df_site_id, river),
                                distinct(df_stock, river)))

df_stock <- df_stock %>% 
  bind_rows(tibble(year_release = rep(1999:2019,
                                      length(unstocked_river)),
                   river = rep(unstocked_river,
                               each = length(1999:2019)),
                   abundance = 0,
                   abundance_unit = "thousand_fish",
                   release_stage = "fry")
            ) %>% 
  group_by(year_release, river) %>% 
  summarize(stock = sum(abundance) * 0.001,
            stock_unit = "million_fish") %>% 
  right_join(df_site_id, by = "river") %>% 
  pivot_wider(id_cols = c("year_release"),
              names_from = "site_id",
              values_from = "stock",
              values_fill = list(stock = 0)) %>% 
  pivot_longer(cols = -year_release,
               names_to = "site_id",
               values_to = "stock") %>% 
  left_join(df_site_id,
            by = "site_id")


# jags --------------------------------------------------------------------

## data ####
d_jags <- list(N = df_fish$abundance,
               Site = df_fish$site_id_numeric,
               Year = df_fish$year - min(df_fish$year) + 1,
               St_year = df_year$St_year,
               End_year = df_year$End_year,
               Area = df_fish$area,
               Nsample = nrow(df_fish),
               Nsite = n_distinct(df_fish$site_id),
               
               Stock = df_stock$stock,
               Year_stock = df_stock$year_release - min(df_stock$year_release) + 1,
               Site_stock = df_stock$site_id_numeric,
               Nsample_stock = nrow(df_stock))
               
## parameters ####
para <- c("log_global_r",
          "sd_r_space",
          "log_mu_r",
          "sd_r_time",
          "sd_obs",
          "b",
          "mu_b",
          "sd_b",
          "log_d",
          "cv",
          "mu",
          "sigma")

## model file ####
m <- read.jagsfile("model_ssm.R")

## mcmc setup ####

n_ad <- 100
n_iter <- 1.0E+4
n_thin <- max(3, ceiling(n_iter / 500))
n_burn <- ceiling(max(10, n_iter/2))
n_sample <- ceiling(n_iter / n_thin)

inits <- replicate(3,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (i in 1:3) inits[[i]]$.RNG.seed <- i

    
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

# cv, mu, sigma are derived parameters
# exclude from convergence check
r_hat <- filter(mcmc_summary,
                !str_detect(rownames(mcmc_summary),
                            pattern = "(^cv)|(^mu)|(^sigma)"))
max(r_hat$Rhat)

while(max(r_hat$Rhat) > 1.09) {
  post <- extend.jags(post,
                      burnin = 0,
                      sample = n_sample,
                      adapt = n_ad,
                      thin = n_thin,
                      n.sims = 3,
                      combine = TRUE)
  
  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
  r_hat <- filter(mcmc_summary,
                  !str_detect(rownames(mcmc_summary),
                              pattern = "(^cv)|(^mu)|(^sigma)"))
  print(max(r_hat$Rhat))
}

n_total_mcmc <- (post$sample / n_sample) * n_iter + n_burn
  
  
# format output -----------------------------------------------------------
  
fn_brrm <- function(x) {
  y <- lapply(str_extract_all(x, pattern = "\\[.{1,}\\]"),
              FUN = function(z) ifelse(identical(z, character(0)),
                                       NA,
                                       z))
  str_remove_all(y, pattern = "\\[|\\]")
}

param <- rownames(mcmc_summary)
param_name <- str_remove(param,
                         pattern = "\\[.{1,}\\]")

df_site <- df_fish %>% 
  group_by(site_id_numeric) %>% 
  summarize(river = unique(river),
            site = unique(site),
            site_id = unique(site_id))

est <- mcmc_summary %>% 
  as_tibble() %>% 
  mutate(n_total_mcmc = n_total_mcmc,
         n_sample = post$sample,
         n_thin = n_thin,
         n_burn = n_burn,
         param_name = param_name,
         param = param,
         character_id = fn_brrm(param)) %>% 
  separate(character_id, into = c("site_id", "year_id")) %>% 
  mutate(site_id_numeric = suppressWarnings(as.numeric(site_id)),
         year_id = as.numeric(year_id),
         year = year_id + 1998) %>% 
  select(-site_id) %>% 
  left_join(df_site, by = "site_id_numeric") %>% 
  left_join(df_fish, by = c("year",
                            "river",
                            "site",
                            "site_id",
                            "site_id_numeric"))
  
write_csv(est, "data_fmt/data_ssm_est.csv")