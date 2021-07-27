
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse, runjags, foreach)
setwd(here::here("code_empirical"))
  
  
# data --------------------------------------------------------------------

source("data_fmt_fishdata.R")

d_jags <- list(N = df_fish$abundance,
               Site = df_fish$site_id_numeric,
               Year = df_fish$year - min(df_fish$year) + 1,
               St_year = df_year$St_year,
               End_year = df_year$End_year,
               Area = df_fish$area,
               Nsample = nrow(df_fish),
               Nsite = n_distinct(df_fish$site_id))
               
para <- c("log_global_r",
          "sd_r_space",
          "log_mu_r",
          "sd_r_time",
          "sd_obs",
          "log_d",
          "cv",
          "mu",
          "sigma")

m <- read.jagsfile("model_ssm.R")


# mcmc setup --------------------------------------------------------------

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
  left_join(dat_site, by = "site_id_numeric") %>% 
  left_join(dat, by = c("year",
                        "river",
                        "site",
                        "site_id",
                        "site_id_numeric"))
  
write_csv(est, "data_fmt/data_ssm_est.csv")