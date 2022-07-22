
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               runjags,
               foreach)

fn_brrm <- function(x) {
  y <- lapply(str_extract_all(x, pattern = "\\[.{1,}\\]"),
              FUN = function(z) ifelse(identical(z, character(0)),
                                       NA,
                                       z))
  str_remove_all(y, pattern = "\\[|\\]")
}

source(here::here("code/data_fmt_fishdata.R"))

# common setup ------------------------------------------------------------

## mcmc setup ####
n_ad <- 100
n_iter <- 5.0E+3
n_thin <- max(3, ceiling(n_iter / 250))
n_burn <- ceiling(max(10, n_iter/2))
n_sample <- ceiling(n_iter / n_thin)

inits <- replicate(3,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (j in 1:3) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 1

## parameters ####
para <- c("p",
          "log_r",
          "sigma",
          "alpha")

# jags --------------------------------------------------------------------

## data screening
unique_site <- unique(df_complete$site_id)

df_subset <- df_complete %>% 
  dplyr::filter(site_id == unique_site[1]) %>% 
  mutate(p = ifelse(abundance > 0, 1, 0)) %>% 
  group_by(taxon) %>% 
  summarize(freq = sum(p, na.rm = T),
            site_id = unique(site_id)) %>% 
  filter(freq > 4) %>% 
  select(taxon, site_id) %>% 
  left_join(df_complete,
            by = c("taxon", "site_id"))

## data for jags ####
d_jags <- list(N = df_subset$abundance,
               Year = df_subset$year - min(df_subset$year) + 1,
               Species = as.numeric(factor(df_subset$taxon)),
               Area = df_subset$area,
               Nsample = nrow(df_subset),
               Nyr = n_distinct(df_subset$year),
               Nsp = n_distinct(df_subset$taxon),
               Q = 1,
               W = diag(n_distinct(df_subset$taxon)))

## model file ####
m <- read.jagsfile("code/model_ssm_sparse0.R")

## run jags ####
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

mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc) %>% 
  mutate(param = rownames(.)) %>% 
  tibble() %>% 
  rename(median = `50%`,
         lower = `2.5%`,
         upper = `97.5%`)

print(max(mcmc_summary$Rhat, na.rm = T))

# while(max(mcmc_summary$Rhat, na.rm = T) > 1.09) {
#   post <- extend.jags(post,
#                       burnin = 0,
#                       sample = n_sample,
#                       adapt = n_ad,
#                       thin = n_thin,
#                       n.sims = 3,
#                       combine = TRUE)
# 
#   mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
#   print(max(mcmc_summary$Rhat))
# }

n_total_mcmc <- (post$sample / n_sample) * n_iter + n_burn

MCMCvis::MCMCtrace(post$mcmc,
                   filename = "result/MCMCtrace_sparse0")


# waic --------------------------------------------------------------------



# output ------------------------------------------------------------------

m_alpha <- mcmc_summary %>% 
  filter(str_detect(.$param, "alpha")) %>% 
  pull(median) %>% 
  matrix(n_distinct(df_subset$taxon), n_distinct(df_subset$taxon)) %>% 
  print()

log_r_hat <- mcmc_summary %>% 
  filter(str_detect(.$param, "log_r")) %>% 
  pull(median)
  
alpha_hat <- diag(m_alpha)

df_est <- tibble(log_r_hat, alpha_hat, taxon = unique(df_subset$taxon))

df_test <- df_subset %>% 
  mutate(density = abundance / area) %>% 
  group_by(taxon) %>% 
  mutate(log_r = c(diff(log(density)), NA),
         log_r = ifelse(log_r %in% c(Inf, -Inf), NA, log_r)) %>% 
  ungroup() %>% 
  left_join(df_est,
            by = "taxon")

df_test %>% 
  drop_na(log_r) %>% 
  ggplot(aes(x = density,
             y = log_r,
             color = taxon)) +
  geom_point() +
  facet_wrap(facets = ~taxon,
             scales = "free_y") +
  geom_abline(aes(slope = -alpha_hat,
                  intercept = log_r_hat,
                  color = taxon)) +
  theme_bw()
  
