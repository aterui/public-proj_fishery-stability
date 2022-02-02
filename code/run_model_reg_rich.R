
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(foreach,
               tidyverse)


# jags setup --------------------------------------------------------------

source("code/data_fmt_analysis.R")
df_m <- list_ssm$all %>% 
  filter(response == "mu")

## parameters ####
para <- c("b",
          "a",
          "sigma",
          "sigma_r",
          "b_raw")

## model file ####
m <- runjags::read.jagsfile("code/model_reg_rich.R")

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

for (j in 1:3) inits[[j]]$.RNG.seed <- (j - 1) * 100 + 1


# jags --------------------------------------------------------------------

df_site <- df_m %>%
  mutate(river_id = as.numeric(factor(river)))

df_river <- df_m %>% 
  mutate(river_id = as.numeric(factor(river))) %>% 
  group_by(river, river_id) %>% 
  summarize(stock = unique(mean_stock),
            chr_a = unique(chr_a))

d_jags <- list(Y = df_site$n_species,
               Wsd_area = df_site$wsd_area,
               Temp = df_site$temp,
               Ppt = df_site$ppt,
               Forest = df_site$frac_forest,
               River = df_site$river_id,
               
               Stock = df_river$stock,
               Chr_a = df_river$chr_a,
               
               Nsite = nrow(df_site),
               Nriver = nrow(df_river))


## run jags ####
post <- runjags::run.jags(m$model,
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

while(max(mcmc_summary$Rhat) > 1.09) {
  post <- runjags::extend.jags(post,
                               burnin = 0,
                               sample = n_sample,
                               adapt = n_ad,
                               thin = n_thin,
                               n.sims = 3,
                               combine = TRUE)
  
  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
}

mcmc_sample <- post$mcmc
save(mcmc_sample, file = "result/mcmc_all_richness.RData")

## output ####
n_total_mcmc <- (post$sample / n_sample) * n_iter + n_burn

re <- as_tibble(mcmc_summary) %>% 
  rename(low = '2.5%',
         median = '50%',
         high = '97.5%') %>% 
  mutate(parameter = rownames(mcmc_summary),
         prob_positive = unlist(MCMCvis::MCMCpstr(post$mcmc,
                                                  func = function(x) mean(x > 0))),
         response = "richness",
         n_total_mcmc = n_total_mcmc,
         n_sample = n_sample,
         n_thin = n_thin,
         n_burn = n_burn)

out <- relocate(re, c(response, parameter))

# export ------------------------------------------------------------------

write_csv(out, "result/reg_rich.csv")
