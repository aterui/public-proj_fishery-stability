
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# data --------------------------------------------------------------------

source("code/data_fmt_reg.R")

df_m <- df_ssm %>%
  filter(response %in% c("mu", "sigma", "n_species"),
         !(response == "n_species" & group != "all")) %>% 
  mutate(river_id = as.numeric(factor(river))) %>% 
  group_by(river) %>% 
  mutate(site0 = as.numeric(factor(site)),
         p_id = as.numeric(factor(response,
                                  levels = c("sigma", "mu", "n_species"))),
         group_id = as.numeric(factor(group))) %>% 
  ungroup() %>% 
  relocate(river, river_id, site, site0, site_id, group, group_id)

df_site <- df_m %>%
  distinct(site_id_numeric,
           river,
           river_id,
           site0,
           wsd_area,
           temp,
           ppt,
           frac_forest) %>% 
  relocate(site_id_numeric, river, river_id) %>% 
  arrange(site_id_numeric)

df_river <- df_m %>% 
  group_by(river, river_id) %>% 
  summarize(stock = unique(mean_stock),
            chr_a = unique(chr_a),
            n_site = n_distinct(site_id))


# jags setup --------------------------------------------------------------

## parameters ####
para <- c("a",
          "b",
          "sigma",
          "sigma_r",
          "beta_raw",
          "RHO",
          "w",
          "ef_stock")

## model file ####
m <- runjags::read.jagsfile("code/model_reg.R")

## mcmc setup ####
n_ad <- 100
n_iter <- 1.0E+4
n_thin <- max(3, ceiling(n_iter / 500))
n_burn <- ceiling(max(10, n_iter/2))
n_sample <- ceiling(n_iter / n_thin)
n_chain <- 4

inits <- replicate(n_chain,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (j in 1:n_chain) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 1


# jags --------------------------------------------------------------------

d_jags <- list(Y = df_m$value,
               P = df_m$p_id,
               Site = df_m$site_id_numeric,
               Group = df_m$group_id,
               Np = n_distinct(df_m$response),
               Nsample = nrow(df_m),
               Ngroup = n_distinct(df_m$group),
               
               Wsd_area = df_site$wsd_area,
               Temp = df_site$temp,
               Ppt = df_site$ppt,
               Forest = df_site$frac_forest,
               River = df_site$river_id,
               Reach = df_site$site0,
               
               Stock = df_river$stock,
               Chr_a = df_river$chr_a,
               Ngs = df_river$n_site,
               
               Nsite = nrow(df_site),
               Nriver = nrow(df_river))

## run jags ####
post <- runjags::run.jags(m$model,
                          monitor = para,
                          data = d_jags,
                          n.chains = n_chain,
                          inits = inits,
                          method = "parallel",
                          burnin = n_burn,
                          sample = n_sample,
                          adapt = n_ad,
                          thin = n_thin,
                          n.sims = n_chain,
                          module = "glm")

mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)

while(max(mcmc_summary$Rhat, na.rm = T) >= 1.1) {
  post <- runjags::extend.jags(post,
                               burnin = 0,
                               sample = n_sample,
                               adapt = n_ad,
                               thin = n_thin,
                               n.sims = n_chain,
                               combine = TRUE)

  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
}

mcmc_sample <- post$mcmc
n_total_mcmc <- (post$sample / n_sample) * n_iter + burnin

### Pr(theta > 0)
pr_po <- MCMCvis::MCMCpstr(post$mcmc,
                           func = function(x) mean(x > 0)) %>%
  unlist() %>%
  na.omit()

### output df
df_est <- as_tibble(mcmc_summary) %>%
  rename(low = '2.5%',
         median = '50%',
         high = '97.5%') %>%
  mutate(parameter = rownames(mcmc_summary),
         param_name = str_remove_all(parameter, "\\[.{1,}\\]"),
         param_id = str_extract(parameter, "\\[.{1,}\\]") %>% 
           str_remove_all("\\[|\\]"),
         pr_po = pr_po,
         n_total_mcmc = n_total_mcmc,
         n_sample = n_sample,
         n_thin = n_thin,
         n_burn = post$burnin) %>% 
  separate(param_id,
           into = c("id1", "id2", "id3"),
           convert = T,
           fill = "left") %>%
  mutate(k = ifelse(str_detect(param_name, "(a)|(b)|beta_raw|sigma"),
                    id1, NA),
         g = ifelse(str_detect(param_name, "(a)|(b)|beta_raw|sigma"),
                    id2, NA),
         p = ifelse(str_detect(param_name, "(a)|(b)|beta_raw|sigma"),
                    id3, NA),
         river_id = ifelse(param_name == "w",
                           id2, NA),
         site0 = ifelse(param_name == "w",
                        id3, NA),
         site_id_numeric = ifelse(param_name == "ef_stock",
                                  id3,
                                  NA),
         response = case_when(p == 1 ~ "sigma",
                              p == 2 ~ "mu",
                              p == 3 ~ "species_richness",
                              p == 4 ~ "cv"),
         group = case_when(g == 1 ~ "all",
                           g == 2 ~ "masu_salmon",
                           g == 3 ~ "other")) %>% 
  filter(!(response %in% c("species_richness", "cv") & group != "all"))

# export ------------------------------------------------------------------

saveRDS(df_est, here::here("output/summary_reg.rds"))
saveRDS(mcmc_sample, file = here::here("output/mcmc_reg.rds"))
MCMCvis::MCMCtrace(mcmc_sample,
                   filename = "output/mcmc_trace_reg")

