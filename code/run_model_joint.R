
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/function_set.R"))


# common setup ------------------------------------------------------------

## fish data ####
## "data_fmt_stock.R" calls `df_fish` through "data_fmt_fishdata.R"
source("code/data_fmt_stock.R")
Order <- 3
model <- "joint"

## mcmc setup ####
n_ad <- 100
n_iter <- 1E+4
n_thin <- max(3, ceiling(n_iter / 250))
n_burn <- ceiling(max(10, n_iter/2))
n_chain <- 4
n_sample <- ceiling(n_iter / n_thin)
sj <- F

inits <- replicate(n_chain,
                   list(.RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA),
                   simplify = FALSE)

for (j in 1:n_chain) inits[[j]]$.RNG.seed <- (j - 1) * 10 + 2

## model file ####
m <- read.jagsfile(paste0("code/model_", model, ".R"))

## parameters ####
para <- c("bp_value",
          "nu0",
          "mu_xi",
          "OMEGA",
          "sd_r_time",
          "sd_obs",
          "mu_b",
          "b",
          "sd_b",
          "log_d_prime")

# jags --------------------------------------------------------------------

## data for jags ####
df_subset <- df_fish %>% 
  mutate(group = factor(group, levels = c("masu_salmon",
                                          "other",
                                          "all")),
         group_numeric = as.numeric(group)) %>% 
  filter(group != "all")

df_t1 <- df_subset %>%
  group_by(site_id_numeric, group_numeric) %>%
  summarize(log_mu_d = log(mean(density)),
            log_max_d = log(max(density))) %>%
  ungroup()

d_jags <- list(N = df_subset$abundance,
               Group = df_subset$group_numeric,
               Site = df_subset$site_id_numeric,
               Year = df_subset$year - min(df_subset$year) + 1,
               St_year = df_year$St_year,
               End_year = df_year$End_year,
               Area = df_subset$area,
               Ng = n_distinct(df_subset$group),
               Nsample = nrow(df_subset),
               Nsite = n_distinct(df_subset$site_id),
               
               # Psi = 0 for fish group "other" because no stocking effect would be expected
               Psi = c(1, 0),
               Stock = df_fry$stock,
               Year_stock = df_fry$year_release - min(df_fry$year_release) + 1,
               Site_stock = df_fry$site_id_numeric,
               Nsample_stock = nrow(df_fry),
               
               # order of auto-regressive process
               Q = Order,

               # max for initial densities
               N_t1 = nrow(df_t1),
               Log_d1 = df_t1$log_mu_d,
               Log_max_d = df_t1$log_max_d,
               Site_t1 = df_t1$site_id_numeric,
               Group_t1 = df_t1$group_numeric)

## run jags ####
post <- run.jags(m$model,
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
                 module = "glm",
                 silent.jags = sj)

mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
print(paste(max(mcmc_summary$Rhat, na.rm = T),
            rownames(mcmc_summary)[which.max(mcmc_summary$Rhat)]))

while(max(mcmc_summary$Rhat, na.rm = T) >= 1.1) {
  post <- extend.jags(post,
                      burnin = 0,
                      sample = n_sample,
                      adapt = n_ad,
                      thin = n_thin,
                      n.sims = n_chain,
                      combine = TRUE,
                      silent.jags = sj)

  mcmc_summary <- MCMCvis::MCMCsummary(post$mcmc)
  print(paste(max(mcmc_summary$Rhat, na.rm = T),
              rownames(mcmc_summary)[which.max(mcmc_summary$Rhat)]))
}


## format output ####
n_total_mcmc <- (post$sample / n_sample) * n_iter + post$burnin

df_site <- df_fish %>% 
  distinct(site_id_numeric,
           river,
           site,
           site_id)

est <- mcmc_summary %>% 
  mutate(n_total_mcmc = n_total_mcmc,
         n_sample = post$sample,
         n_thin = n_thin,
         n_burn = n_burn,
         param = rownames(.),
         param_name = str_remove(param,
                                 pattern = "\\[.{1,}\\]"),
         character_id = fn_brrm(param)) %>% 
  as_tibble() %>% 
  separate(character_id,
           into = c("site_id_numeric",
                    "year_id_numeric",
                    "group_numeric"),
           convert = TRUE,
           fill = "right") %>%
  group_by(param_name) %>% 
  mutate(n_site_id = n_distinct(site_id_numeric),
         n_year_id = n_distinct(year_id_numeric),
         group_numeric = ifelse(n_site_id == 2, site_id_numeric, group_numeric),
         group_numeric = ifelse(n_year_id == 2, year_id_numeric, group_numeric),
         site_id_numeric = ifelse(n_site_id == 2, NA, site_id_numeric),
         year_id_numeric = ifelse(n_year_id == 2, NA, year_id_numeric)) %>% 
  select(-n_site_id, -n_year_id) %>% 
  mutate(year = year_id_numeric + 1998,
         group = case_when(group_numeric == 1 ~ "masu_salmon",
                           group_numeric == 2 ~ "other",
                           group_numeric == 3 ~ "all")) %>% 
  left_join(df_site, by = "site_id_numeric") %>% 
  left_join(df_fish, by = c("year",
                            "river",
                            "site",
                            "site_id",
                            "site_id_numeric",
                            "group")) %>% 
  relocate(param_name,
           param,
           site_id,
           site_id_numeric,
           year_id_numeric) %>% 
  mutate(year_id_numeric = ifelse(param_name == "theta",
                                  NA,
                                  year_id_numeric))


# export ------------------------------------------------------------------

saveRDS(est, here::here(paste0("data_fmt/data_", model, Order, ".rds")))

saveRDS(post,
        file = here::here(paste0("result/post_",
                                 model,
                                 Order,
                                 ".rds")))

MCMCvis::MCMCtrace(post$mcmc,
                   params = para[-which(para %in% c("log_d_prime", "b", "loglik"))],
                   filename = paste0("result/mcmc_trace_",
                                     model,
                                     Order))
