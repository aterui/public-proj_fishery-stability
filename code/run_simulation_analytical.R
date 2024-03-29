
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

n_core <- detectCores() - 1
cl <- makeCluster(n_core)
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

df_param <- expand.grid(n_timestep = 1000,
                        n_warmup = 100,
                        n_burnin = 400,
                        n_species = 2,
                        k = seq(50, 500, length = 20),
                        r_type = "constant",
                        r1 = seq(0.5, 3.5, length = 20),
                        sd_env = c(0, 0.5),
                        phi = 1,
                        int_type = "constant",
                        alpha = c(0.25, 0.5),
                        model = "ricker",
                        seed = 50,
                        seed_interval = 0,
                        extinct = 0) %>% 
  mutate(param_id = seq_len(nrow(.)))

n_rep <- 100
stock <- seq(0, 500, length = n_rep)

# run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = nrow(df_param), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach",
                                "dplyr",
                                "cdyns"),
                  .options.snow = opts) %dopar% {
                    
                    df_set <- foreach(j = seq_len(n_rep),
                                      .combine = "bind_rows") %do% {
                                        
                                        ## for reproducibility
                                        set.seed((x$param_id - 1) * n_rep + j)
                                        
                                        ## simulation
                                        dyn <- cdynsim(n_timestep = x$n_timestep,
                                                       n_warmup = x$n_warmup,
                                                       n_burnin = x$n_burnin,
                                                       n_species = x$n_species,
                                                       k = x$k,
                                                       r_type = x$r_type,
                                                       r = x$r1,
                                                       sd_env = x$sd_env,
                                                       stock = stock[j],
                                                       phi = x$phi,
                                                       int_type = x$int_type,
                                                       alpha = x$alpha,
                                                       model = x$model,
                                                       seed = x$seed,
                                                       extinct = x$extinct)
                                        
                                        dyn_summary <- dyn$df_dyn %>% 
                                          mutate(status = case_when(species == 1 ~ "enhanced",
                                                                    species != 1 ~ "unenhanced")) %>% 
                                          group_by(status,
                                                   timestep) %>% 
                                          summarize(summed_density = sum(density)) %>% 
                                          summarize(mean_density = mean(summed_density),
                                                    sd_density = sd(summed_density)) %>% 
                                          bind_rows(tibble(status = "all",
                                                           dyn$df_community))
                                        
                                        n_sp_last <- dyn$df_dyn %>% 
                                          filter(density > 0,
                                                 timestep == max(timestep)) %>% 
                                          n_distinct(.$species)
                                        
                                        df <- tibble(n_rep = j,
                                                     x,
                                                     stock = stock[j],
                                                     dyn_summary,
                                                     n_sp_last = n_sp_last)
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }

stopCluster(cl)

# return ------------------------------------------------------------------

saveRDS(result, file = "output/result_ricker_2sp.rds")
