
# setup -------------------------------------------------------------------

rm(list = ls())
source("code/library.R")

cl <- makeCluster(detectCores() - 1)
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

df_param <- expand.grid(n_timestep = 1000,
                        n_warmup = 100,
                        n_burnin = 400,
                        n_species = 10,
                        k = c(100, 400),
                        r_type = "constant",
                        r1 = seq(0.5, 3.5, by = 1),
                        r_min = 0.5,
                        r_max = 2.5,
                        sd_env = 0.5,
                        phi = c(0.5, 1),
                        int_type = "random",
                        alpha = c(0.1, 0.5),
                        model = "ricker",
                        seed = 5,
                        seed_interval = 10,
                        extinct = 0.01)

n_rep <- 1000
stock <- seq(0, 500, length = n_rep)

# run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = nrow(df_param), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach", "dplyr", "cdyns"),
                  .options.snow = opts) %dopar% {
                    
                    df_set <- foreach(j = seq_len(n_rep),
                                      .combine = "bind_rows") %do% {
                                        
                                        ## simulation
                                        dyn <- cdynsim(n_timestep = x$n_timestep,
                                                       n_warmup = x$n_warmup,
                                                       n_burnin = x$n_burnin,
                                                       n_species = x$n_species,
                                                       k = x$k,
                                                       r_type = x$r_type,
                                                       r = c(x$r1, runif(n = x$n_species - 1,
                                                                         x$r_min, x$r_max)),
                                                       sd_env = x$sd_env,
                                                       stock = stock[j],
                                                       phi = x$phi,
                                                       int_type = x$int_type,
                                                       alpha = x$alpha,
                                                       model = x$model,
                                                       seed = x$seed,
                                                       extinct = x$extinct)
                                        
                                        ## summarize temporal dynamics
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
                                        
                                        ## persisting species
                                        n_sp_last <- dyn$df_dyn %>% 
                                          filter(density > 0,
                                                 timestep == max(timestep)) %>% 
                                          n_distinct(.$species)
                                        
                                        ## output dataframe
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

saveRDS(result, file = "result/result_ricker.rds")
