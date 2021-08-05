
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(here,
               tidyverse,
               foreach,
               doParallel,
               doSNOW)
setwd(here::here("code_theory"))
source("function_sim.R")

cl <- makeCluster(detectCores())
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

df_param <- expand.grid(n_timestep = 1000,
                        n_warmup = 200,
                        n_burnin = 200,
                        n_species = 10,
                        k = 100,
                        r_type = "random",
                        r_min = 0.1,
                        r_max = 2,
                        sd_env = 0.5,
                        phi = 0.6,
                        int_type = "random",
                        alpha_min = 0,
                        alpha_max = c(0.5, 1),
                        model = "ricker",
                        seed = 5)

n_rep <- 1000
repeat {
  stock <- round(runif(n_rep, min = 0, max = 500))
  if(min(stock) == 0 & max(stock) == 500) break    
}

# run simulation ----------------------------------------------------------

pb <- txtProgressBar(max = nrow(df_param), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

result <- foreach(x = iter(df_param, by = 'row'),
                  .combine = "bind_rows",
                  .packages = c("foreach", "dplyr"),
                  .options.snow = opts) %dopar% {
                    
                    df_set <- foreach(j = seq_len(n_rep),
                                      .combine = "bind_rows") %do% {
                                        
                                        dyn <- dynsim(n_timestep = x$n_timestep,
                                                      n_warmup = x$n_warmup,
                                                      n_burnin = x$n_burnin,
                                                      n_species = x$n_species,
                                                      k = x$k,
                                                      r_type = x$r_type,
                                                      r_min = x$r_min,
                                                      r_max = x$r_max,
                                                      sd_env = x$sd_env,
                                                      stock = stock[j],
                                                      phi = x$phi,
                                                      int_type = x$int_type,
                                                      alpha_min = x$alpha_min,
                                                      alpha_max = x$alpha_max,
                                                      model = x$model,
                                                      seed = x$seed)
                                        
                                        df <- tibble(n_rep = j,
                                                     x,
                                                     stock = stock[j],
                                                     dyn$df_community)
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }

# return ------------------------------------------------------------------

stopCluster(cl)

write_csv(result,
          "result/result_ricker.csv")