
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

pacman::p_load(here,
               cdyns,
               tidyverse,
               foreach,
               doParallel,
               doSNOW)

cl <- makeCluster(detectCores())
registerDoSNOW(cl)

# set parameters ----------------------------------------------------------

df_param <- expand.grid(n_timestep = 1000,
                        n_warmup = 200,
                        n_burnin = 400,
                        n_species = c(10, 20),
                        k = 100,
                        r_type = "constant",
                        r1 = c(0.5, 1, 2),
                        r_min = 0.5,
                        r_max = c(1, 2),
                        sd_env = c(0.1, 0.5),
                        phi = c(0.8, 1),
                        int_type = "random",
                        alpha = c(0.1, 0.3, 0.5),
                        model = "ricker",
                        seed = 5,
                        seed_interval = 10) %>% 
  filter(r_max >= r_min)

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
                  .packages = c("foreach", "dplyr", "cdyns"),
                  .options.snow = opts) %dopar% {
                    
                    df_set <- foreach(j = seq_len(n_rep),
                                      .combine = "bind_rows") %do% {
                                        
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
                                                       seed = x$seed)
                                        
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
                                        
                                        var_sum <- sum(diag(dyn$vcov_matrix))
                                        cov_sum <- sum(dyn$vcov_matrix[upper.tri(dyn$vcov_matrix)]) * 2
                                        
                                        n_sp_persist <- dyn$df_dyn %>% 
                                          filter(density > 0.01,
                                                 timestep == max(timestep)) %>% 
                                          n_distinct(.$species)
                                        
                                        df <- tibble(n_rep = j,
                                                     x,
                                                     stock = stock[j],
                                                     dyn_summary,
                                                     n_sp_persist = n_sp_persist,
                                                     var_sum = var_sum,
                                                     cov_sum = cov_sum)
                                        
                                        return(df)
                                      }
                    return(df_set)
                  }

# return ------------------------------------------------------------------

stopCluster(cl)

write_csv(result,
          "result/result_ricker.csv")
