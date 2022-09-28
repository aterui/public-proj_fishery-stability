
# setup -------------------------------------------------------------------

rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/set_functions.R"))

knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      eval.after = "fig.cap") # to embed codes in fig.cap

options(knitr.kable.NA = '')


# theory ------------------------------------------------------------------

cols <- c(
  "n_species",
  "k",
  "r_type",
  "r1",
  "r_min",
  "r_max",
  "sd_env",
  "phi",
  "int_type",
  "alpha",
  "model",
  "seed")

list_sim <- list(readRDS(here::here("output/result_ricker_2sp.rds")),
                 readRDS(here::here("output/result_ricker.rds")))

list_param <- list_sim %>% 
  lapply(FUN = function(x) {
    n_sim <- x %>% 
      mutate(n_sim = n_warmup + n_burnin + n_timestep) %>% 
      pull(n_sim) %>% 
      unique()
    
    param_set <- x %>% 
      dplyr::select(which(colnames(x) %in% cols)) %>% 
      distinct()
    
    attr(param_set, "n_sim") <- n_sim
    return(param_set)
  })


# empirical ---------------------------------------------------------------

## raw data
source(here::here("code/data_fmt_fishdata.R"))
source(here::here("code/data_fmt_reg.R"))

df_area <- d0 %>% 
  group_by(site_id, year) %>% 
  summarize(area = unique(area))

## ssm estimate
df_bp <- list.files(path = here::here("output"),
                    full.names = T,
                    pattern = "summary_ssm_ar") %>% 
  readRDS() %>% 
  filter(param == "bp_value") %>% 
  dplyr::select(mean)

## mcmc sample size

df_mcmc <- lapply(c("summary_ssm_ar", "summary_reg", "summary_multi_ricker"),
                  function(x) {
                    list.files(path = here::here("output"),
                               full.names = T,
                               pattern = x) %>% 
                      readRDS() %>% 
                      ungroup() %>% 
                      dplyr::select(n_total_mcmc,
                                    n_sample,
                                    n_thin,
                                    n_burn) %>% 
                      distinct() %>% 
                      mutate(model = str_extract(x, "ssm|reg|ricker"))
                  }) %>% 
  bind_rows()

## release data
source(here::here("code/data_fmt_stock.R"))

df_stock_stage <- df_stock %>% 
  group_by(release_stage) %>% 
  summarize(stock = sum(abundance)) %>% 
  mutate(ratio = stock / max(stock),
         prop = stock / sum(stock))
