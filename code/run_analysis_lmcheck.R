
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

## raw data for cv, mean, sd
suppressMessages(source(here::here("code/data_fmt_reg.R")))

## regression data
df_reg <- list.files(path = here::here("output"),
                     full.names = TRUE,
                     pattern = "summary_reg") %>% 
  readRDS() %>% 
  mutate(y = response)

## join weighted stock
df_m <- df_reg %>% 
  filter(str_detect(parameter, "ef_stock")) %>%
  dplyr::select(site_id_numeric,
                ef_stock = median) %>% 
  right_join(df_ssm,
             by = "site_id_numeric")


# check outlier influence -------------------------------------------------

## outlier check for log-cv
y <- df_m %>%
  filter(group == "all", response == "cv") %>% 
  pull(value)

outliers::grubbs.test(log(y))

## check robustness with MM method
MASS::rlm(log(value) ~ scale(ef_stock),
          method = "MM",
          df_m %>% filter(group == "all",
                          response == "cv")) %>% 
  summary()


# lmer double check -------------------------------------------------------

df_id <- distinct(df_m, group, response) %>% 
  filter(!(group != "all" &
           (response == "cv" | response == "n_species"))) %>% 
  mutate(response = factor(response,
                           levels = c("cv", "n_species", "mu", "sigma"))) %>% 
  arrange(group, response)

list_fit <- foreach(i = 1:nrow(df_id)) %do% {
  
  df_lm <- df_m %>% 
    filter(group == df_id %>% slice(i) %>% pull(group),
           response == df_id %>% slice(i) %>% pull(response)) %>% 
    mutate(log_wsd_area = log(wsd_area),
           across(.cols = c(ef_stock,
                            log_wsd_area,
                            temp,
                            ppt,
                            frac_forest,
                            n_obs,
                            chr_a,
                            sd_elev),
                  .fns = function(x) (x - mean(x)) / sd(x)))
  
  if(df_id %>% slice(i) %>% pull(response) == "n_species") {
    
    fit <- glmmTMB::glmmTMB(value ~ 
                              ef_stock +
                              log_wsd_area +
                              temp +
                              ppt +
                              frac_forest +
                              n_obs +
                              chr_a +
                              sd_elev +
                              (1 | river),
                            family = poisson,
                            data = df_lm)
    
  } else {
    
    fit <- glmmTMB::glmmTMB(log(value) ~ 
                              ef_stock +
                              log_wsd_area +
                              temp +
                              ppt +
                              frac_forest +
                              n_obs +
                              chr_a +
                              sd_elev +
                              (1 | river),
                            data = df_lm)
    
  }
  
  return(fit)
}
