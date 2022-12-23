

# setup -------------------------------------------------------------------

#rm(list = ls())
source(here::here("code/library.R"))

# data --------------------------------------------------------------------

## raw data for cv, mean, sd
suppressMessages(source(here::here("code/data_fmt_reg.R")))

df_m <- df_ssm %>%
  filter(group == "all",
         response == "cv")


# check outlier influence -------------------------------------------------

## outlier check for log-cv
outliers::grubbs.test(log(df_m$value))

## check robustness with MM method
MASS::rlm(log(value) ~ scale(ef_stock),
          method = "MM",
          df_test) %>% 
  summary()
