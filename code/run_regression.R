
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# run models --------------------------------------------------------------

(run_model <- list.files(path = here::here("code"),
                        full.names = TRUE,
                        pattern = "run_model_reg"))

lapply(run_model, source)
