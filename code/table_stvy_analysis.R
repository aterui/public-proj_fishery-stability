
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               stargazer)

load(here::here("result/result_stvy_analysis.RData"))


# table -------------------------------------------------------------------

table_stvy <- list(NULL)
text_match <- c("_all", "_enhanced", "_unenhanced")

for(i in seq_len(length(text_match))) {
  
  table_stvy[[i]] <- stargazer(fit_sense$fit[which(str_detect(fit_sense$response, text_match[i]))],
                               header = FALSE,
                               type = "latex",
                               covariate.labels = c("Number of species $S$",
                                                    "Intrinsic growth rate $r_1$",
                                                    "Maximum intrinsic growth rate $r_{max}$",
                                                    "Competition coefficient $\\bar{\\alpha}$",
                                                    "Carrying capacity $K$",
                                                    "Environmental variability $\\sigma_{\\epsilon}$",
                                                    "Relative fitness $f_R$",
                                                    "Intercept"),
                               single.row = FALSE,
                               digits = 3,
                               dep.var.caption  = "Response variable",
                               dep.var.labels.include = FALSE,
                               column.labels = c("Correlation with $\\mu$",
                                                 "Correlation with $\\sigma$"),
                               report = "vcs",
                               omit.table.layout = "ns",
                               model.numbers = FALSE)

}

table_stvy <- lapply(table_stvy, function(x) sub('^.+\\caption.+$','', x))