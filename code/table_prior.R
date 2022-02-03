
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# table for simulation parameters -----------------------------------------

df_prior <- tibble(Model = c("State space", rep(NA, 6),
                             "Regression", rep(NA, 3)),
                   Parameter = c("$ln~n_{s,1}$",
                                 "$\\mu_r$",
                                 "$\\mu_{\\beta}$",
                                 "$\\sigma_r$",
                                 "$\\sigma_{\\beta}$",
                                 "$\\sigma_{state,s}$",
                                 "$\\sigma_{obs,s}$",
                                 "$\\gamma_k$",
                                 "$\\delta_k$",
                                 "$\\sigma_y$",
                                 "$\\sigma_{\\gamma}$"),
                   Prior = c("Normal(0, 10)",
                             "Normal(0, 10)",
                             "Normal(0, 10)",
                             "half-Student's t(0, 2.5, 3)",
                             "half-Student's t(0, 2.5, 3)",
                             "half-Student's t(0, 2.5, 3)",
                             "half-Student's t(0, 2.5, 3)",
                             "Normal(0, 10)",
                             "Normal(0, 10)",
                             "half-Student's t(0, 2.5, 3)",
                             "half-Student's t(0, 2.5, 3)"))
