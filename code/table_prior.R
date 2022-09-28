
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)


# table for simulation parameters -----------------------------------------

df_prior <- tibble(Model = c(rep("AR model", 9),
                             rep("Ricker model", 11),
                             rep("Regression", 4)),
                   Parameter = c(
                     "$\\theta_{\\beta}$",
                     "$\\theta_{\\xi_1}$",
                     "$\\theta_{\\xi_2}$",
                     "$\\tau$",
                     "$\\sigma_{\\beta}$",
                     "$\\sigma_{\\text{obs},s}$",
                     "$\\sigma_{\\text{state},s}$",
                     "$\\pmb{\\Omega_{\\xi}}$",
                     "$\\ln n_{s,1-3}$",
                     
                     "$\\sigma_{\\text{obs},i}$",
                     "$\\sigma_{\\text{state},i}$",
                     "$\\sigma_{\\lambda}$",
                     "$\\sigma_{\\gamma}$",
                     "$\\sigma_{\\epsilon,i}$",
                     "$\\pmb{\\Omega_{\\varepsilon}}$",
                     "$p_{\\alpha}^{\\text{intra}}$",
                     "$p_{\\alpha}^{\\text{inter}}$",
                     "$\\zeta_{t,d}$",
                     "$\\delta_{d,i}$",
                     "$\\ln n_{i,1}$",
                     
                     "$\\gamma$",
                     "$\\gamma'$",
                     "$\\pmb{\\eta_{w}}$ ($\\pmb{\\eta_{w}} = \\eta_{w,1},...,\\eta_{w,S_w}$)",
                     "$\\pmb{\\Omega_{y}}$"),
                   Prior = c("Normal(0, 10)",
                             "Normal(0, 10)",
                             "Normal(1, 10)",
                             "Unif(0, 1)",
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Scaled Inv-Wishart_2($\\pmb{\\phi}$)",
                             "Half-t(0, 2.5, 6)",
                             
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Half-t(0, 2.5, 6)",
                             "Scaled Inv-Wishart_2($\\pmb{\\phi}$)",
                             "Beta(1, 1)",
                             "Beta(1, 1)",
                             "Normal(0, 1)",
                             "Multiplicative gamma prior ($\\nu = 3$, $a_1 = 2$, $a_2 = 3$)",
                             "Half-t(-2, 2, 6)",
                             
                             "Normal(0, 10)",
                             "Normal(0, 10)",
                             "Dirichlet(1,...,1)",
                             "Scaled Inv-Wishart_2($\\pmb{\\phi}$)"),
                   ) %>% 
  mutate(Model = replace(Model, duplicated(Model), NA))
