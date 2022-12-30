README
================

## Article Information

**Title:** Intentional release of native species undermines ecological
stability

**Authors:** Akira Terui, Hirokazu Urabe, Masayuki Senzaki, Bungo
Nishizawa

## Analysis flow

### Theory

**Two-species simulation** – simulation with `cdyns::cdynsim()` (see R
package <https://github.com/aterui/cdyns>). Run
`run_simulation_analytical.R`. Simulation results are saved in
`output/result_ricker_2sp.rds`.

**Whole community simulation** – simulation with `cdyns::cdynsim()` (see
R package <https://github.com/aterui/cdyns>). Run `run_simulation.R`.
Simulation results are saved in `output/result_ricker.rds`.

### Empirical

**State-space AR model** – run `run_jags_model_ssm_ar.R`. Data sourced
from `data_fmt_XXX.R` files. Model code is `model_ssm_ar.R`

**Regression** – run `run_jags_model_reg.R`. Data sourced from
`data_fmt_XXX.R` files. Model code is `model_reg.R`

**State-space Ricker model** – run
`run_jags_model_multi_ricker_sparse.R`. Data sourced from
`data_fmt_XXX.R` files. Model code is `model_multi_ricker_sparse.R`

## Contents

| dir      | class      | name                                 | description                                                                                                                                                                                                                                                                           |
|:---------|:-----------|:-------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| code     | analysis   | run_analysis_lmcheck.R               | confirm Bayesian results with `glmmTMB()`                                                                                                                                                                                                                                             |
| code     | figure     | figure_layout.R                      | layout empirical and theoretical patterns                                                                                                                                                                                                                                             |
| code     | figure     | figure_obs_coef.R                    | figure codes for map (`_map`), coefficients (`_coef`), stock effect (`_stock`), or raw plot (`_rawplot`)                                                                                                                                                                              |
| code     | figure     | figure_obs_map.R                     | figure codes for map (`_map`), coefficients (`_coef`), stock effect (`_stock`), or raw plot (`_rawplot`)                                                                                                                                                                              |
| code     | figure     | figure_obs_rawplot.R                 | figure codes for map (`_map`), coefficients (`_coef`), stock effect (`_stock`), or raw plot (`_rawplot`)                                                                                                                                                                              |
| code     | figure     | figure_obs_stock.R                   | figure codes for map (`_map`), coefficients (`_coef`), stock effect (`_stock`), or raw plot (`_rawplot`)                                                                                                                                                                              |
| code     | figure     | figure_species_int.R                 | NA                                                                                                                                                                                                                                                                                    |
| code     | figure     | figure_theory.R                      | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | figure     | si_figure_co.R                       | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_cor.R                      | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_env.R                      | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_obs_dyns.R                 | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_species_int.R              | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_theory.R                   | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | figure     | si_figure_theory_2sp.R               | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | format     | data_fmt_fishdata.R                  | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_reg.R                       | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_stock.R                     | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_trait.R                     | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | gis        | gis_environment.R                    | extract layer data for environment                                                                                                                                                                                                                                                    |
| code     | gis        | gis_ocean.R                          | extract layer data for ocean                                                                                                                                                                                                                                                          |
| code     | gis        | gis_watershed.R                      | delineate watersheds                                                                                                                                                                                                                                                                  |
| code     | jags       | model_multi_ricker_sparse.R          | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | model_reg.R                          | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | model_ssm_ar.R                       | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_multi_ricker_sparse.R | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_reg.R                 | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_ssm_ar.R              | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | set        | set_crs.R                            | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_figure_theme.R                   | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_functions.R                      | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_readme.R                         | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_rmd.R                            | set defaults                                                                                                                                                                                                                                                                          |
| code     | simulation | run_simulation.R                     | run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)                                                                                                                                                          |
| code     | simulation | run_simulation_analytical.R          | run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)                                                                                                                                                          |
| code     | table      | table_param_set.R                    | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_prior.R                        | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_reg.R                          | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_ricker.R                       | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_species_list.R                 | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_ssm_ar.R                       | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | NA         | library.R                            | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | data       | data_env_fmt.rds                     | formatted data for species trait (`_trait`), ocean data (`_ocean`), environment (`_env`), fish abundance (`_hkd_prtwsd`), and fish stocking (`_hkd_prtwsd_stock`)                                                                                                                     |
| data_fmt | data       | data_hkd_prtwsd_fmt.csv              | formatted data for species trait (`_trait`), ocean data (`_ocean`), environment (`_env`), fish abundance (`_hkd_prtwsd`), and fish stocking (`_hkd_prtwsd_stock`)                                                                                                                     |
| data_fmt | data       | data_hkd_prtwsd_stock_fmt.csv        | formatted data for species trait (`_trait`), ocean data (`_ocean`), environment (`_env`), fish abundance (`_hkd_prtwsd`), and fish stocking (`_hkd_prtwsd_stock`)                                                                                                                     |
| data_fmt | data       | data_ocean_fmt.rds                   | formatted data for species trait (`_trait`), ocean data (`_ocean`), environment (`_env`), fish abundance (`_hkd_prtwsd`), and fish stocking (`_hkd_prtwsd_stock`)                                                                                                                     |
| data_fmt | data       | data_trait.rds                       | formatted data for species trait (`_trait`), ocean data (`_ocean`), environment (`_env`), fish abundance (`_hkd_prtwsd`), and fish stocking (`_hkd_prtwsd_stock`)                                                                                                                     |

## Session Information

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] whitebox_2.1.5      stars_0.5-6         abind_1.4-5        
    ##  [4] sf_1.0-8            exactextractr_0.9.0 raster_3.6-3       
    ##  [7] sp_1.5-0            terra_1.6-17        knitr_1.40.4       
    ## [10] stargazer_5.2.3     msir_1.3.3          corrplot_0.92      
    ## [13] here_1.0.1          scales_1.2.1        forecast_8.18      
    ## [16] loo_2.5.1           runjags_2.2.1-7     doSNOW_1.0.20      
    ## [19] snow_0.4-4          doParallel_1.0.17   iterators_1.0.14   
    ## [22] foreach_1.5.2       cdyns_0.1.0         cowplot_1.1.1      
    ## [25] ggspatial_1.1.6     ggridges_0.5.4      ggside_0.2.1       
    ## [28] patchwork_1.1.2     forcats_0.5.2       stringr_1.4.1      
    ## [31] dplyr_1.0.10        purrr_0.3.5         readr_2.1.3        
    ## [34] tidyr_1.2.1         tibble_3.1.8        ggplot2_3.3.6      
    ## [37] tidyverse_1.3.2    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] googledrive_2.0.0   colorspace_2.0-3    ellipsis_0.3.2     
    ##  [4] class_7.3-20        mclust_5.4.10       rprojroot_2.0.3    
    ##  [7] fs_1.5.2            rstudioapi_0.14     proxy_0.4-27       
    ## [10] fansi_1.0.3         lubridate_1.8.0     xml2_1.3.3         
    ## [13] codetools_0.2-18    jsonlite_1.8.3      broom_1.0.1        
    ## [16] dbplyr_2.2.1        compiler_4.2.1      httr_1.4.4         
    ## [19] backports_1.4.1     assertthat_0.2.1    fastmap_1.1.0      
    ## [22] gargle_1.2.1        cli_3.4.1           htmltools_0.5.3    
    ## [25] tools_4.2.1         coda_0.19-4         gtable_0.3.1       
    ## [28] glue_1.6.2          Rcpp_1.0.9          cellranger_1.1.0   
    ## [31] fracdiff_1.5-1      vctrs_0.5.1         urca_1.3-3         
    ## [34] nlme_3.1-160        lmtest_0.9-40       lwgeom_0.2-9       
    ## [37] timeDate_4021.106   xfun_0.34           rvest_1.0.3        
    ## [40] lifecycle_1.0.3     pacman_0.5.1        googlesheets4_1.0.1
    ## [43] zoo_1.8-11          hms_1.1.2           yaml_2.3.6         
    ## [46] quantmod_0.4.20     curl_4.3.3          stringi_1.7.8      
    ## [49] highr_0.9           tseries_0.10-52     e1071_1.7-11       
    ## [52] checkmate_2.1.0     TTR_0.24.3          rlang_1.0.6        
    ## [55] pkgconfig_2.0.3     matrixStats_0.62.0  evaluate_0.17      
    ## [58] lattice_0.20-45     tidyselect_1.2.0    magrittr_2.0.3     
    ## [61] R6_2.5.1            generics_0.1.3      DBI_1.1.3          
    ## [64] pillar_1.8.1        haven_2.5.1         withr_2.5.0        
    ## [67] units_0.8-0         xts_0.12.2          nnet_7.3-18        
    ## [70] modelr_0.1.9        crayon_1.5.2        KernSmooth_2.23-20 
    ## [73] utf8_1.2.2          tzdb_0.3.0          rmarkdown_2.17     
    ## [76] grid_4.2.1          readxl_1.4.1        reprex_2.0.2       
    ## [79] digest_0.6.30       classInt_0.4-8      munsell_0.5.0      
    ## [82] quadprog_1.5-8
