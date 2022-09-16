README
================
Akira Terui
Last updated 2022-09-16

## Article information

**Title:** Intentional release of native species undermines ecological
stability

**Author:** Akira Terui, Hirokazu Urabe, Masayuki Senzaki, Bungo
Nishizawa

## Contents

| dir      | class      | name                                 | description                                                                                                                                                                                                                                                                           |
|:---------|:-----------|:-------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| code     | analysis   | analysis_stvy.R                      | correlation for sensitivity analysis of theoretical model                                                                                                                                                                                                                             |
| code     | figure     | figure_layout.R                      | layout empirical and theoretical patterns                                                                                                                                                                                                                                             |
| code     | figure     | figure_obs_coef.R                    | figure codes for map (`_map`), coefficients (`_coef`), or stock effect (`_stock`)                                                                                                                                                                                                     |
| code     | figure     | figure_obs_map.R                     | figure codes for map (`_map`), coefficients (`_coef`), or stock effect (`_stock`)                                                                                                                                                                                                     |
| code     | figure     | figure_obs_stock.R                   | figure codes for map (`_map`), coefficients (`_coef`), or stock effect (`_stock`)                                                                                                                                                                                                     |
| code     | figure     | figure_theory.R                      | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | figure     | figure_theory_2sp.R                  | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | figure     | si_figure_cor.R                      | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_env.R                      | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_obs_dyns.R                 | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_species_int.R              | supporting figure codes for correlations in environmental variables (`_cor`), histograms for environmental data (`_env`), observed community dynamics (`_obs_dyns`), pairwise species interactions (`_species_int`), and scenario simulations in a species-rich community (`_theory`) |
| code     | figure     | si_figure_theory.R                   | theoretical predictions for stocking & community dynamics in species-rich (no postfix) or 2-species scenarios (`_2sp`)                                                                                                                                                                |
| code     | format     | data_fmt_fishdata.R                  | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_reg.R                       | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_stock.R                     | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | format     | data_fmt_trait.R                     | format data for regression (`_reg`), fish count data (`_fishdata`), stock data (`_stock`), and trait data (`_trait`)                                                                                                                                                                  |
| code     | gis        | gis_environment.R                    | extract layer data for environment                                                                                                                                                                                                                                                    |
| code     | gis        | gis_ocean.R                          | extract layer data for ocean                                                                                                                                                                                                                                                          |
| code     | gis        | gis_watershed.R                      | delineate watersheds                                                                                                                                                                                                                                                                  |
| code     | jags       | model_joint.R                        | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | model_multi_ricker_sparse.R          | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | model_reg.R                          | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_joint.R               | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_multi_ricker_sparse.R | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | jags       | run_jags_model_reg.R                 | jags model for joint state-space (`_joint`), multi-species ricker (`_multi_ricker_sparse`), and regression (`_reg`)                                                                                                                                                                   |
| code     | set        | set_crs.R                            | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_figure_theme.R                   | set defaults                                                                                                                                                                                                                                                                          |
| code     | set        | set_functions.R                      | set defaults                                                                                                                                                                                                                                                                          |
| code     | simulation | run_simulation.R                     | run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)                                                                                                                                                          |
| code     | simulation | run_simulation_analytical.R          | run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)                                                                                                                                                          |
| code     | simulation | run_simulation_stvy.R                | run simulation model for species rich (no postfix) and 2-species community (`_analytical`) or sensitivity analysis (`_stvy`)                                                                                                                                                          |
| code     | table      | table_param_set.R                    | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_prior.R                        | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_reg.R                          | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_species_list.R                 | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_ssm.R                          | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_stvy_analysis.R                | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | NA         | library.R                            | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_env_fmt.csv                     | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_hkd_prtwsd_fmt.csv              | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_hkd_prtwsd_stock_fmt.csv        | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_joint3.rds                      | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_ocean_fmt.csv                   | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_trait.rds                       | NA                                                                                                                                                                                                                                                                                    |

## Session Information

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
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
    ##  [1] loo_2.4.1         forecast_8.17.0   ggside_0.2.1      runjags_2.2.1-7  
    ##  [5] scales_1.2.0      doSNOW_1.0.19     snow_0.4-3        doParallel_1.0.16
    ##  [9] iterators_1.0.13  foreach_1.5.1     cdyns_0.1.0       here_1.0.1       
    ## [13] patchwork_1.1.1   forcats_0.5.1     stringr_1.4.1     dplyr_1.0.9      
    ## [17] purrr_0.3.4       readr_2.1.2       tidyr_1.2.0       tibble_3.1.7     
    ## [21] ggplot2_3.3.6     tidyverse_1.3.2  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-152        matrixStats_0.59.0  fs_1.5.2           
    ##  [4] xts_0.12.1          lubridate_1.8.0     httr_1.4.4         
    ##  [7] rprojroot_2.0.2     tools_4.2.1         backports_1.2.1    
    ## [10] utf8_1.2.2          R6_2.5.1            DBI_1.1.1          
    ## [13] colorspace_2.0-3    nnet_7.3-16         withr_2.5.0        
    ## [16] tidyselect_1.1.2    curl_4.3.2          compiler_4.2.1     
    ## [19] cli_3.3.0           rvest_1.0.3         pacman_0.5.1       
    ## [22] xml2_1.3.3          tseries_0.10-51     checkmate_2.0.0    
    ## [25] lmtest_0.9-38       fracdiff_1.5-1      quadprog_1.5-8     
    ## [28] digest_0.6.29       rmarkdown_2.16      pkgconfig_2.0.3    
    ## [31] htmltools_0.5.2     dbplyr_2.1.1        fastmap_1.1.0      
    ## [34] highr_0.9           rlang_1.0.3         readxl_1.3.1       
    ## [37] TTR_0.24.3          rstudioapi_0.14     quantmod_0.4.20    
    ## [40] generics_0.1.3      zoo_1.8-9           jsonlite_1.7.2     
    ## [43] googlesheets4_1.0.0 magrittr_2.0.3      Rcpp_1.0.8.3       
    ## [46] munsell_0.5.0       fansi_1.0.3         lifecycle_1.0.1    
    ## [49] stringi_1.6.1       yaml_2.2.1          grid_4.2.1         
    ## [52] crayon_1.5.1        lattice_0.20-45     haven_2.5.1        
    ## [55] hms_1.1.2           knitr_1.40          pillar_1.8.1       
    ## [58] codetools_0.2-18    reprex_2.0.2        urca_1.3-0         
    ## [61] glue_1.6.2          evaluate_0.16       modelr_0.1.8       
    ## [64] vctrs_0.4.1         tzdb_0.3.0          cellranger_1.1.0   
    ## [67] gtable_0.3.0        assertthat_0.2.1    xfun_0.30          
    ## [70] broom_1.0.0         coda_0.19-4         googledrive_2.0.0  
    ## [73] gargle_1.2.0        timeDate_3043.102   ellipsis_0.3.2
