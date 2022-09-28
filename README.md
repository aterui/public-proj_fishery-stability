README
================

## Article Information

Title: Intentional release of native species undermines ecological
stability Authors: Akira Terui, Hirokazu Urabe, Masayuki Senzaki, Bungo
Nishizawa

## Contents

| dir      | class      | name                                 | description                                                                                                                                                                                                                                                                           |
|:---------|:-----------|:-------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
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
| code     | table      | table_species_list.R                 | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | table      | table_ssm.R                          | tables for theoretical parameters (`_param_set`), priors (`_prior`), species list (`_species_list`), and estimated regression (`_reg` and `_stvy_analysis`) or state-space model parameters (`_ssm`)                                                                                  |
| code     | NA         | library.R                            | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_env_fmt.rds                     | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_fd.rds                          | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_hkd_prtwsd_fmt.csv              | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_hkd_prtwsd_stock_fmt.csv        | NA                                                                                                                                                                                                                                                                                    |
| data_fmt | NA         | data_ocean_fmt.rds                   | NA                                                                                                                                                                                                                                                                                    |
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
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.5.2   stringr_1.4.1   dplyr_1.0.10    purrr_0.3.4    
    ## [5] readr_2.1.2     tidyr_1.2.1     tibble_3.1.8    ggplot2_3.3.6  
    ## [9] tidyverse_1.3.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.2    xfun_0.33           haven_2.5.1        
    ##  [4] gargle_1.2.1        colorspace_2.0-3    vctrs_0.4.1        
    ##  [7] generics_0.1.3      htmltools_0.5.3     yaml_2.3.5         
    ## [10] utf8_1.2.2          rlang_1.0.5         pillar_1.8.1       
    ## [13] withr_2.5.0         glue_1.6.2          DBI_1.1.3          
    ## [16] dbplyr_2.2.1        modelr_0.1.9        readxl_1.4.1       
    ## [19] lifecycle_1.0.2     munsell_0.5.0       gtable_0.3.1       
    ## [22] cellranger_1.1.0    rvest_1.0.3         evaluate_0.16      
    ## [25] knitr_1.40          tzdb_0.3.0          fastmap_1.1.0      
    ## [28] fansi_1.0.3         highr_0.9           broom_1.0.1        
    ## [31] backports_1.4.1     scales_1.2.1        googlesheets4_1.0.1
    ## [34] jsonlite_1.8.0      fs_1.5.2            hms_1.1.2          
    ## [37] digest_0.6.29       stringi_1.7.8       grid_4.2.1         
    ## [40] rprojroot_2.0.3     here_1.0.1          cli_3.4.0          
    ## [43] tools_4.2.1         magrittr_2.0.3      pacman_0.5.1       
    ## [46] crayon_1.5.1        pkgconfig_2.0.3     ellipsis_0.3.2     
    ## [49] xml2_1.3.3          reprex_2.0.2        googledrive_2.0.0  
    ## [52] lubridate_1.8.0     assertthat_0.2.1    rmarkdown_2.16     
    ## [55] httr_1.4.4          rstudioapi_0.14     R6_2.5.1           
    ## [58] compiler_4.2.1
