# Hiring Discrimination: Determinants, Heterogeneity and Composition
This repository contains the data and code to replicate all results, graphs and tables for my master thesis, "Hiring Discrimination: Determinants, Heterogeneity and Composition", submitted in August 2024 at the University of Bonn.

## Folder Structure
* `src`: Contains the code and original data.
* `bld`: Contains the resulting tables, figures and tables.

## Prerequisites
* R (https://www.r-project.org/)

## Scripts
* `indShare.R`: Calculates the share of female employees in the respective inudstry.
* `indWage.R`: Estimate adjusted gender wage gaps.
* `variance.R`: Estimates firm and industry contact gap distributions.
* `descriptiveTables.R`: Script for creating all descriptive tables.
* `genderGradient.R`: Estimates and visualises the gender gradient.
* `wageGradient.R`: Estimates and visualises the wage gradient.
* `baselineRegressions.R`: Runs baseline regressions.
* `genderMajMinGradient.R`: Estimates and visualises the gender gradient conditional on race.

## Session Info
```
R version 4.4.1 (2024-06-14)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] ggplot2_3.5.1          fastDummies_1.7.3      labelled_2.13.0        gtsummary_2.0.0       
 [5] tikzDevice_0.12.6      fplot_1.1.0            tibble_3.2.1           kableExtra_1.4.0      
 [9] modelsummary_2.1.1     marginaleffects_0.21.0 fixest_0.12.1          readstata13_0.10.1    
[13] here_1.0.1             collapse_2.0.15        data.table_1.15.4      dplyr_1.1.4           
[17] plyr_1.8.9             readxl_1.4.3          

loaded via a namespace (and not attached):
 [1] gtable_0.3.5        xfun_0.46           bayestestR_0.14.0   insight_0.20.2      lattice_0.22-6     
 [6] numDeriv_2016.8-1.1 vctrs_0.6.5         tools_4.4.1         generics_0.1.3      parallel_4.4.1     
[11] datawizard_0.12.2   sandwich_3.1-0      fansi_1.0.6         pkgconfig_2.0.3     checkmate_2.3.2    
[16] stringmagic_1.1.2   filehash_2.4-6      lifecycle_1.0.4     farver_2.1.2        compiler_4.4.1     
[21] stringr_1.5.1       munsell_0.5.1       htmltools_0.5.8.1   Formula_1.2-5       crayon_1.5.3       
[26] tidyr_1.3.1         pillar_1.9.0        nlme_3.1-165        tidyselect_1.2.1    digest_0.6.36      
[31] performance_0.12.2  stringi_1.8.4       purrr_1.0.2         labeling_0.4.3      forcats_1.0.0      
[36] rprojroot_2.0.4     fastmap_1.2.0       grid_4.4.1          colorspace_2.1-1    cli_3.6.3          
[41] magrittr_2.0.3      cards_0.2.0         utf8_1.2.4          broom_1.0.6         withr_3.0.1        
[46] dreamerr_1.4.0      scales_1.3.0        backports_1.5.0     cardx_0.2.0         rmarkdown_2.27     
[51] cellranger_1.1.0    hms_1.1.3           zoo_1.8-12          evaluate_0.24.0     haven_2.5.4        
[56] knitr_1.48          parameters_0.22.1   viridisLite_0.4.2   rlang_1.1.4         Rcpp_1.0.13        
[61] glue_1.7.0          xml2_1.3.6          renv_1.0.7          svglite_2.1.3       rstudioapi_0.16.0  
[66] R6_2.5.1            tables_0.9.28       systemfonts_1.1.0  
```
