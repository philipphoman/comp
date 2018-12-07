
# Neural computations of threat in the aftermath of combat trauma



## Author

Philipp Homan (philipp dot homan at mssm dot edu)


## Citation

The corresponding paper is:

Homan P, Levy I, Feltham E, Gordon C, Hu J, Li J, Pietrzak RH, Southwick
S, Krystal JH, Harpaz-Rotem I, Schiller D. Neural computations of threat
in the aftermath of combat trauma. Nature Neuroscience.


## Getting Started

This repository contains all the data and analysis code to reproduce the
manuscript Neural computations of threat in the aftermath of combat
trauma. These instructions describe how to obtain a copy of the project
up and running on your local machine for reproducing the analysis
described in the manuscript. The repository contains a Makefile which
reflects the dependencies of the analysis; analysis, figures and
manuscript can be produced by simply typing 'make' from the Unix command
line.


### Prerequisites

All analyses were conducted with the R software 
R version 3.5.1 (2018-07-02) and the probabilistic
programming language Stan
2.18.2. Mixed models were estimated
using the lme4 library. The full session info under R can be found at
the end of this file


## Installing

Clone the repository or download the zip file.


## Producing the figures

Change to the comp directory and run 'make figures'. The figures can then
be found in output/figures.


## Producing the manuscript

Change to the comp directory and run 'make manuscript'. The manuscript
will be in src/comp\_ms.pdf. The manuscript was written in emacs
(<http://gnu.org/s/emacs>) and org-mode (<http://orgmode.org>) which is a
powerful plain text markdown language that allows to integrate code and
text.


## License

See the MIT License information in the project folder.


## Acknowledgments

The main source of funding for this work was provided by NIMH 105535 R01
grant awarded to I. Harpaz-Rotem and D. Schiller (MPI) and funding
provided by the Clinical Neurosciences Division of the National Center
for PTSD. Additional support was provided by Klingenstein-Simons
Fellowship Award in the Neurosciences to D. Schiller; The Brain and
Behavior Research Foundation to I. Harpaz-Rotem; Chinese NSF grant
31421003 to J. Li; and the Swiss National Science Foundation grant SNF
161077 to P. Homan. The analytic work was supported in part through the
computational resources and staff expertise provided by Scientific
Computing at the Icahn School of Medicine at Mount Sinai.


## Built With

Ubuntu 18.04.1 LTS on emacs
25.2.2 and org-mode
9.1.7.

[![HitCount](http://hits.dwyl.io/philipphoman/comp.svg)](http://hits.dwyl.io/philipphoman/comp)


## Session info

    R version 3.5.1 (2018-07-02)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 18.04.1 LTS
    
    Matrix products: default
    BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
    LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
    
    locale:
     [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
     [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    
    attached base packages:
    [1] grid      stats     graphics  grDevices utils     datasets  methods  
    [8] base     
    
    other attached packages:
     [1] bindrcpp_0.2.2         represearch_0.0.0.9000 lmerTest_3.0-1        
     [4] lme4_1.1-19            plotrix_3.7-4          lm.beta_1.5-1         
     [7] car_3.0-2              carData_3.0-2          truncnorm_1.0-8       
    [10] rstan_2.18.2           StanHeaders_2.18.0     R.matlab_3.6.2        
    [13] psych_1.8.10           cowplot_0.9.3          mediation_4.4.6       
    [16] sandwich_2.5-0         mvtnorm_1.0-8          Matrix_1.2-15         
    [19] QuantPsyc_1.5          MASS_7.3-51.1          boot_1.3-20           
    [22] png_0.1-7              forcats_0.3.0          stringr_1.3.1         
    [25] dplyr_0.7.8            purrr_0.2.5            readr_1.2.1           
    [28] tidyr_0.8.2            tibble_1.4.2           ggplot2_3.1.0         
    [31] tidyverse_1.2.1        pacman_0.5.0          
    
    loaded via a namespace (and not attached):
     [1] nlme_3.1-137        matrixStats_0.54.0  lubridate_1.7.4    
     [4] RColorBrewer_1.1-2  httr_1.3.1          numDeriv_2016.8-1  
     [7] tools_3.5.1         backports_1.1.2     R6_2.3.0           
    [10] rpart_4.1-13        Hmisc_4.1-1         lazyeval_0.2.1     
    [13] colorspace_1.3-2    nnet_7.3-12         withr_2.1.2        
    [16] tidyselect_0.2.5    gridExtra_2.3       prettyunits_1.0.2  
    [19] mnormt_1.5-5        processx_3.2.0      curl_3.2           
    [22] compiler_3.5.1      cli_1.0.1           rvest_0.3.2        
    [25] htmlTable_1.12      xml2_1.2.0          scales_1.0.0       
    [28] checkmate_1.8.5     callr_3.0.0         digest_0.6.18      
    [31] foreign_0.8-71      minqa_1.2.4         R.utils_2.7.0      
    [34] rio_0.5.10          base64enc_0.1-3     pkgconfig_2.0.2    
    [37] htmltools_0.3.6     htmlwidgets_1.3     rlang_0.3.0.1      
    [40] readxl_1.1.0        rstudioapi_0.8      bindr_0.1.1        
    [43] zoo_1.8-4           jsonlite_1.5        zip_1.0.0          
    [46] acepack_1.4.1       R.oo_1.22.0         inline_0.3.15      
    [49] magrittr_1.5        Formula_1.2-3       loo_2.0.0          
    [52] Rcpp_1.0.0          munsell_0.5.0       abind_1.4-5        
    [55] R.methodsS3_1.7.1   stringi_1.2.4       pkgbuild_1.0.2     
    [58] plyr_1.8.4          parallel_3.5.1      crayon_1.3.4       
    [61] lattice_0.20-38     haven_2.0.0         splines_3.5.1      
    [64] hms_0.4.2           knitr_1.20          ps_1.2.1           
    [67] pillar_1.3.0        lpSolve_5.6.13      stats4_3.5.1       
    [70] glue_1.3.0          latticeExtra_0.6-28 data.table_1.11.8  
    [73] modelr_0.1.2        nloptr_1.2.1        cellranger_1.1.0   
    [76] gtable_0.2.0        assertthat_0.2.0    openxlsx_4.1.0     
    [79] broom_0.5.0         survival_2.43-1     cluster_2.0.7-1

