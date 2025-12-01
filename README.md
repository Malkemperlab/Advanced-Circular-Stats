
README for the R scripts associated with Landler et al. 2025, Methods in Ecology and Evolution

Overview
These folders contains the code and data for the simulations and examples shown and discussed in the associated scientific publicaton (Landler et al. 2025: Advanced circular statistics in biology: multiple factors, interactions and repeated measures; Methods in Ecology and Evolution: XX-XX). 
Folder Structure and content
1. Simulation_Code
This folder contains simulation scripts for various scenarios and analyses.

1.1. 0_Basics_sim
R Files
 `MANOVA-(5-10-15-25-50-100)_10000.R`: Script for basic simulations (used in Fig S2 and Fig S3). 

`Background_stuff_Manova_manuscript`: Script for background functions (loaded in all simulation scripts)

1.2. 1_Hypothetical_Example_sim
R Files
`Hypothetical_Example_LM.R`: Script for simulating hypothetical examples without random effects (used in Fig S4 and Fig S5). 

 `Hypothetical_Example_LMM_IDerr_k4.R`: Script for simulating hypothetical examples with random effects (used in Fig. 3, Fig S7 and FigS8).

`Background_stuff_Manova_manuscript`: Script for background functions (loaded in all simulation scripts)

1.3. 2_Interaction_sim
R Files
 `Hypothetical_Example_LM_interaction.R`: Script for simulating interaction effects (used in Fig. 2 and Fig S6). 

`Background_stuff_Manova_manuscript`: Script for background functions (loaded in all simulation scripts)

1.4. 3_Emlen_Funnel_sim
R Files
`Hypothetical_EmlenFunnel_unpaired.R`: Script for unpaired Emlen funnel simulations (used in Fig 4 and Fig. S9).

`Hypothetical_EmlenFunnel_paired.R`: Script for paired Emlen funnel simulations (used in Fig. 5 and Fig. S10).

`Background_stuff_Manova_manuscript`: Script for background functions (loaded in all simulation scripts)

2. Code_and_Data_for_Figs
This folder contains code used to generate the figures for the manuscript (data was generated from the simulations shown in “Simulation_Code”).

R Files
`Figure_example_dist.R`: Script for Fig. S1. 
`Figure_Type1_error_basic_distribution.R`: Script for Fig. S2.
`Figure_For_MANOVA_intercept_power.R`: Script for Fig. S3.
`Figure_For_MANOVA_examples_both.R`: Script for Fig. S4, Fig. S5, Fig. S7 and Fig. S8. 
`Main_Figure_MANOVA_examples.R`: Script for Fig. 3.
 `Figure_interaction_hypothetical_example.R`: Script for Fig. S6.
`Main_Figure_interaction_hypothetical_example.R`: Script for Fig. 2.
`Figure_simEmlenFunnel_examples_unpaired.R`: Script for Fig. S9.
`Main_simEmlenFunnel_unpaired.R`: Script for Fig. 4 .
`Figure_simEmlenFunnel_examples_paired.R`: Script for Fig. S10.
`Main_simEmlenFunnel_paired.R`: Script for Fig. 5 .
`Background_stuff_Manova_manuscript_fig.R`: Supporting script for figure generation.

3. Real_Example_Code_and_Data
This folder contains code and data for real-world examples analyzed in the project.

R Files
`Functions_Real_Examples.R`: Contains custom functions loaded in the two main scripts. 
 `Real_Example_birds.R`: Script for analyzing the real world Emlen funnel data. 
 `Real_example_shark.R`: Script for analyzing the real world shark orientation. 

Data Files
shark.xlsx: Shark data containg the shark data as described in the paper (with seven columns: ID, Wind_direct2, Wind_strength, Swell, Depth, Sunglare and Approach). 
Wraw_reduced.xlsx: Bird Emlen funnel data as described in the paper (with four columns: Bird, Treatment, dir, Exp_Sequence). 
How to Use
1.Set Up the Environment
- Ensure you have R installed.
- If you want to use the GUI Rstudio (make sure you have it installed), open the `.Rproj` files in RStudio for the respective folders to load the project environment.

2. Run Scripts
- Navigate to the relevant folder and open the `.R` script in RStudio.
- Execute the script line by line or as a whole to perform the analysis or generate figures.

3. Modify Parameters
- Many scripts allow for parameter customization (e.g., sample size, number of iterations). Modify these parameters as needed for your analysis.

R session info 
R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Ventura 13.4.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Vienna
tzcode source: internal

attached base packages:
[1] parallel  compiler  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] foreach_1.5.2     lmerTest_3.1-3    lme4_1.1-36       Matrix_1.7-1      data.table_1.16.4
[6] CircMLE_0.3.0     NPCirc_3.1.1      circular_0.5-1   

loaded via a namespace (and not attached):
 [1] gtable_0.3.6        shape_1.4.6.1       xfun_0.52           ggplot2_3.5.2      
 [5] htmlwidgets_1.6.4   lattice_0.22-6      numDeriv_2016.8-1.1 vctrs_0.6.5        
 [9] tools_4.4.2         Rdpack_2.6.2        generics_0.1.3      stats4_4.4.2       
[13] flexmix_2.3-20      rgl_1.3.17          tibble_3.2.1        fansi_1.0.6        
[17] pkgconfig_2.0.3     lifecycle_1.0.4     munsell_0.5.1       codetools_0.2-20   
[21] misc3d_0.9-1        movMF_0.2-9         htmltools_0.5.8.1   pillar_1.9.0       
[25] Bolstad2_1.0-29     nloptr_2.1.1        MASS_7.3-61         reformulas_0.4.0   
[29] iterators_1.0.14    boot_1.3-31         nlme_3.1-166        tidyselect_1.2.1   
[33] digest_0.6.37       mvtnorm_1.3-2       slam_0.1-55         dplyr_1.1.4        
[37] splines_4.4.2       gsl_2.1-8           fastmap_1.2.0       grid_4.4.2         
[41] colorspace_2.1-1    cli_3.6.5           magrittr_2.0.3      base64enc_0.1-3    
[45] utf8_1.2.4          scales_1.3.0        plotrix_3.8-4       energy_1.7-12      
[49] nnet_7.3-19         modeltools_0.2-24   evaluate_1.0.1      knitr_1.49         
[53] rbibutils_2.3       tcltk_4.4.2         doParallel_1.0.17   rlang_1.1.6        
[57] Rcpp_1.0.13-1       glue_1.8.0          rstudioapi_0.17.1   minqa_1.2.8        
[61] jsonlite_1.8.9      R6_2.5.1           

