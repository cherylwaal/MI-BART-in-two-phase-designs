# MI-BART-in-two-phase-designs

R code for "Improving Survey Inference in Two-phase Designs Using Bayesian Machine Learning"

R code and results for simulation studies can be found at the file "simulation", where

  1) The file "code" includes the R code for simulation studies:
   a) R_pac_2.R include all functions for generating phase-I and phase-II sample, and various estimation methods (subsample weighting, MI-tree based methods);
   b) simu.R include the parameter setting and the function for running simulation replicates under each parameter setting;
   c) runSimu include the code for running simu.R for each parameter setting using HPC.
   d) plot.R include the code for plot the results from simulations. The data used for generating the figures is stored in the file "results". The figures are stored in the file "figures".
   
  2) The file "data" includes a finite population dataset generated according to the main manuscript.
  
  3) The file "figure" includes all simulation results figures.
  
  4) The file "results" includes all simulation results data.
  

R code for the NHANES data analysis in our manuscript can be found at the file "application", where

  1) The file "code" includes the R code for conducting the estimation by methods introduced in our main manuscript and plotting the results;
  2) The file "figures" includes all application results figures.
  For all the NHANES data used in this analysis, we direct users to \url{https://wwwn.cdc.gov/Nchs/Nhanes/continuousnhanes/default.aspx?BeginYear=2017}, which is freely available but against the use agreement to archive separately.