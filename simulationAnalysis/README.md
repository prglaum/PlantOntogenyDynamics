## File Descriptions:
## Simulation Analysis Folder:
Model code (Mathematica) and analysis scripts (R, RMarkdown, RShiny) used to run the model and analyze its behavior respectively. 
* Model-SI.cdf – Plant-herbivore model code written as a Mathematica cdf file. Simply download Wolfram’s free cdf player (see above) and open the file. The model is ready for immediate user input via parameter settings set by the user. 
* SimDataLoader.R: This script loads simulation data from model runs across our studied parameter space (see main text). All permutations of simulation data files are too large to load all at once. Therefore, simulation data is stored in subsets based on specific combinations of our sensitivity parameters, handling time h and density dependence, alpha (see Table 1).
  * Each subset is stored in its own unique folder as multiple .csv files. Each parameter permutation subset is available in its own folder online at (GET TINY URL   HERE). Download the simulation data and store it locally. 
  * Open SimDataLoader.R, install/load all necessary packages towards the beginning of the script. 
  * Choose which specific data subset you would like to analyze and run that code chunk. Make sure it is locally available. The script will stich the individual csv    files into a single usable data frame in R.
  * Finally, make sure to run the lines which create the ecological factors used in our analysis.
* randomForestCode.html – Static HTML document which can be opened in any browser. This document lays out the process of analyzing simulation data with the randomForest R package for both categorical tasks (e.g., stable or unstable) and regression tasks (e.g., the value of the maximum eigenvalue). Additionally, we also include code used to analyze feature effects via the iml R package. 
* Main_text_figures.R – Base R script containing code used to create all main text figures except for iml based figures. Code for iml based figures is available in randomForestCode.html. 
* SI-figs.R – Base R script containing code used to create supplementary text figures. 
* simData-randomForestRMD.Rmd – This is the code used to create the random forest demonstration app (https://prglaum.shinyapps.io/simData-randomForestRMD/) using a combination of RMarkdown and RShiny.
