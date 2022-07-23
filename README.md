# PlantOntogenyDynamics README
Collection of code and scripts for modeling project studying consumer-resource dynamics with explicit plant ontogeny from Simon et al, 2022 publication.

## Code Types:

* Mathematica .cdf files can be run for free using Wolfram’s Computable Document Format (cdf) file format with Wolfram’s free cdf player, available at: https://www.wolfram.com/cdf/
* R Markdown’s HTML files can opened and viewed with any standard internet browser.
* R scripts can be opened and run in either base R or RStudio.
* The RShiny app describing the random forest analysis we completed is active online and can be accessed via any internet browser by clicking the following link: https://prglaum.shinyapps.io/simData-randomForestRMD/ 

## Major Folders:
### COMPADRE:
Folder containing .R script and R Markdown html file describing process used to download plant matrix demographic data from the COMPADRE online database. 

### Simulation Data: 
Folder containing example simulation data made publicly available for use in the R Shiny random forest demonstration app available at https://prglaum.shinyapps.io/simData-randomForestRMD/. 
We only provide a selected subset of our full data given its large size. For the full simulation data set, please visit … 

### Simulation Analysis:
Folder containing Model Mathematica code (.cdf) and .R scripts of analysis used in used Sophia et al 2022, .R scripts required to load data for analysis in R, R markdown file of static random forest example, and R markdown/R Shiny code used to run the random forest demonstration app available at https://prglaum.shinyapps.io/simData-randomForestRMD/.

## File Descriptions:

### COMPADRE Analysis Folder:
Scripts used to download and analyze the plant demographic matrices used to inform our model structure and parameters is available in 3 different formats. 
* compadreAnalysis.R – Base .R script with necessary packages and code required to recreate our use of COMPADRE data. Load in R or RStudio, install/load necessary packages, and run script. Please  visit https://compadre-db.org/ for more information on COMPADRE. 
* CompadreAnalysisRMD.html – R Markdown created HTML file can be opened in any internet browser. All the code from the base R script (compadreAnalysis.R) is pre-run and available to examine as a standalone HTML document without needing to open R directly. Also includes interactive map of plant data locations. 
* CompadreAnalysisRMD.Rmd – The R Markdown code used to create the CompadreAnalysisRMD.html file. This does not need to be run to complete any of the analyses in the manuscript but is provided to anyone interested in the creation of the HTML files.

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

## Simulation Analysis Folder:
Folder containing a subset of the simulation data. This subset functions as the publicly accessible data necessary to run the random forest example application available at https://prglaum.shinyapps.io/simData-randomForestRMD/. It does not need to be used for any other analysis, but could be downloaded and analyzed locally should the reader be interested. 
* h1a1Small.csv – A csv file containing the simulation data subset where the handling times h~F = h~2 = 1 and density dependence effects (α~g1 = α~g2 = α~F = 0.1). Simulation data consists of all examined model parameters as well as the maximum eigenvalues and categorical indication of stability or instability from the resultant simulation. 

