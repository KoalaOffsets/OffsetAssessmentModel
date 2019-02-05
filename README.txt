CA-MLR User Guide version 1.0 (4 February 2019)
===============================================


1. About the model
------------------
The CA-MLR is a cellular-automata (CA) spatial model with embedded Multinomial Logistic Regression (MLR) to calibrate its transition rule. 
The implementation of CA-MLR aims to simulate the multiple land-use changes pathways. The model was written in R language. It is highly recommended to use RStudio as GUI to run this model.


The input of the model comprises of quantitative independent variables and selected qualitative data.

The independent variables are
1. Slope (slope)
2. Elevation (elev)
3. Distance to road (road)
4. Road density (roadDen)
5. Distance to city (city)
6. Averaged soil water content (awc)
7. Averaged soil clay content (cly)
8. Urban Neighborhood ratio (NeighUrb)

Categorical independent variables
9. LGA's administration boundary (sa4)
10.Planning scheme 2017 (plan2017)

The dependent variable for model's calibration is
1. Land-use classes (lu2016). 
Classification of land-use classes with their codes in the model are,
10 Conservation,
21 Grazing native vegetation,
22 Cropping and modified pasture,
23 Other agricultural,
30 Plantation forest,
40 Rural residential,
51 Low-density urban residential,
52 Medium-density urban residential,
53 High-density urban residential,
60 Intensive urban area,
71 Intensive agriculture,
72 Industrial,
80 Water

The CA-MLR model has three modules
1. koala_ca_selectSample
2. koala_ca_fittingCoef
3. koala_ca_simulate

For calibration purposes: Note that user does not need to run all three modules to start the simulation. The koala_ca_selectSample and koala_ca_fittingCoef that produce respectively (1) the selection of samples based on the land-use changes pathway, and (2) the fitting the coefficient of estimates and intercepts have previously been run and their results can be called from koala_ca_simulate.

Unless NEW independent variables become available, the koala_ca_selectSample and koala_ca_selectSample need to be run. 


2. How to run koala_ca_selectSample
-----------------------------------
The koala_ca_selectSample aims to select the sampling points according to lu1999 (land-use classes in 1999) and the corresponding independent variables. The koala_ca_selectSample module has five running steps

0. Start
1. Checking and installing the required packages
2. Functions 
3. Load input
4. Input data preparation
5. Sampling points selection


Step 0. START 
Remove existing variables in the workspace including figures (if any)

Step 1. CHECKING AND INSTALLING REQUIRED PACKAGES
Instal the required packages to run the module

Step 2. FUNCTIONS
Default functions to be used for simulation.

Step 3. LOAD WORKING MAPS
Load the inputs, convert into data frame, and simplify the name of dataset. (sub-step 3.1) In this step, users need to define the working directory  before proceeding into subsequent steps. The default working directory is ("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset"). Alternatively, to work in the GPEM-LSEC2 server, users need to change the working directory into  ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset"). If problem appears during Load the input from "../input/maps/", check the working directory on step 3.1. Make sure you have the correct working directory that contains "../input/maps/" folder.(sub-step 3.1). (sub-step 3.2) Load working maps. The default file type is ASCII. The ASCII file can be prepared by exporting original Raster file in GIS into ASCII file with standard extent and spatial projection similar to land-use map 1999. New independent variables can be added using "[variable_dataset] <- raster( "input/maps/variable.asc")".(sub-step 3.3)  Conversion of input data with original unit into new values with range of zero and one [0 1]. To check the converted probability distribution function (pdf) of independent variablesm un-comments the "# hist" lines. To plot the converted independent variables, un-comments the "# plot" lines. The input data excludes the recreation and protection area for fitting the coefficient of estimates.

Step 4. SAMPLING POINTS SELECTION
Selection of the sampling points to be used for fitting the coefficient estimates. The selected points for each land-use classes in 1999 and the corresponding independent variables are stored in './input/mlr_201811/' folder with the file name mlr_dataXX.rda with XX is the land-use code. To change the storing folder, modify the fileNametest <- paste('./input/mlr_201811/mlr_data',luLabel[i],'.rda', sep = "") line. The output of this module is list of sampling points with the coloumns that consist of original land use in 1999, independent variables, and land-use 2016. These sampling points provide input for the koala_ca_fittingCoef.


3. How to run koala_ca_fittingCoef
-----------------------------------
The koala_ca_fittingCoef was a fitting module for the MLR coefficients and intercepts. It requires input from koala_ca_selectSample and gives coefficient of estimates and intercepts on each possible land changes pathways as its end-products.


The koala_ca_fittingCoef has three steps.
 
0. Start
1. Checking and installing the required packages
2. Specify working folder
3. Start fitting the coefficient


Step 0. START 
Remove existing variables in the workspace including figures (if any)

Step 1. CHECKING AND INSTALLING REQUIRED PACKAGES
Instal the required packages to run the module

Step 2. SPECIFY WORKING FOLDER
Users need to define the working directory  before proceeding into subsequent steps. The default working directory is ("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset"). Alternatively, to work in the GPEM-LSEC2 server, users need to change the working directory into  ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset"). If problem appears during Load the input from "../input/maps/", check the working directory on step 3.1. Make sure you have the correct working directory that contains "../input/maps/" folder.(step 3.1).

Step 3. START MLR for EACH LU in 1999
This step runs the estimation of coefficients and intercepts for MLR. Using the folder where selection of sample points is stored ( "./input/mlr_201811/mlr_data"), we load the data and ensure the categorical data are in trasnformed into factor class type (non integer). Then re-level the dependent variables (LCfact). The sub-step 3.5.1 estimates the coefficient using default input of  (test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly  + NeighUrb + UFfact + sa4fact + plan2017fact, data = macroVar, maxit = 150)  #maximum iteration 150. Users could also modify (adding or removing) the combined set of independent variables by modifying the input variable. The coefficient estimates and intercept are stored in fileNametest <- paste('./input/mlrsummary_NeighUrb_UF/coefficient',i,'.rda', sep = ""). To assess the convidence interval, un-comment the section to estimate the z and p-score (sub-step 3.x.3 and 3.x.4). The outputs of this module (koala_ca_fittingCoef) are list of coefficients and intercepts for each possible land-use changes. These outputs are stored in "./input/mlrsummary" folder and used for input on koala_ca_simulate module. 


4. How to run koala_ca_simulate
-----------------------------------
koala_ca_simulate is the core module for running the simulation of land-use changes. 


0. Start
1. Checking and installing the required packages
2. Load input
3. Functions
4. Input data preparation
5. Land-use change simulation
6. Accuracy assessment
7. Export maps for visualization

Steps 0 - 4 are routine procedure before running this module. They have similar procedures as in koala_ca_selectSample.

Step 0. Start: Remove existing variables in the workspace including figures (if any)

Step 1. Instal the required packages to run the module

Step 2. Load the inputs, convert into data frame, and simplify the name of headers. In this step, users need to define the working directory (step 2.1) before proceeding into subsequent steps. The default working directory is  

 ("C:/Users/uqawahy1/Documents/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset"). 

Alternatively, to work in the GPEM-LSEC2 server, users need to change the working directory into 

 ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset"). 

If problem appears during Load the input from "../input/maps/", check the working directory on step 2.1. Make sure you have the correct working directory that contains "../input/maps/" folder.

Step 3. Define functions for running the simulation on the subsequent steps and modules. New independent variables can be added using "[variable_dataset] <- raster( "input/maps/variable.asc")". The required file type is ASCII. The ASCII file can be prepared by exporting original Raster file in GIS into ASCII file with standard extent and spatial projection similar to land-use map 1999.

Step 4. Prepare the input data for simulation as well as the estimated transition probability (tp) on each cell. There are three sections in this module.

Section 4.1 Convert input data into the range of zero and one [0 1]. To check the converted probability distribution function (pdf) of independent variablesm un-comments the "# hist" lines. To plot the converted independent variables, un-comments the "# plot" lines.

Section 4.2 Predict the transition probablity on every cells in the study area using the independent variables and the coefficients of estimates.

Section 4.3 Re-write the transition probability for each possible transitions. tp_zero means no transition possibility observed.  

 tp.stats.10.10 <- to_raster(tp.stats[[1]][,2])
 
The above line has a meaning of transition probability from 10 (conservation) into 10 (conservation).


Step 5. This is the core module for land-use change simulation. The module simply runs multinomial random land-class selection based on transition probability on each cell. This module could onlly run properly when the necessary input data and parameters are available.

There are three sections on this module.
Section 5.1 Preparation of simulation parameters


Section 5.2 Calibration of CA model. (LU1999->LU2016) VS (LU1999->SimulLU2016)

Section 5.3



Step 6. Accuracy assessment module. This module assesses the accuracy of simulation result. Using the observed land-use map 1999 and 2016, different accuracy indices are used. The 



Step 7. Export the simulation results and their accuracy assessent into ArcGIS. Folder that stors the exported maps can be modified by changing "output/20181114" on the following lines

  filen  <- paste("output/20181114/lu_simul_", 1999+i, ".asc", sep="")	
  filen  <- paste("output/20181114/lu_simul_change.asc", sep="")	





Support
=======
If you any inquiries about the model, please send your email to a.wahyudi1@uq.edu.au or agung.jobs@gmail.com.

Have fun





Brisbane, December 2018
Agung Wahyudi





