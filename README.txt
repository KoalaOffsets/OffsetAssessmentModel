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

The CA-MLR model has four modules
1. koala_ca_selectSample
2. koala_ca_fittingCoef
3. koala_ca_simulate
4. koala_ca_forward

For calibration purposes: Note that user does not need to run all four modules to start the simulation. The koala_ca_selectSample and koala_ca_fittingCoef that produce respectively (1) the selection of samples based on the land-use changes pathway, and (2) the fitting the coefficient of estimates and intercepts have previously been run and their results can be called from koala_ca_simulate.

Unless NEW independent variables become available or existing independent variable needs to be taken away, the koala_ca_selectSample and koala_ca_selectSample need to be run. 


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
koala_ca_simulate simulates land-use changes in 2016 using the estimated coefficients obtained by comparing land use 1999 and 2016.  


0. Start
1. Checking and installing the required packages
2. Functions 
3. Load input
4. Simulation
5. Accuracy assessment
6. Export maps for visualization

Step 0. START 
Remove existing variables in the workspace including figures (if any)

Step 1. CHECKING AND INSTALLING REQUIRED PACKAGES
Instal the required packages to run the module. 

Step 2. FUNCTIONS
Define functions for running the koala_ca_simulate. The functions start with "to_" to differentiate with variables. Frequently used functions are to_raster and to_get_mode. to_raster converts data.frame input into raster. The input needs to have ncol = 2359, nrow = 1152.

Step 3. LOAD WORKING MAPS
(sub-step 3.1) Users need to define the working directory  before proceeding into subsequent steps. The default working directory is ("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset"). Alternatively, to work in the GPEM-LSEC2 server, users need to change the working directory into  ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset"). If problem appears during Load the input from "../input/maps/", check the working directory on step 3.1. Make sure you have the correct working directory that contains "../input/maps/" folder.(step 3.1).

(Sub-Step 3.2) New independent variables can be added using "[variable_dataset] <- raster( "input/maps/variable.asc")". The required file type is ASCII. The ASCII file can be prepared by exporting original Raster file in GIS into ASCII file with standard extent and spatial projection similar to land-use map 1999. After Loading the inputs, convert the raster maps into data frame, and simplify the name of headers. 

(Sub-Step 3.3) Convert dataframe with original unit into new values with [0 1] range of values. Exclude the protection and recreation areas (constraints). 
(Optional) Plot the histogram and maps of the input independent variables.

Step 4. SIMULATION 
(sub-step 4.1) Set the simulation parameters. Two important simulation paramteres are nYearGap and tSimul. The nYearGap defines the simulation cycle for single run, whilst the tSimul defines how many cycle the simulation need to run. The default nYearGap is 17 and tSimul 1, which means the simulaiton will run for one cycle (tSimul=1) and each cycle represents 17 years.

(sub-step 4.2) Get the transition probability using the coefficiento f estimate and independent variable in stackMacroVar. Select the cell acording to current Land use where transition probability needs to be estimated. Then predict the transition probability using to_predict function. Whereever the transition probability is NA or total transition probability is 0, then return the transition probability to initial land use (initial land class gets 1 transition probability and the remaining land classes get zero 0).

(sub-step 4.3) This sub-step adjusts the transition probability according to nYearGap (one cycle of simulation period). The smaller the nYearGap, the less likely land use changes occur.

(sub-step 4.4) Core of the simulation. Run the simulation using the transition probability produced by multinomial logistic regression. Users need to set the number of simulation instances (nSimulation). To determine the selected land use classes, the random multinomial (rmultinom) was used. Apply rmultinom over row [with binary results of 0 and 1] and multiple the binary result with the 13 land use classes using cross-product. To speed up the simulation instances, parallel computing is used. Simulate the model in gpem-lsec2 to get the advantage of multi-cores computer and hence speed-up the simulation. Once simulation instances are obtained, the "to_get_mode" is implemented to remove the salt-and-pepper effect.

(sub-step 4.5) Update the neighborhood urban ratio. The Default windows is 5 by 5. Function to_neighUrb" gives 100 when all the 5 by 5 neighbouring cells are urban.


Step 5. ACCURACY ASSESSMENT USING KAPPA
(sub-step 5.1) Accuracy assessment using kappa and statistical accuracy. Users need to define the initialLU (input land use map at t=0), actualLU (actual land use map as reference), and simultLU (the simulate land use to be compared with the referenced map).

(sub-step 5.2) Locational accuracy assessment using missed-hit and correct-hit indices. 

Step 6. EXPORT MAP 
Export the simulation results and their accuracy assessent into .asc, .grd, .tif. Folder's name is YYYYMMDD based on the simulation day.


5. How to run koala_ca_forward
-----------------------------------
Similar to koala_ca_simulate but with planning scheme 2017 as the contrainst.


0. Start
1. Checking and installing the required packages
2. Functions 
3. Load input
4. Simulation
6. Export maps for visualization

Step 0. START 
Remove existing variables in the workspace including figures (if any)

Step 1. CHECKING AND INSTALLING REQUIRED PACKAGES
Instal the required packages to run the module. 

Step 2. FUNCTIONS
Define functions for running the koala_ca_simulate. The functions start with "to_" to differentiate with variables. Frequently used functions are to_raster and to_get_mode. to_raster converts data.frame input into raster. The input needs to have ncol = 2359, nrow = 1152.

Step 3. LOAD WORKING MAPS
(sub-step 3.1) Users need to define the working directory  before proceeding into subsequent steps. The default working directory is ("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset"). Alternatively, to work in the GPEM-LSEC2 server, users need to change the working directory into  ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset"). If problem appears during Load the input from "../input/maps/", check the working directory on step 3.1. Make sure you have the correct working directory that contains "../input/maps/" folder.(step 3.1).


(Sub-Step 3.2) New independent variables can be added using "[variable_dataset] <- raster( "input/maps/variable.asc")". The required file type is ASCII. The ASCII file can be prepared by exporting original Raster file in GIS into ASCII file with standard extent and spatial projection similar to land-use map 1999. After Loading the inputs, convert the raster maps into data frame, and simplify the name of headers. The modulem also loads the planning scheme 2017 table that contains binary values [0 or 1] according to zoning description and land classes. 

(Sub-Step 3.3) Convert dataframe with original unit into new values with [0 1] range of values. Exclude the protection and recreation areas (constraints). 
(Optional) Plot the histogram and maps of the input independent variables.

Step 4. SIMULATION 
(sub-step 4.1) Set the simulation parameters. Two important simulation paramteres are nYearGap and tSimul. The nYearGap defines the simulation cycle for single run, whilst the tSimul defines how many cycle the simulation need to run. The default nYearGap is 17 and tSimul 1, which means the simulaiton will run for one cycle (tSimul=1) and each cycle represents 17 years.

(sub-step 4.2) Get the transition probability using the coefficiento f estimate and independent variable in stackMacroVar. Select the cell acording to current Land use where transition probability needs to be estimated. Then predict the transition probability using to_predict function. Whereever the transition probability is NA or total transition probability is 0, then return the transition probability to initial land use (initial land class gets 1 transition probability and the remaining land classes get zero 0).
Applying the planning scheme as contraint.  Development of a land class that can occur on particular zoning area receive 1 in constraint matrix, whereas development that are not allowed in particular zone receive zero (0). Hence, Multipling the transition probability with the constraint matrix will adjust the transition probability according to planning scheme 2017.  the Adjust the transition probability to total of one according to current land use class.

(sub-step 4.3) This sub-step adjusts the transition probability according to nYearGap (one cycle of simulation period). The smaller the nYearGap, the less likely land use changes occur.

(sub-step 4.4) Core of the simulation. Run the simulation using the transition probability produced by multinomial logistic regression. Users need to set the number of simulation instances (nSimulation). To determine the selected land use classes, the random multinomial (rmultinom) was used. Apply rmultinom over row [with binary results of 0 and 1] and multiple the binary result with the 13 land use classes using cross-product. To speed up the simulation instances, parallel computing is used. Simulate the model in gpem-lsec2 to get the advantage of multi-cores computer and hence speed-up the simulation. Once simulation instances are obtained, the "to_get_mode" is implemented to remove the salt-and-pepper effect.

(sub-step 4.5) Update the neighborhood urban ratio. The Default windows is 5 by 5. Function to_neighUrb" gives 100 when all the 5 by 5 neighbouring cells are urban.


 

Step 6. EXPORT MAP 
Export the simulation results and their accuracy assessent into .asc, .grd, .tif. Folder's name is YYYYMMDD based on the simulation day.




Support
=======
If you any inquiries about the model, please send your email to a.wahyudi1@uq.edu.au or agung.jobs@gmail.com.

Have fun





Brisbane, December 2018
Agung Wahyudi





