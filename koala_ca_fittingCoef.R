## Input: ./input/mlr_201811/mlr_data
## Analysis: Multinomial logistic regression
## Author: Agung Wahyudi
## Date first created: 08/08/2018
## 
## Date modified:
## - 15/oct/2018 Adding the neighbourhood urban as independent variable
## - 01/Nov/2018 Adding contrainst/city council as binary independent variables (covariate)
## - 20/11/2018 Get sampling point from R (Koala_ca_selectSample)


## 0. START ============================================================

# Remove all variables
rm(list=ls())
if(!is.null(dev.list())) dev.off()



## 1. CHECKING AND INSTALLING REQUIRED PACKAGES ========================


if (!require("foreign"))  install.packages("foreign") ; library("foreign")
if (!require("nnet"))     install.packages("nnet")    ; library("nnet")
if (!require("reshape"))  install.packages("reshape") ; library("reshape")
if (!require("R.matlab")) install.packages("R.matlab"); library("R.matlab")
if (!require("foreign"))  install.packages("foreign") ; library("foreign")
if (!require("raster"))   install.packages("raster")  ; library("raster")




## 2. SPECIFY WORKING FOLDER  =================== 
setwd("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd("M:/Projects/koala_offsets/02 Map/Quant_analysis")


##__2.1 -- Load sampling points. OBSELETE. 20/11/2018 MATLAB ####


##__2.2 -- Load coefficients table ####

luLabel <- c(10,21,22,23,30,40,51,52,53,60,71,72,80)


## 3. START MLR for EACH LU in 1999 =================== 


for (i in 1:length(luLabel)){
  print(paste("Fitting coefficient and estimate probability of ", luLabel[i]))
  
  fileName = paste("./input/mlr_201811/mlr_data", luLabel[i], ".rda", sep="")
  load(fileName)
  macroVar <- mlr_Dummy.df
  remove(mlr_Dummy.df)
  
  
  
  ## sort the data based on Land cover (LC) variable. Sort based on ?? similar LC (no change)
  ## example: if we load mlr_data30 (forest); then sorting will be based on no change land cover (0)
  ## convert categorical data into factor, 
  
  macroVar$LCfact         <- factor(macroVar$lu2016)  
  macroVar$sa4fact        <- factor(macroVar$sa4) 
  macroVar$UFfact         <- factor(macroVar$UF)
  macroVar$plan2010fact   <- factor(macroVar$plan2010)
  macroVar$plan2017fact   <- factor(macroVar$plan2017)
  
  
  # length(levels(macroVar$sa4fact ))
  # length(levels(macroVar$UFfact ))
  # length(levels(macroVar$ptfact ))
  # length(levels(macroVar$rcfact ))
  
  levels(macroVar$LCfact)
  
  macroVar$LCsort <- relevel(macroVar$LCfact, ref = "51") # sort? kind of? reference level
  
  ##____3.5.1 ---- set up the MLR function and independent variables ####
  print("Coefficient estimates ")
  
  test <- multinom(LCsort ~ slope + 
                     elev + 
                     road + 
                     city + 
                     roadDen + 
                     awc + 
                     cly + 
                     NeighUrb + 
                     UFfact +
                     sa4fact , 
                   data = macroVar,
                   na.action = na.exclude,
                   maxit = 150) 
  
  fileNametest      <- paste('./input/mlrsummary_NeighUrb_UF/coefficient',i,'.rda', sep = "")
  fileNametestcoef  <- paste('./input/mlrsummary_NeighUrb_UF/coefficient',i,'.txt', sep = "")
  fileNametestlevel <- paste('./input/mlrsummary_NeighUrb_UF/coefficientlevel',i,'.txt', sep = "")
  
  save (test, file = fileNametest)
  capture.output(coef(test), append = FALSE, file = fileNametestcoef)
  capture.output(test$lev, file = fileNametestlevel)
  
}






























 
  ##____3.4.3 ---- z-value calculation ####
  ## The multinom package does not include p-value calculation
  ## for the regression coefficients, so we calculate p-values 
  ## using Wald tests (here z-tests).
  
  # print("-- estimate the Z-score ")
  # z <- summary(test)$coefficients/summary(test)$standard.errors
  # 
  # 
  # fileNameZ <- paste('./input/mlrsummary_plan2010/z/z',i,'.rda', sep = "")
  # writeMat (fileNameZ, z = z)
  # fileNameZ <- paste('./input/mlrsummary_plan2010/z/z',i,'.txt', sep = "")
  # capture.output(z, file = fileNameZ)
  # 
  # 
  # 
  # #   # # 2-tailed z test
  # #
  # #   ##____3.4.4 ---- p-value calculation ####
  # 
  # print("-- estimate the p-values ")
  # p <- (1 - pnorm(abs(z), 0, 1)) * 2
  # 
  # 
  # fileNamep <- paste('./input/mlrsummary_plan2010/p/p',i,'.rda', sep = "")
  # writeMat (fileNamep, p = p)
  # fileNamep <- paste('./input/mlrsummary_plan2010/p/p',i,'.txt', sep = "")
  # capture.output(p, file = fileNamep)
  
  ## extract the coefficients from the model and exponentiate
  ## exp(coef(test))
  ## You can calculate predicted probabilities for each of our outcome levels using the fitted function.
  ## head(pp <- fitted(test))
  
  


