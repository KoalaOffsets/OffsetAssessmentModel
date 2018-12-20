## Input: MLR_dataxx.mat from matlab
## Analysis: Multinomial logistic regression
## Author: Agung Wahyudi
## Date first created: 08/08/2018
## Ensure you instal R.matlab
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




## 2. LOAD WORKING MAPS =================== 
setwd("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd("M:/Projects/koala_offsets/02 Map/Quant_analysis")


##__2.1 -- Load sampling points. OBSELETE. 20/11/2018 MATLAB ####
# files <- list.files("./input/mlr_201811", pattern = '*mat', full.names=T)
# files <- files[2:length(files)]

##__2.2 -- Load coefficients table ####

luLabel <- c(10,21,22,23,30,40,51,52,53,60,71,72,80)


## 3. START MLR for EACH LU in 1999 =================== 


for (i in 1:length(luLabel)){
  print(i)
  
  # matlabFile      <- readMat(files[i])
  # varNames        <- names(matlabFile$mlr.Data[,,1])
  # datList         <- matlabFile$mlr.Data
  # datList         <- lapply(datList, unlist, use.names=FALSE)
  # macroVar        <- as.data.frame(datList)
  # names(macroVar) <- varNames
  
  fileName = paste("./input/mlr_201811/mlr_data", luLabel[i], ".rda", sep="")
  load(fileName)
  macroVar <- mlr_Dummy.df
  remove(mlr_Dummy.df)
  
  
  
  print("-- multinomial fitting process ")
  
  ## sort the data based on Land cover (LC) variable. Sort based on ?? similar LC (no change)
  ## example: if we load mlr_data30 (forest); then sorting will be based on no change land cover (0)
  ## convert categorical data into factor, 
  
  macroVar$LCfact   <- factor(macroVar$lu2016)  
  # macroVar$sa4fact  <- factor(macroVar$sa4) 
  macroVar$UFfact   <- factor(macroVar$UF)
  # macroVar$ptfact   <- factor(macroVar$Protect)
  # macroVar$rcfact   <- factor(macroVar$Recre)
  macroVar$plan2010fact   <- factor(macroVar$plan2010)
  
  # length(levels(macroVar$sa4fact ))
  # length(levels(macroVar$UFfact ))
  # length(levels(macroVar$ptfact ))
  # length(levels(macroVar$rcfact ))
  
  levels(macroVar$LCfact)
  
  macroVar$LCsort <- relevel(macroVar$LCfact, ref = "51") # sort? kind of? reference level
  
  
  
  ##__3.1 -- Ind. Var. + PatchDensity ####
  # ##____3.1.1 ---- set up the MLR function and independent variables ####
  # print("Coefficient estimates ")
  # 
  # test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + ptchDen, data = macroVar, maxit = 100)  #maximum iteration 1000
  # # test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + ptchDen + NeighUrb, data = macroVar, maxit = 100)  #maximum iteration 1000
  # # test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + ptchDen + NeighUrb + sa4fact + UFfact , data = macroVar, maxit = 100)  #maximum iteration 1000
  # # test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + ptchDen + NeighUrb + UFfact , data = macroVar, maxit = 100)  #maximum iteration 1000
  # 
  # # summary(test)
  # 
  # 
  # # test$residuals = c()
  # # test$fitted.values = c()
  # # test$weights = c()
  # 
  # 
  # fileNametest <- paste('./input/mlrsummary_patchDens/coefficient',i,'.rda', sep = "")
  # 
  # save (test, file = fileNametest)
  # 
  # 
  # ##____3.1.2 ---- save to txt file ####
  # 
  # fileNametest <- paste('./input/mlrsummary_patchDens/coefficient',i,'.txt', sep = "")
  # capture.output(coef(test), append = FALSE, file = fileNametest)
  # 
  # 
  # fileNametestlevel <- paste('./input/mlrsummary_patchDens/coefficientlevel',i,'.txt', sep = "")
  # capture.output(test$lev, file = fileNametestlevel)
  # print("-- end")
  # 
  # 
  # 
  # 
  # ##____3.1.3 ---- z-value calculation ####
  # ## The multinom package does not include p-value calculation
  # ## for the regression coefficients, so we calculate p-values 
  # ## using Wald tests (here z-tests).
  # 
  # 
  # 
  
  #   print("-- estimate the Z-score ")
  #   z <- summary(test)$coefficients/summary(test)$standard.errors
  # 
  # 
  #   fileNameZ <- paste('./input/mlrsummary_patchDens/z/z',i,'.rda', sep = "")
  #   writeMat (fileNameZ, z = z)
  #   fileNameZ <- paste('./input/mlrsummary_patchDens/z/z',i,'.txt', sep = "")
  #   capture.output(z, file = fileNameZ)
  # 
  # 
  # 
  # #   # # 2-tailed z test
  # #
  # #   ##____3.1.4 ---- p-value calculation ####
  # 
  #   print("-- estimate the p-values ")
  #   p <- (1 - pnorm(abs(z), 0, 1)) * 2
  # 
  # 
  #   fileNamep <- paste('./input/mlrsummary_patchDens/p/p',i,'.rda', sep = "")
  #   writeMat (fileNamep, p = p)
  #   fileNamep <- paste('./input/mlrsummary_patchDens/p/p',i,'.txt', sep = "")
  #   capture.output(p, file = fileNamep)
  # 
  #   ## extract the coefficients from the model and exponentiate
  #   ## exp(coef(test))
  #   ## You can calculate predicted probabilities for each of our outcome levels using the fitted function.
  #   ## head(pp <- fitted(test))
  #   
  #   
  #   
  # 
  #   
  # ##__3.2 -- Ind. Var. + NeighUrb ####
  #   ##____3.2.1 ---- set up the MLR function and independent variables ####
  #   print("Coefficient estimates using slope + elev + road + city + roadDen + awc + cly + NeighUrb")
  #   
  #   
  #   test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + NeighUrb, data = macroVar, maxit = 100)  #maximum iteration 1000
  #   
  #   
  #   
  #   # test$residuals = c()
  #   # test$fitted.values = c()
  #   # test$weights = c()
  #   
  #   
  #   fileNametest <- paste('./input/mlrsummary_NeighUrb/coefficient',i,'.rda', sep = "")
  #   
  #   save (test, file = fileNametest)
  #   
  #   
  #   ##____3.2.2 ---- save to txt file ####
  #   
  #   fileNametest <- paste('./input/mlrsummary_NeighUrb/coefficient',i,'.txt', sep = "")
  #   capture.output(coef(test), append = FALSE, file = fileNametest)
  #   
  #   
  #   fileNametestlevel <- paste('./input/mlrsummary_NeighUrb/coefficientlevel',i,'.txt', sep = "")
  #   capture.output(test$lev, file = fileNametestlevel)
  #   print("-- end")
  #   
  #   
  # 
  #   
  #   ##____3.2.3 ---- z-value calculation ####
  #   ## The multinom package does not include p-value calculation
  #   ## for the regression coefficients, so we calculate p-values 
  #   ## using Wald tests (here z-tests).
  # 
  # 
  # 
  # 
  #   print("-- estimate the Z-score ")
  #   z <- summary(test)$coefficients/summary(test)$standard.errors
  # 
  # 
  #   fileNameZ <- paste('./input/mlrsummary_NeighUrb/z/z',i,'.rda', sep = "")
  #   writeMat (fileNameZ, z = z)
  #   fileNameZ <- paste('./input/mlrsummary_NeighUrb/z/z',i,'.txt', sep = "")
  #   capture.output(z, file = fileNameZ)
  # 
  # 
  # 
  # #   # # 2-tailed z test
  # #
  # #   ##____3.2.4 ---- p-value calculation ####
  # 
  #   print("-- estimate the p-values ")
  #   p <- (1 - pnorm(abs(z), 0, 1)) * 2
  # 
  # 
  #   fileNamep <- paste('./input/mlrsummary_NeighUrb/p/p',i,'.rda', sep = "")
  #   writeMat (fileNamep, p = p)
  #   fileNamep <- paste('./input/mlrsummary_NeighUrb/p/p',i,'.txt', sep = "")
  #   capture.output(p, file = fileNamep)
  # 
  #   ## extract the coefficients from the model and exponentiate
  #   ## exp(coef(test))
  #   ## You can calculate predicted probabilities for each of our outcome levels using the fitted function.
  #   ## head(pp <- fitted(test))
  #   
  #   
  #   
  #   ##__3.3 -- Ind. Var. ptchDen + NeighUrb ####
  #   ##____3.3.1 ---- set up the MLR function and independent variables ####
  #   print("Coefficient estimates using slope + elev + road + city + roadDen + awc + cly + ptchDen + NeighUrb")
  #   
  #   test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + + ptchDen + NeighUrb, data = macroVar, maxit = 100)  #maximum iteration 1000
  #   
  #   
  #   
  #   # test$residuals = c()
  #   # test$fitted.values = c()
  #   # test$weights = c()
  #   
  #   
  #   fileNametest <- paste('./input/mlrsummary_patchDens_NeighUrb/coefficient',i,'.rda', sep = "")
  #   
  #   save (test, file = fileNametest)
  #   
  #   
  #   ##____3.3.2 ---- save to txt file ####
  #   
  #   fileNametest <- paste('./input/mlrsummary_patchDens_NeighUrb/coefficient',i,'.txt', sep = "")
  #   capture.output(coef(test), append = FALSE, file = fileNametest)
  #   
  #   
  #   fileNametestlevel <- paste('./input/mlrsummary_patchDens_NeighUrb/coefficientlevel',i,'.txt', sep = "")
  #   capture.output(test$lev, file = fileNametestlevel)
  #   print("-- end")
  #   
  #   
  # 
  #   
  #   ##____3.3.3 ---- z-value calculation ####
  #   ## The multinom package does not include p-value calculation
  #   ## for the regression coefficients, so we calculate p-values 
  #   ## using Wald tests (here z-tests).
  # 
  # 
  # 
  # 
  #   print("-- estimate the Z-score ")
  #   z <- summary(test)$coefficients/summary(test)$standard.errors
  # 
  # 
  #   fileNameZ <- paste('./input/mlrsummary_patchDens_NeighUrb/z/z',i,'.rda', sep = "")
  #   writeMat (fileNameZ, z = z)
  #   fileNameZ <- paste('./input/mlrsummary_patchDens_NeighUrb/z/z',i,'.txt', sep = "")
  #   capture.output(z, file = fileNameZ)
  # 
  # 
  # 
  # #   # # 2-tailed z test
  # #
  # #   ##____3.3.4 ---- p-value calculation ####
  # 
  #   print("-- estimate the p-values ")
  #   p <- (1 - pnorm(abs(z), 0, 1)) * 2
  # 
  # 
  #   fileNamep <- paste('./input/mlrsummary_patchDens_NeighUrb/p/p',i,'.rda', sep = "")
  #   writeMat (fileNamep, p = p)
  #   fileNamep <- paste('./input/mlrsummary_patchDens_NeighUrb/p/p',i,'.txt', sep = "")
  #   capture.output(p, file = fileNamep)
  # 
  #   ## extract the coefficients from the model and exponentiate
  #   ## exp(coef(test))
  #   ## You can calculate predicted probabilities for each of our outcome levels using the fitted function.
  #   ## head(pp <- fitted(test))
  
  
  
  
  
  #   ##__3.4 -- Ind. Var. ptchDen + NeighUrb +plan2010 ####
  
  ##____3.4.1 ---- set up the MLR function and independent variables ####
  # print("Coefficient estimates ")
  # 
  # test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + ptchDen + NeighUrb + plan2010fact , data = macroVar, maxit = 100)  #maximum iteration 1000
  # 
  # 
  # fileNametest <- paste('./input/mlrsummary_plan2010/coefficient',i,'.rda', sep = "")
  # 
  # save (test, file = fileNametest)
  # 
  # 
  # ##____3.4.2 ---- save to txt file ####
  # 
  # fileNametest <- paste('./input/mlrsummary_plan2010/coefficient',i,'.txt', sep = "")
  # capture.output(coef(test), append = FALSE, file = fileNametest)
  # 
  # 
  # fileNametestlevel <- paste('./input/mlrsummary_plan2010/coefficientlevel',i,'.txt', sep = "")
  # capture.output(test$lev, file = fileNametestlevel)
  # print("-- end")
  
  
  
  
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
  
  
  #   ##__3.5 -- (Drop ptchDen) + NeighUrb + UF ####
  
  ##____3.5.1 ---- set up the MLR function and independent variables ####
  print("Coefficient estimates ")
  
  test <- multinom(LCsort ~ slope + elev + road + city + roadDen + awc + cly + NeighUrb + UFfact , data = macroVar, maxit = 100)  #maximum iteration 1000
  
  
  fileNametest <- paste('./input/mlrsummary_NeighUrb_UF/coefficient',i,'.rda', sep = "")
  
  save (test, file = fileNametest)
  
  
  ##____3.5.2 ---- save to txt file ####
  
  fileNametest <- paste('./input/mlrsummary_NeighUrb_UF/coefficient',i,'.txt', sep = "")
  capture.output(coef(test), append = FALSE, file = fileNametest)
  
  
  fileNametestlevel <- paste('./input/mlrsummary_NeighUrb_UF/coefficientlevel',i,'.txt', sep = "")
  capture.output(test$lev, file = fileNametestlevel)
  print("-- end")
  
  
  
  
}

