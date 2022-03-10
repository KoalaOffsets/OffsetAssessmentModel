## Input: /input/mlr/mlr_data
## Analysis: Multinomial logistic regression
## Author: Agung Wahyudi (modified by Jonathan Rhodes)
## Date first created: 08/08/2018
##
## Date modified:
## - 15/oct/2018 Adding the neighbourhood urban as independent variable
## - 01/Nov/2018 Adding contrainst/city council as binary independent variables (covariate)
## - 20/11/2018 Get sampling point from R (koala_ca_selectSample.r)

## land use codes used

# 10	Conservation
# 21	Grazing native vegetation
# 22	Cropping and modified pasture
# 23	Transition land
# 30	Forestry
# 40	Rural residential
# 51	Low-density urban residential
# 52	Medium-density urban residential
# 53	High-density urban residential
# 60	Commercial
# 71	Intensive agriculture
# 72	Industrial
# 80	Water

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

##__2.2 -- Load coefficients table ####

luLabel <- c(21,22,23,40,51,52,53,60,71,72)

## 3. START MLR for EACH LU in 1999 ===================

#list to store models
Models <- vector(mode = "list")

for (i in 1:length(luLabel)){
  print(paste("Fitting coefficients and estimating transition probabilities for land use", luLabel[i]))

  fileName = paste("input/mlr/mlr_data", luLabel[i], ".rda", sep="")
  load(fileName)
  macroVar <- mlr_Dummy.df
  remove(mlr_Dummy.df)

  macroVar$LCfact <- factor(macroVar$lu2016)
  macroVar$UFfact <- factor(macroVar$UF)
  macroVar$lgasfact <- factor(macroVar$lgas)
  macroVar$plan2010fact <- factor(macroVar$plan2010)
  macroVar$plan2017fact <- factor(macroVar$plan2017)

  macroVar$LCsort <- relevel(macroVar$LCfact, ref = "51") # set the reference level to 51

  ##____3.5.1 ---- set up the MLR function and independent variables ####
  print("Coefficient estimates ")

  temp_mod <- multinom(LCsort ~ slope +
                     elev +
                     road +
                     city +
                     roadDen +
                     awc + I(awc ^ 2) +
                     cly + I(cly ^ 2) +
                     NeighUrb +
                     NeighInd +
                     UFfact +
                     lgasfact,
                   data = macroVar,
                   na.action = na.exclude,
                   maxit = 1000)

  fileNametest <- paste("input/mlrsummary/model",i,".rda", sep = "")
  fileNametestcoef <- paste("input/mlrsummary/coefficient",i,".txt", sep = "")
  fileNametestlevel <- paste("input/mlrsummary/coefficientlevel",i,".txt", sep = "")

  save(temp_mod, file = fileNametest)
  capture.output(coef(temp_mod), append = FALSE, file = fileNametestcoef)
  capture.output(temp_mod$lev, file = fileNametestlevel)

  Models[[i]] <- temp_mod
}
