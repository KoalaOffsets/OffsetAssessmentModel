# CODE TO FIT CA MODEL

# land use codes used

# 10	Conservation
# 21	Grazing
# 22	Cropping and modified pasture
# 23	Other agricultural
# 30	Forestry
# 40	Rural residential
# 51	Low-density urban residential
# 52	Medium-density urban residential
# 53	High-density urban residential
# 60	Services, utilities, mining
# 71	Intensive agriculture
# 72	Industrial
# 80	Water

# clear objects in workspace
rm(list=ls())
gc()

# load pacakges
if (!require("foreign")) install.packages("foreign"); library("foreign")
if (!require("nnet")) install.packages("nnet"); library("nnet")
if (!require("reshape")) install.packages("reshape"); library("reshape")
if (!require("R.matlab")) install.packages("R.matlab"); library("R.matlab")
if (!require("foreign")) install.packages("foreign"); library("foreign")
if (!require("raster")) install.packages("raster"); library("raster")

# load functions
source("functions.r")

# create land-use labels
luLabel <- c(21, 22, 23, 40, 51, 52, 53, 60, 71, 72)

# create list to store models
Models <- vector(mode = "list")

# fit multinomial models
# loop through land-uses
for (i in 1:length(luLabel)){

  # load data
  fileName = paste("input/mlr/mlr_data", luLabel[i], ".rda", sep="")
  load(fileName)
  macroVar <- mlr_Dummy.df
  remove(mlr_Dummy.df)

  # convert categorcal variables to factors
  macroVar$LCfact <- factor(macroVar$lu2016)
  macroVar$LCsort <- relevel(macroVar$LCfact, ref = "51") # set the reference level to 51
  macroVar$UFfact <- factor(macroVar$UF) # urban footprint
  macroVar$lgasfact <- factor(macroVar$lgas) * LGAs

  # fit model
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

  # save models
  fileName <- paste("input/mlrsummary/model",i,".rda", sep = "")
  fileNamecoef <- paste("input/mlrsummary/coefficient",i,".txt", sep = "")
  fileNamelevel <- paste("input/mlrsummary/coefficientlevel",i,".txt", sep = "")
  save(temp_mod, file = fileName)
  capture.output(coef(temp_mod), append = FALSE, file = fileNamecoef)
  capture.output(temp_mod$lev, file = fileNamelevel)

  # add model to list
  Models[[i]] <- temp_mod
}
