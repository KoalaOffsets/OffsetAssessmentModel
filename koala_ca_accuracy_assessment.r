## Input: land cover 1999 and 2016; macro factors, MLR coefficients
## Analysis:
## Author: Agung Wahyudi & Jonathan Rhodes
## Date first created: 06/08/2018
##
## About the ca model:
## The initial structure of the ca model
## was inspired by simlander/apolus.
## Different to simlander, current ca model
## takes into account multiple-change pathways
## Modification: "removal of sample points of less than 2" on cross-tab

## land use codes used

# 10	Conservation
# 21	Grazing native vegetation
# 22	Cropping and modified pasture
# 23	Other agricultural
# 30	Forestry
# 40	Rural residential
# 51	Low-density urban residential
# 52	Medium-density urban residential
# 53	High-density urban residential
# 60	Intensive urban area
# 71	Intensive agriculture
# 72	Industrial
# 80	water

## 0. START ============================================================

rm(list=ls())
if(!is.null(dev.list())) dev.off()

## Set working directory
#setwd("XXX")

## 1. CHECKING AND INSTALLING REQUIRED PACKAGES ========================

library(tidyverse)
library(raster)
library(nnet)
library(grDevices)
library(diffeR)
library(snowfall)
library(foreach)
library(doParallel)
library(vcd)

## 2. FUNCTIONS =====================================================

# function to create raster from data frame
to_raster <- function(df, rast_template) {
  NewRast <- raster(t(matrix(df, ncol = dim(rast_template)[1], nrow = dim(rast_template)[2])))
  extent(NewRast) <- extent(rast_template)
  projection(NewRast) <- projection(rast_template)
  return(NewRast)
}

# to create a cross tabulation based on two raster maps with categorical values (does not need to have similar amount of classes)
# columns are observed, rows are predicted
to_crosstab <- function (obs, pred, rast_template) {
  ctable <- crosstabm(to_raster(pred, rast_template), to_raster(obs, rast_template))
  return (ctable)
}

Mode <- function(x) {
    ux <- unique(x)
    ux=ux[!is.na(ux)]
    ux[which.max(tabulate(match(x, ux)))]
  }

## SET UP DATA

## Load maps, data, and lookup tables ####
lu1999 <- raster("input/maps/landuse99reclsuburb4.asc") # land use in 1999
lu2016 <- raster("input/maps/landuse16reclsuburb4.asc") # land use 2016
slope <- raster("input/maps/slpfinal1.asc") # slope
elev <- raster("input/maps/demfinal.asc") # elevation
road <- raster("input/maps/SEQ_distRoad.asc") # distance to main roads
city <- raster("input/maps/seq_cityDist.asc") # distance to cities
roadDen <- raster("input/maps/seq_roadDens.asc") # road density (all roads)
awc <- raster("input/maps/seq_awcMeans.asc") # soil mean available water content
awc[is.na(lu1999)] <- NA # remove zeros outside the study area
cly <- raster("input/maps/seq_clymeans1.asc") # soil clay content
NeighUrb <- raster("input/maps/lu99rdevn1.asc") # proportion of residential dev (51,52,53) in a 5 x 5 moving window (ignoring NAs)
NeighInd <- raster("input/maps/lu99indn1.asc") # proportion of commercial and industrial (60,72) in a 5 x 5 moving window (ignoring NAs)
UFfact <- raster("input/maps/seq_urbanfootprint2017.asc") # [0:regional 40:rural 50:urban] from Shaping SEQ
UFfacttb <- read.csv("input/table/shapingseq_cons.csv", header = TRUE) # Shaping SEQ land uses
lgasfact <- raster("input/maps/lgas.asc") # LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan]

# apply names
names(lu1999) <- "lu1999"
names(lu2016) <- "lu2016"
names(slope) <- "slope"
names(elev) <- "elev"
names(road) <- "road"
names(city) <- "city"
names(roadDen) <- "roadDen"
names(awc) <- "awc"
names(cly) <- "cly"
names(NeighUrb) <- "NeighUrb"
names(NeighInd) <- "NeighInd"
names(UFfact) <- "UFfact"
names(lgasfact) <- "lgasfact"

# compile predictors into a data frame
Predictors <- as.data.frame(stack(c(slope, elev, road, city, roadDen, awc, cly, NeighUrb, NeighInd, UFfact, lgasfact)))
# scale data the same way that they are scaled in the fitting process
Predictors <- Predictors %>% mutate_at(c("slope", "elev", "road", "city", "roadDen", "awc", "cly", "NeighUrb", "NeighInd"), ~(scale(.)))
# convert to factors
Predictors$UFfact <- as.factor(Predictors$UFfact)
Predictors$lgasfact <- as.factor(Predictors$lgasfact)

# get transition probability models
luLabel <- c(21,22,23,40,51,52,53,60,71,72)
luLabelList <- list()
TMods <- list()
for (i in 1:length(luLabel)){
  fileName = paste("input/mlrsummary/model", i, ".rda", sep="")
  load(fileName)
  TMods[[i]] <- temp_mod
  luLabelList[[i]] <- luLabel[i]
}

# set up current land use
lucurr <- lu1999 ##set current land use
lu1999.df <- as.data.frame(lu1999) # land use in data from format
lu2016.df <- as.data.frame(lu2016) # land use in data from format
lucurr.df <- lu1999.df # set current land use as a data frame
names(lucurr) <- "lucurr"
names(lu1999.df) <- "lucurr"
names(lucurr.df) <- "lucurr"

MaxIter <- 100
# current land use raster
LandUsePredList <- list()

#loop through iterations to run simulation

for (i in 1:MaxIter) {
  Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})

  # simulate transitions
  NewLU.df <- lucurr.df
  NewLUTemp <- unlist(lapply(Predictions, FUN = function(y) {apply(y, 1, FUN = function(x) {if (any(is.na(x))) {NA} else {as.integer(names(x)[which(rmultinom(1, 1, x) == 1)])}})}))
  NewLU.df$lucurr[as.integer(names(NewLUTemp[which(!is.na(NewLUTemp))]))] <- NewLUTemp[which(!is.na(NewLUTemp))]

  # record new land use in list
  LandUsePredList[[i]] <- NewLU.df
}

# save actual and predicted land-uses in 2016
saveRDS(lu1999.df, "sim_results/accuracy_assessment/lu1999.rds")
saveRDS(lu2016.df, "sim_results/accuracy_assessment/lu2016.rds")
saveRDS(LandUsePredList,"sim_results/accuracy_assessment/predictions.rds")

# read in simulations
Past <- readRDS("sim_results/accuracy_assessment/lu1999.rds")
Current <- readRDS("sim_results/accuracy_assessment/lu2016.rds")
Simulated <- readRDS("sim_results/accuracy_assessment/predictions.rds")

# create raster stack
for (i in 1:length(Simulated)){
  if (i == 1) {
    SimStack <- to_raster(Simulated[[i]]$lucurr, lu2016)
  } else {
    SimStack <- addLayer(SimStack, to_raster(Simulated[[i]]$lucurr, lu2016))
  }
}

# get the model of the simulations
SimMode <- calc(SimStack, fun = Mode)

# create error matrix based on the modal land use of the simulations
# actual in the columns and simulated in the rows
Obs <- Current$lu2016
Pred <- SimMode$layer
Obs[(Past$lucurr == 10) | (Past$lucurr == 30) | (Past$lucurr == 80)] <- NA
Pred[(Past$lucurr == 10) | (Past$lucurr == 30) | (Past$lucurr == 80)] <- NA
ctable.act.sim <- to_crosstab(obs = Obs, pred = Pred, lu2016) # crosstab between actual vs simulated LU map
write.csv(ctable.act.sim, "sim_results/accuracy_assessment/error_matrix.csv")
