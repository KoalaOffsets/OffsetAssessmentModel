# CODE TO RUN THE ACCURACY ASSESSMENT FOR THE CA MODEL

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
library(tidyverse)
library(raster)
library(nnet)
library(grDevices)
library(diffeR)
library(snowfall)
library(foreach)
library(doParallel)
library(vcd)

# load functions
source("functions.r")

# set up data

# load maps, data, and lookup tables
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
UFfact <- raster("input/maps/seq_urbanfootprint_2006fin.asc") # [0:regional 40:rural 50:urban] from Shaping SEQ
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
# convert categorical variables to factors
Predictors$UFfact <- as.factor(Predictors$UFfact)
Predictors$lgasfact <- as.factor(Predictors$lgasfact)

# get transition probability models
luLabel <- c(21, 22, 23, 40, 51, 52, 53, 60, 71, 72)
luLabelList <- list()
TMods <- list()
for (i in 1:length(luLabel)){
  fileName = paste("input/mlrsummary/model", i, ".rda", sep="")
  load(fileName)
  TMods[[i]] <- temp_mod
  luLabelList[[i]] <- luLabel[i]
}

# set up current land use
lucurr <- lu1999 # set current land use
lu1999.df <- as.data.frame(lu1999) # land use in data from format
lu2016.df <- as.data.frame(lu2016) # land use in data from format
lucurr.df <- lu1999.df # set current land use as a data frame
names(lucurr) <- "lucurr"
names(lu1999.df) <- "lucurr"
names(lucurr.df) <- "lucurr"

# set number of replicates
Iter <- 100

# create list to hold predicted land-uses
LandUsePredList <- list()

#loop through iterations to run simulation
for (i in 1:Iter) {
  Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})

  # simulate transitions
  NewLU.df <- lucurr.df
  NewLUTemp <- unlist(lapply(Predictions, FUN = function(y) {apply(y, 1, FUN = function(x) {if (any(is.na(x))) {NA} else {as.integer(names(x)[which(rmultinom(1, 1, x) == 1)])}})}))
  NewLU.df$lucurr[as.integer(names(NewLUTemp[which(!is.na(NewLUTemp))]))] <- NewLUTemp[which(!is.na(NewLUTemp))]

  # record new land-uses in list
  LandUsePredList[[i]] <- NewLU.df
}

# save actual and predicted land-uses in 2016
saveRDS(lu1999.df, "sim_results/accuracy_assessment/lu1999.rds")
saveRDS(lu2016.df, "sim_results/accuracy_assessment/lu2016.rds")
saveRDS(LandUsePredList,"sim_results/accuracy_assessment/predictions.rds")

# read in simulations if necessary
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

# get the mode of the simulated land-uses
SimMode <- calc(SimStack, fun = Mode)

# create error matrix based on the modal land use of the simulations
# actual in the columns and simulated in the rows
Obs <- Current$lu2016
Pred <- SimMode$layer
Obs[(Past$lucurr == 10) | (Past$lucurr == 30) | (Past$lucurr == 80)] <- NA
Pred[(Past$lucurr == 10) | (Past$lucurr == 30) | (Past$lucurr == 80)] <- NA
ctable.act.sim <- to_crosstab(obs = Obs, pred = Pred, lu2016) # crosstab between actual vs simulated LU map
write.csv(ctable.act.sim, "sim_results/accuracy_assessment/error_matrix.csv")
