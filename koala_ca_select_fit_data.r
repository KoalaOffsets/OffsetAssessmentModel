## Analysis:
## Author: Agung Wahyudi (modified by Jonathan Rhodes)
## Date first created: 20/11/2018
##
## About the ca model:
## The initial structure of the ca model
## was inspired by simlander/apolus.
## Different to simlander, current ca model
## takes into account multiple-change pathways
##
## This is a translation from MATLAB \Quant_analysis\subUrb_macroVar2.m

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

rm(list=ls())
if(!is.null(dev.list())) dev.off()

## 1. CHECKING AND INSTALLING REQUIRED PACKAGES ========================

library("tidyr")
library("dplyr")
library("raster")
library("rJava")
library("raster")
library("nnet")
library("grDevices")
library("diffeR")

## 2. FUNCTIONS =====================================================

# input df
to_raster <- function(df.dummy) {
  mat.dummy             <- raster(t(matrix(df.dummy, ncol = 2359, nrow = 1152)))
  extent(mat.dummy)     <- extent(sa4)
  projection(mat.dummy) <- projection(lu1999)
  return(mat.dummy)
}

# stats::predict, use lapply
to_predict <- function (coeff, newdata.df) {
  stats::predict(coeff, newdata = newdata.df, type = "probs", se = TRUE)
}

# to mask stacked raster according to LU1999 land classes code (lucode)
to_mask  <- function(lumap, lucode, stack.dummy) {
  funselect <- function(x) { x[ x !=lucode] <- NA; return(x) } #extract only non existing land use (53 hi res urb) so that existing functional land uses are not randomized. Set everything else to NA
  vacants   <- calc(lumap, funselect)
  tp.masked <- mask( stack.dummy, vacants) #new raster with value of x, except for the cells that are NA on mask.
  return (tp.masked)
}

# to rank using base::rank.
to_rank   <- function(x) {
  rank(-x, na.last = TRUE, ties.method =  "random")
}

# to select n-th (ludemand)  cells from x -> as land use demand
to_select <- function(x, ludemand) {
  x[x > ludemand] <- NA; return(x)
}

# to run simulation. Remove commented lines #, when saving the plot is required
to_simulate_mp <- function (lucode, tprank){
  for (i in 1:yearLength){
    urbdemand1  <- urbdemand[lucode] + annualdemand[lucode]*i
    testX       <- to_select(tprank, urbdemand1)
    #filen       <- paste("output/20180904/lu", lucode, "_", i, ".png", sep="")

    #png(filename = paste(filen),width = 1200, height = 1600, bg="white")
    plot(to_raster(testX), breaks=breakpoints,col=colors)
    #dev.off()

    print(paste(1999 + i,  "with land demand of", urbdemand1))
  }
}

to_count_cutOff <- function (cutOff, datamap) {
  dataDummy <- as.data.frame(datamap)
  z     <- dataDummy >= cutOff
  sumZ  <- sum(z, na.rm=TRUE)
  return (sumZ)
}

to_zero_one <- function (datasetDummy) {
  minDummy <- min(as.matrix(datasetDummy), na.rm = TRUE)
  maxDummy <- max(as.matrix(datasetDummy), na.rm = TRUE)
  if (minDummy < 0) {minDummy=0}
  converted_dataset <- (datasetDummy - minDummy)/(maxDummy - minDummy)
  return (converted_dataset)
}

to_get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

##__3.2 -- Load working maps ####
lu1999 <- raster("input/maps/landuse99reclsuburb4.asc")
lu2016 <- raster("input/maps/landuse16reclsuburb4.asc")
# Independent variables
slope_dataset <- raster("input/maps/slpfinal1.asc")
elev_dataset <- raster("input/maps/demfinal.asc")
road_dataset<- raster("input/maps/SEQ_distRoad.asc")
roaddens_dataset <- raster("input/maps/seq_roadDens.asc")
city_dataset <- raster("input/maps/seq_cityDist.asc")
water_dataset <- raster("input/maps/seq_awcMeans.asc")
water_dataset[is.na(lu1999)] <- NA # remove zeros outside the study area
clay_dataset <- raster("input/maps/seq_clymeans1.asc")
neighUrb_dataset <- raster("input/maps/lu99rdevn1.asc") # proportion of residential dev (51,52,53) in a 5 x 5 moving window (ignoring NAs)
neighInd_dataset <- raster("input/maps/lu99indn1.asc") # proportion of commercial and industrial (60,72) in a 5 x 5 moving window (ignoring NAs)
urbanFootprint <- raster("input/maps/seq_urbanfootprint_2006fin.asc") # [0:regional 40:rural 50:urban]
lgas <- raster("input/maps/lgas.asc") # LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan]

# set data frame names
names(lu1999) <- ("lu1999")
names(lu2016) <- ("lu2016")
names(slope_dataset) <- ("slope")
names(elev_dataset) <- ("elev")
names(road_dataset) <- ("road")
names(roaddens_dataset) <- ("roadDen")
names(city_dataset) <- ("city")
names(water_dataset) <- ("awc")
names(clay_dataset) <- ("cly")
names(neighUrb_dataset) <- ("NeighUrb")
names(neighInd_dataset) <- ("NeighInd")
names(urbanFootprint) <- ("UF")
names(lgas) <- ("lgas")

MacroVar <- as.data.frame(stack(lu1999,
                                  lu2016,
                                  slope_dataset,
                                  elev_dataset,
                                  road_dataset,
                                  roaddens_dataset,
                                  city_dataset,
                                  water_dataset,
                                  clay_dataset,
                                  neighUrb_dataset,
                                  neighInd_dataset,
                                  urbanFootprint,
                                  lgas))

# Get land use data into data frames type and calculate frequencies
lu1999.df <- as.data.frame(lu1999)
lu2016.df <- as.data.frame(lu2016)
freq1999 <- as.data.frame(freq(lu1999))
freq2016 <- as.data.frame(freq(lu2016))

# Get zero map
tp.zero <- lu1999 - lu1999

# remove original variables to save memory
remove(lu1999,
       lu2016,
       slope_dataset,
       elev_dataset,
       road_dataset,
       city_dataset,
       roaddens_dataset,
       water_dataset,
       clay_dataset,
       neighUrb_dataset,
       neighInd_dataset,
       urbanFootprint,
       lgas)
gc()

## __3.3 -- Conversion of factors to [0 1] , stacked, and df converted ########

stackMacroVar.df <- MacroVar

stackMacroVar.df$slope <- scale(MacroVar$slope)
stackMacroVar.df$elev <- scale(MacroVar$elev)
stackMacroVar.df$road <- scale(MacroVar$road)
stackMacroVar.df$city <- scale(MacroVar$city)
stackMacroVar.df$roadDen <- scale(MacroVar$roadDen)
stackMacroVar.df$awc <- scale(MacroVar$awc)
stackMacroVar.df$cly <- scale(MacroVar$cly)
stackMacroVar.df$NeighUrb <- scale(MacroVar$NeighUrb)
stackMacroVar.df$NeighInd <- scale(MacroVar$NeighInd)

# exclude areas that are either conservation (10), water (80), and forestry (30) - we are not modelling these transitions
stackMacroVar.df$lu2016[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$slope[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$elev[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$road[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$city[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$roadDen[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$awc[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$cly[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$NeighUrb[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$NeighInd[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$UF[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$lgas[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA

## 4. SAMPLING POINTS SELECTION =====================================================

luLabel <- c(21,22,23,40,51,52,53,60,71,72)

for (i in 1:length(luLabel)){
  print(i)
  mlr_Dummy.df <- stackMacroVar.df %>%
    filter(lu1999 == luLabel[i])

  fileNametest <- paste("input/mlr/mlr_data",luLabel[i],".rda", sep = "")
  save (mlr_Dummy.df, file = fileNametest)
}
