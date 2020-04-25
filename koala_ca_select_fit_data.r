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

if (!require("tidyr"))    install.packages("tidyr")   ; library("tidyr")
if (!require("dplyr"))    install.packages("dplyr")   ; library("dplyr")
if (!require("raster"))   install.packages("raster")  ; library("raster")
if (!require("rJava"))    install.packages("rJava")   ; library("rJava")
if (!require("raster"))   install.packages("raster")  ; library("raster")
if (!require("RNetLogo")) install.packages("RNetLogo"); library("RNetLogo")
if (!require("nnet"))     install.packages("nnet")    ; library("nnet")
if (!require("grDevices"))install.packages("grDevices");library("grDevices")
if (!require("diffeR"))   install.packages("diffeR")  ;library("diffeR")

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
  funselect <- function(x) { x[ x !=lucode] <- NA; return(x) } #extract only non existing land use (53 hi res urb)so that existing functional land uses are not 	randomized. Set everything else to NA
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

## 3. LOAD WORKING MAPS =================================================

##__3.1 -- Set working path $ DO CHANGE THIS PATH ####
setwd("R:/KOALA2018-A0206/model/CA-KoalaOffset")
# setwd ("M:/Projects/koala_offsets/model/CA-KoalaOffset") ## server gpem-lsec2

##__3.2 -- Load working maps ####
saArea <- raster("input/maps/seq_kpa_region.asc")
lu1999 <- raster("input/maps/landuse99reclsuburb4.asc")
lu2016 <- raster("input/maps/landuse16reclsuburb4.asc")
luChange <- raster("input/maps/seq_lndcovch4.asc") #[first two digits:lu1999 , last two digits: lu2016]
# Independent variables
slope_dataset <- raster("input/maps/slpfinal.asc")
elev_dataset <- raster("input/maps/seq_dem.asc")
road_dataset<- raster("input/maps/SEQ_distRoad.asc")
patchdens_dataset <- raster("input/maps/SEQ_patchdens.asc")
roaddens_dataset <- raster("input/maps/seq_roadDens.asc")
city_dataset <- raster("input/maps/seq_cityDist.asc")
water_dataset <- raster("input/maps/seq_awcMeans.asc")
water_dataset[is.na(lu1999)] <- NA # remove zeros outside the study area
clay_dataset <- raster("input/maps/seq_clymeans1.asc")
neighUrb_dataset <- raster("input/maps/lu99rdevn1.asc") # proportion of residential dev (51,52,53) in a 5 x 5 moving window (ignoring NAs)
neighInd_dataset <- raster("input/maps/lu99indn1.asc") # proportion of commercial and industrial (60,72) in a 5 x 5 moving window (ignoring NAs)
sa4_dataset <- raster("input/maps/seq_sa4_code11.asc")
urbanFootprint <- raster("input/maps/seq_urbanfootprint2017.asc") # [0:regional 40:rural 50:urban]
protect_area <- raster("input/maps/seq_protected_areas.asc") # [56:protect area]
recreation_area <- raster("input/maps/seq_recreation_areas.asc") #[2:Garden 3:GOlfCourse  4:SportComplex(Multiple use)  5:Miscellaneous Area(pool:caravan park)  6:oval(football, hockey)  7:racecourse  8:recreationarea 9:riflerange 10:showground 11:zoo]
sprp_ada <- raster("input/maps/seq_sprp_ada.asc") # [1:KADA, 2:PKADA]
kada_bush_uf <- raster("input/maps/seq_kada_bushland_outside_uf_hab_only.asc") # [2:HV Bushland, 5:LV Bushland 8:MV Bushland]
plan2010 <- raster("input/maps/seq_planning_scheme_2010.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$planning_scheme_2010 for description
plan2017 <- raster("input/maps/seq_planning_scheme_2017b.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$seq_planningScheme2017b for description
lgas <- raster("input/maps/lgas.asc") # LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan]

# Deal with missing data and odd values
sprp_ada[is.na(sprp_ada) & (!is.na(lu1999))] <- 0
kada_bush_uf[is.na(kada_bush_uf) & (!is.na(lu1999))] <- 0
protect_area[is.na(protect_area) & (!is.na(lu1999))] <- 0
recreation_area[is.na(recreation_area) & (!is.na(lu1999))] <- 0
recreation_area[recreation_area > 0] <- 1

# set data frame names
names(saArea) <- ("studyArea")
names(lu1999) <- ("lu1999")
names(lu2016) <- ("lu2016")
names(luChange) <- ("luChange")
names(slope_dataset) <- ("slope")
names(elev_dataset) <- ("elev")
names(road_dataset) <- ("road")
names(patchdens_dataset) <- ("ptchDen")
names(roaddens_dataset) <- ("roadDen")
names(city_dataset) <- ("city")
names(water_dataset) <- ("awc")
names(clay_dataset) <- ("cly")
names(neighUrb_dataset) <- ("NeighUrb")
names(neighInd_dataset) <- ("NeighInd")
names(sa4_dataset) <- ("sa4")
names(urbanFootprint) <- ("UF")
names(protect_area) <- ("protectArea")
names(recreation_area) <- ("recreArea")
names(sprp_ada) <- ("sprdpAda")
names(kada_bush_uf) <- ("kadaBushUF")
names(plan2010) <- ("plan2010")
names(plan2017) <- ("plan2017")
names(lgas) <- ("lgas")

MacroVar <- as.data.frame(stack(lu1999,
                                  lu2016,
                                  slope_dataset,
                                  elev_dataset,
                                  road_dataset,
                                  patchdens_dataset,
                                  roaddens_dataset,
                                  city_dataset,
                                  water_dataset,
                                  clay_dataset,
                                  neighUrb_dataset,
                                  neighInd_dataset,
                                  sa4_dataset,
                                  urbanFootprint,
                                  protect_area,
                                  recreation_area,
                                  sprp_ada,
                                  kada_bush_uf,
                                  plan2010,
                                  plan2017,
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
       patchdens_dataset,
       neighUrb_dataset,
       neighInd_dataset,
       sa4_dataset,
       urbanFootprint,
       protect_area,
       recreation_area,
       sprp_ada,
       kada_bush_uf,
       plan2010,
       plan2017,
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
stackMacroVar.df$ptchDen <- scale(MacroVar$ptchDen)
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
stackMacroVar.df$ptchDen[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$NeighUrb[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$NeighInd[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$sa4[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$UF[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$sprpAda[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$kadaBushUF[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$plan2010[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$plan2017[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA
stackMacroVar.df$lgas[((stackMacroVar.df$lu1999 == 10) | (stackMacroVar.df$lu1999 == 80) | (stackMacroVar.df$lu1999 == 30) | (stackMacroVar.df$lu2016 == 10) | (stackMacroVar.df$lu2016 == 80) | (stackMacroVar.df$lu2016 == 30))] <- NA

## 4. SAMPLING POINTS SELECTION =====================================================

print("-- Sampling point selection")

luLabel <- c(21,22,23,40,51,52,53,60,71,72)

for (i in 1:length(luLabel)){
  print(i)
  mlr_Dummy.df <- stackMacroVar.df %>%
    filter(lu1999 == luLabel[i])

  fileNametest <- paste('./input/mlr_2020/mlr_data',luLabel[i],'.rda', sep = "")
  save (mlr_Dummy.df, file = fileNametest)
}
