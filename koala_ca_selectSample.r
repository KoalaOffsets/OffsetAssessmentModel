## Input: land cover 1999 and 2016; macro factors, MLR coefficients
## Analysis: 
## Author: Agung Wahyudi
## Date first created: 20/11/2018
## 
## About the ca model:
## The initial structure of the ca model 
## was inspired by simlander/apolus.
## Different to simlander, current ca model
## takes into account multiple-change pathways
## 
## This is a translation from MATLAB \Quant_analysis\subUrb_macroVar2.m

## 0. START ============================================================


rm(list=ls())
graphics.off()


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





## 2. LOAD WORKING MAPS ================================================= 


## 2.1 -- Set working path $ DO CHANGE THIS PATH ####
setwd ("C:/Users/uqawahy1/Documents/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset") ## server gpem-lsec2


## 2.2 -- Load working maps ####
landValue          <- raster("input/maps/seq_kpa_region.asc")
sa4                <- raster("input/maps/seq_sa4_code11.asc")
lu1999             <- raster("input/maps/landuse99reclsuburb4.asc")
lu2016             <- raster("input/maps/landuse16reclsuburb4.asc")




# Independent variables
slope_dataset      <- raster( "input/maps/seq_slope.asc")
elev_dataset       <- raster( "input/maps/seq_dem.asc")
road_dataset       <- raster( "input/maps/SEQ_distRoad.asc")
patchdens_dataset  <- raster( "input/maps/SEQ_patchdens.asc")
roaddens_dataset   <- raster( "input/maps/seq_roadDens.asc")
city_dataset       <- raster( "input/maps/seq_cityDist.asc")
water_dataset      <- raster( "input/maps/seq_awcMeans.asc")
clay_dataset       <- raster( "input/maps/seq_clymeans1.asc")
neighUrb_dataset   <- raster( "input/maps/seq_neighboururban.asc")
sa4_dataset        <- raster( "input/maps/seq_sa4_code11.asc")
urbanFootprint     <- raster( "input/maps/seq_urbanfootprint2017.asc") # [0:regional 40:rural 50:urban]
protect_area       <- raster( "input/maps/seq_protected_areas.asc") # [56:protect area]
recreation_area    <- raster( "input/maps/seq_recreation_areas.asc") #[2:Garden 3:GOlfCourse  4:SportComplex(Multiple use)  5:Miscellaneous Area(pool:caravan park)  6:oval(football, hockey)  7:racecourse  8:recreationarea  9:riflerange 10:showground 11:zoo]

sprp_ada           <- raster( "input/maps/seq_sprp_ada.asc") # [1:KADA, 2:PKADA]
kada_bush_uf       <- raster( "input/maps/seq_kada_bushland_outside_uf_hab_only.asc") # [2:HV Bushland, 5:LV Bushland 8:MV Bushland]
luChange           <- raster( "input/maps/seq_lndcovch4.asc") #[first two digits:lu1999 , last two digits: lu2016]
plan2010           <- raster( "input/maps/seq_planning_scheme_2010.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$planning_scheme_2010 for description


sprp_ada       [is.na(sprp_ada) & (!is.na(lu1999))]        = 0
kada_bush_uf   [is.na(kada_bush_uf) & (!is.na(lu1999))]    = 0
protect_area   [is.na(protect_area) & (!is.na(lu1999))]    = 0
recreation_area[is.na(recreation_area) & (!is.na(lu1999))] = 0
recreation_area[recreation_area > 0] = 1

# Simplify  variables' names
names(slope_dataset)    <- ("slope")
names(elev_dataset)     <- ("elev")
names(road_dataset)     <- ("road")
names(city_dataset)     <- ("city")
names(roaddens_dataset) <- ("roadDen")
names(water_dataset)    <- ("awc")
names(clay_dataset)     <- ("cly")
names(patchdens_dataset)<- ("ptchDen")
names(neighUrb_dataset) <- ("NeighUrb")
names(sa4_dataset)      <- ("sa4")
names(urbanFootprint)   <- ("UF")
names(protect_area)     <- ("protectArea")
names(recreation_area)  <- ("recreArea")

names(sprp_ada)         <- ("sprdpAda")
names(kada_bush_uf)     <- ("kadaBushUF")
names(luChange)         <- ("luChange")
names(plan2010)         <- ("plan2010")





MacroVar          <- as.data.frame(stack(slope_dataset,
                                         elev_dataset,
                                         road_dataset,
                                         city_dataset,
                                         roaddens_dataset,
                                         water_dataset,
                                         clay_dataset,
                                         patchdens_dataset,
                                         neighUrb_dataset,
                                         sa4_dataset,
                                         urbanFootprint,
                                         protect_area,
                                         recreation_area,
                                         plan2010))


lu1999.df     <- as.data.frame(lu1999)
lu2016.df     <- as.data.frame(lu2016)
freq1999      <- as.data.frame(freq(lu1999))
freq2016      <- as.data.frame(freq(lu2016))
tp.zero       <- lu1999 - lu1999 # zero map


MacroVar$lu1999     <- lu1999.df$landuse99reclsuburb4
MacroVar$lu2016     <- lu2016.df$landuse16reclsuburb4

remove(slope_dataset,
       elev_dataset,
       road_dataset,
       city_dataset,
       roaddens_dataset,
       water_dataset,
       clay_dataset,
       patchdens_dataset,
       neighUrb_dataset,
       sa4_dataset,
       urbanFootprint,
       protect_area,
       recreation_area)

## 3. FUNCTIONS ===================================================== 

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


## 4. DATA PREPARATION ===================================================== 

## 4.1 -- Conversion of factors to [0 1] , stacked, and df converted ########

stackMacroVar.df <- MacroVar 



stackMacroVar.df$slope    <- to_zero_one(MacroVar$slope)
stackMacroVar.df$elev     <- to_zero_one(MacroVar$elev)
stackMacroVar.df$road     <- to_zero_one(MacroVar$road)
stackMacroVar.df$roadDen  <- to_zero_one(MacroVar$roadDen)
stackMacroVar.df$city     <- to_zero_one(MacroVar$city)
stackMacroVar.df$awc      <- to_zero_one(MacroVar$awc)
stackMacroVar.df$cly      <- to_zero_one(MacroVar$cly)
stackMacroVar.df$ptchDen  <- to_zero_one(MacroVar$ptchDen)
stackMacroVar.df$NeighUrb <- to_zero_one(MacroVar$NeighUrb)

# exclude protected area and recreation area from the predicted/simulated area
stackMacroVar.df$slope    [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$elev     [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$road     [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$roadDen  [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$city     [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$awc      [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$cly      [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$ptchDen  [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$NeighUrb [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA

stackMacroVar.df$sa4      [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$UF       [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$plan2010 [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA

stackMacroVar.df$lu1999   [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$lu2016   [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA

# hist (stackMacroVar.df$slope )
# hist (stackMacroVar.df$elev )
# hist (stackMacroVar.df$road )
# hist (stackMacroVar.df$roadDen )
# hist (stackMacroVar.df$city )
# hist (stackMacroVar.df$awc )
# hist (stackMacroVar.df$cly )
# hist (stackMacroVar.df$ptchDen )
# hist (stackMacroVar.df$NeighUrb )

# plot(to_raster(stackMacroVar.df$slope))
# plot(to_raster(stackMacroVar.df$elev))
# plot(to_raster(stackMacroVar.df$road))
# plot(to_raster(stackMacroVar.df$roadDen))
# plot(to_raster(stackMacroVar.df$city))
# plot(to_raster(stackMacroVar.df$awc))
# plot(to_raster(stackMacroVar.df$cly))
# plot(to_raster(stackMacroVar.df$ptchDen))
# plot(to_raster(stackMacroVar.df$NeighUrb))


## 5. SAMPLING POINTS SELECTION ===================================================== 

# test year 0 and 0 land demand (to imitate initial 1999 land classes)
print("-- Sampling point selection ")

luLabel <- c(10,21,22,23,30,40,51,52,53,60,71,72,80)

for (i in 1:length(luLabel)){
  print(i)
  mlr_Dummy.df <- stackMacroVar.df %>% 
    filter(lu1999 == luLabel[i] &
             !is.na(slope) &
             !is.na(elev) &
             !is.na(road) &
             !is.na(city) &
             !is.na(roadDen) &
             !is.na(awc) &
             !is.na(cly) &
             !is.na(ptchDen) &
             !is.na(NeighUrb) &
             
             !is.na(sa4) &
             !is.na(UF) &
             !is.na(plan2010) &
             !is.na(protectArea) &
             !is.na(recreArea) 
    )
  
  fileNametest <- paste('./input/mlr_201811/mlr_data',luLabel[i],'.rda', sep = "")
  # fileNametest <- paste('./input/mlr_201811/mlr_data',i,'.rda', sep = "")
  
  save (mlr_Dummy.df, file = fileNametest)
  
}
