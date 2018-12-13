## Input: land cover 1999 and 2016; macro factors, MLR coefficients
## Analysis: 
## Author: Agung Wahyudi
## Date first created: 06/08/2018
## 
## About the ca model:
## The initial structure of the ca model 
## was inspired by simlander/apolus.
## Different to simlander, current ca model
## takes into account multiple-change pathways
## Modification: "removal of sample points of less than 2" on cross-tab 




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


##__2.1 -- Set working path $ DO CHANGE THIS PATH ####
setwd ("C:/Users/uqawahy1/Documents/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset") ## server gpem-lsec2


##__2.2 -- Load working maps ####
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


MacroVar$sa4fact  <- factor(MacroVar$sa4)
MacroVar$UFfact   <- factor(MacroVar$UF)
MacroVar$ptfact   <- factor(MacroVar$protectArea)
MacroVar$rcfact   <- factor(MacroVar$recreArea)
MacroVar$plan2010fact <- factor(MacroVar$plan2010)


lu1999.df     <- as.data.frame(lu1999)
lu2016.df     <- as.data.frame(lu2016)
freq1999      <- as.data.frame(freq(lu1999))
freq2016      <- as.data.frame(freq(lu2016))
tp.zero       <- lu1999 - lu1999 # zero map


names(lu1999.df) <- "lu1999"
names(lu2016.df) <- "lu2016"

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

##__2.3 -- Load coefficients table ####
coefficients = c()
setwd("C:/Users/uqawahy1/Documents/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd("M:/Projects/koala_offsets/02 Map/Quant_analysis")


for (i in 1:13){
  # fileName = paste("./input/mlrsummary_20181102/coefficient", i, ".rda", sep="")
  # fileName = paste("./input/mlrsummary_patchDens/coefficient", i, ".rda", sep="")
  # fileName = paste("./input/mlrsummary_NeighUrb/coefficient", i, ".rda", sep="")
  # fileName = paste("./input/mlrsummary_patchDens_NeighUrb/coefficient", i, ".rda", sep="")
  # fileName = paste("./input/mlrsummary_plan2010/coefficient", i, ".rda", sep="")
  
  fileName = paste("./input/mlrsummary_NeighUrb_UF/coefficient", i, ".rda", sep="")
  
  load(fileName)
  coefficients [[i]] <- test
}

# lapply(coefficients, function(x) x$lev)
remove(test)


setwd ("C:/Users/uqawahy1/Documents/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset")


## __2.4 -- Prepare simulation parameters ####
print ('Prepare simulation parameters')

red           <- c(0,198,51,0,0,255,255,255,255,255,217,166,91)
green         <- c(92,224,204,153,128,217,199,139,83,235,217,166,155)
blue          <- c(0,182,51,0,0,222,206,153,103,156,217,166,213)
colors        <- rgb(red, green, blue, maxColorValue = 255)
breakpoints   <- c(0,10,21,22,23,30,40,51,52,53,60,71,72,80)
luLabel       <- c(10,21,22,23,30,40,51,52,53,60,71,72,80)





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


to_crosstab <- function (obs, pred) {
  ctable <- (crosstabm( to_raster(obs), to_raster(pred)))
  View(ctable)
  return (ctable)
  
}


to_neighUrb <- function (inputRaster, movWin_nrow, movWin_ncol) {
  
  inputLU     <- inputRaster 
  inputDummy  <- inputLU-inputLU
  inputDummy[(  inputLU==51 | 
                  inputLU==52 | 
                  inputLU==53 | 
                  inputLU==60 | 
                  inputLU==71 | 
                  inputLU==72 & 
                  !is.na(inputLU))] <- 1
  movWin      <-  matrix(100/25,nrow=movWin_nrow,ncol=movWin_ncol)
  neighUrb    <- focal(inputDummy, w=movWin,  na.rm=TRUE)
  return (neighUrb)
  
}



## 4. DATA PREPARATION ===================================================== 

## __4.1 -- Conversion of factors to [0 1] , stacked, and df converted ########

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
stackMacroVar.df$sa4fact  [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$UFfact   [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$plan2010 [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$plan2010fact[(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA




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



## __4.2 -- Predict transition probabilities maps using stats::predict ########
for (i in 1:13 ){
  # coefficients[[i]]$xlevels$sa4fact <- levels(stackMacroVar.df$sa4fact)
  # coefficients[[i]]$xlevels$UFfact  <- levels(stackMacroVar.df$UFfact)
  coefficients[[i]]$xlevels$plan2010fact  <- levels(stackMacroVar.df$plan2010fact)
  
}


## could take for a while. a 13 by 13 prediction statistics would be generated
tp.stats   <- lapply (coefficients, to_predict, newdata.df = stackMacroVar.df)

# test <- to_predict(coefficients[[13]], stackMacroVar.df[c(700707),])
# lapply(coefficients, function(x) x$lev)


## __4.3 -- Re-write the transition probabilities maps to corresponding LU changes ########

# coefficients[1] "51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
tp.stats.10.10 <- to_raster(tp.stats[[1]][,2])
tp.stats.10.21 <- to_raster(tp.stats[[1]][,3])
tp.stats.10.22 <- to_raster(tp.stats[[1]][,4])
tp.stats.10.23 <- to_raster(tp.stats[[1]][,5])
tp.stats.10.30 <- to_raster(tp.stats[[1]][,6])
tp.stats.10.40 <- to_raster(tp.stats[[1]][,7])
tp.stats.10.51 <- to_raster(tp.stats[[1]][,1])
tp.stats.10.52 <- to_raster(tp.stats[[1]][,8])
tp.stats.10.53 <- to_raster(tp.stats[[1]][,9])
tp.stats.10.60 <- to_raster(tp.stats[[1]][,10])
tp.stats.10.71 <- to_raster(tp.stats[[1]][,11])
tp.stats.10.72 <- to_raster(tp.stats[[1]][,12])
tp.stats.10.80 <- to_raster(tp.stats[[1]][,13])


# coefficients[2] "51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
tp.stats.21.10 <- to_raster(tp.stats[[2]][,2])
tp.stats.21.21 <- to_raster(tp.stats[[2]][,3])
tp.stats.21.22 <- to_raster(tp.stats[[2]][,4])
tp.stats.21.23 <- to_raster(tp.stats[[2]][,5])
tp.stats.21.30 <- to_raster(tp.stats[[2]][,6])
tp.stats.21.40 <- to_raster(tp.stats[[2]][,7])
tp.stats.21.51 <- to_raster(tp.stats[[2]][,1])
tp.stats.21.52 <- to_raster(tp.stats[[2]][,8])
tp.stats.21.53 <- to_raster(tp.stats[[2]][,9])
tp.stats.21.60 <- to_raster(tp.stats[[2]][,10])
tp.stats.21.71 <- to_raster(tp.stats[[2]][,11])
tp.stats.21.72 <- to_raster(tp.stats[[2]][,12])
tp.stats.21.80 <- to_raster(tp.stats[[2]][,13])


# coefficients[3] "51" "10" "21" "22" "23" "30" "40" "52"  "60" "71" "72" "80"
tp.stats.22.10 <- to_raster(tp.stats[[3]][,2])
tp.stats.22.21 <- to_raster(tp.stats[[3]][,3])
tp.stats.22.22 <- to_raster(tp.stats[[3]][,4])
tp.stats.22.23 <- to_raster(tp.stats[[3]][,5])
tp.stats.22.30 <- to_raster(tp.stats[[3]][,6])
tp.stats.22.40 <- to_raster(tp.stats[[3]][,7])
tp.stats.22.51 <- to_raster(tp.stats[[3]][,1])
tp.stats.22.52 <- to_raster(tp.stats[[3]][,8])
tp.stats.22.53 <- tp.zero
tp.stats.22.60 <- to_raster(tp.stats[[3]][,9])
tp.stats.22.71 <- to_raster(tp.stats[[3]][,10])
tp.stats.22.72 <- to_raster(tp.stats[[3]][,11])
tp.stats.22.80 <- to_raster(tp.stats[[3]][,12])


# coefficients[4]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
tp.stats.23.10 <- to_raster(tp.stats[[4]][,2])
tp.stats.23.21 <- to_raster(tp.stats[[4]][,3])
tp.stats.23.22 <- to_raster(tp.stats[[4]][,4])
tp.stats.23.23 <- to_raster(tp.stats[[4]][,5])
tp.stats.23.30 <- to_raster(tp.stats[[4]][,6])
tp.stats.23.40 <- to_raster(tp.stats[[4]][,7])
tp.stats.23.51 <- to_raster(tp.stats[[4]][,1])
tp.stats.23.52 <- to_raster(tp.stats[[4]][,8])
tp.stats.23.53 <- to_raster(tp.stats[[4]][,9])
tp.stats.23.60 <- to_raster(tp.stats[[4]][,10])
tp.stats.23.71 <- to_raster(tp.stats[[4]][,11])
tp.stats.23.72 <- to_raster(tp.stats[[4]][,12])
tp.stats.23.80 <- to_raster(tp.stats[[4]][,13])


# coefficients[5]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72"
tp.stats.30.10 <- to_raster(tp.stats[[5]][,2])
tp.stats.30.21 <- to_raster(tp.stats[[5]][,3])
tp.stats.30.22 <- to_raster(tp.stats[[5]][,4])
tp.stats.30.23 <- to_raster(tp.stats[[5]][,5])
tp.stats.30.30 <- to_raster(tp.stats[[5]][,6])
tp.stats.30.40 <- to_raster(tp.stats[[5]][,7])
tp.stats.30.51 <- to_raster(tp.stats[[5]][,1])
tp.stats.30.52 <- to_raster(tp.stats[[5]][,8])
tp.stats.30.53 <- to_raster(tp.stats[[5]][,9])
tp.stats.30.60 <- to_raster(tp.stats[[5]][,10])
tp.stats.30.71 <- to_raster(tp.stats[[5]][,11])
tp.stats.30.72 <- to_raster(tp.stats[[5]][,12])
tp.stats.30.80 <- tp.zero


# coefficients[6]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
tp.stats.40.10 <- to_raster(tp.stats[[6]][,2])
tp.stats.40.21 <- to_raster(tp.stats[[6]][,3])
tp.stats.40.22 <- to_raster(tp.stats[[6]][,4])
tp.stats.40.23 <- to_raster(tp.stats[[6]][,5])
tp.stats.40.30 <- to_raster(tp.stats[[6]][,6])
tp.stats.40.40 <- to_raster(tp.stats[[6]][,7])
tp.stats.40.51 <- to_raster(tp.stats[[6]][,1])
tp.stats.40.52 <- to_raster(tp.stats[[6]][,8])
tp.stats.40.53 <- to_raster(tp.stats[[6]][,9])
tp.stats.40.60 <- to_raster(tp.stats[[6]][,10])
tp.stats.40.71 <- to_raster(tp.stats[[6]][,11])
tp.stats.40.72 <- to_raster(tp.stats[[6]][,12])
tp.stats.40.80 <- to_raster(tp.stats[[6]][,13])


# coefficients[7]"51" "10" "23" "40" "52" "53" "60" "71" "72"
tp.stats.51.10 <- to_raster(tp.stats[[7]][,2])
tp.stats.51.21 <- tp.zero
tp.stats.51.22 <- tp.zero
tp.stats.51.23 <- to_raster(tp.stats[[7]][,3])
tp.stats.51.30 <- tp.zero
tp.stats.51.40 <- to_raster(tp.stats[[7]][,4])
tp.stats.51.51 <- to_raster(tp.stats[[7]][,1])
tp.stats.51.52 <- to_raster(tp.stats[[7]][,5])
tp.stats.51.53 <- to_raster(tp.stats[[7]][,6])
tp.stats.51.60 <- to_raster(tp.stats[[7]][,7])
tp.stats.51.71 <- to_raster(tp.stats[[7]][,8])
tp.stats.51.72 <- to_raster(tp.stats[[7]][,9])
tp.stats.51.80 <- tp.zero


# coefficients[8] "51" "23" "52" "53" "60"
tp.stats.52.10 <- tp.zero
tp.stats.52.21 <- tp.zero
tp.stats.52.22 <- tp.zero
tp.stats.52.23 <- to_raster(tp.stats[[8]][,2])
tp.stats.52.30 <- tp.zero
tp.stats.52.40 <- tp.zero
tp.stats.52.51 <- to_raster(tp.stats[[8]][,1])
tp.stats.52.52 <- to_raster(tp.stats[[8]][,3])
tp.stats.52.53 <- to_raster(tp.stats[[8]][,4])
tp.stats.52.60 <- to_raster(tp.stats[[8]][,5])
tp.stats.52.71 <- tp.zero
tp.stats.52.72 <- tp.zero
tp.stats.52.80 <- tp.zero


# coefficients[9] "51" "52" "53" "60"
tp.stats.53.10 <- tp.zero
tp.stats.53.21 <- tp.zero
tp.stats.53.22 <- tp.zero
tp.stats.53.23 <- tp.zero
tp.stats.53.30 <- tp.zero
tp.stats.53.40 <- tp.zero
tp.stats.53.51 <- to_raster(tp.stats[[9]][,1])
tp.stats.53.52 <- to_raster(tp.stats[[9]][,2])
tp.stats.53.53 <- to_raster(tp.stats[[9]][,3])
tp.stats.53.60 <- to_raster(tp.stats[[9]][,4])
tp.stats.53.71 <- tp.zero
tp.stats.53.72 <- tp.zero
tp.stats.53.80 <- tp.zero


# coefficients[10] "51" "10" "21" "22" "23" "40" "52" "53" "60" "71" "72" "80"
tp.stats.60.10 <- to_raster(tp.stats[[10]][,2])
tp.stats.60.21 <- to_raster(tp.stats[[10]][,3])
tp.stats.60.22 <- to_raster(tp.stats[[10]][,4])
tp.stats.60.23 <- to_raster(tp.stats[[10]][,5])
tp.stats.60.30 <- tp.zero
tp.stats.60.40 <- to_raster(tp.stats[[10]][,6])
tp.stats.60.51 <- to_raster(tp.stats[[10]][,1])
tp.stats.60.52 <- to_raster(tp.stats[[10]][,7])
tp.stats.60.53 <- to_raster(tp.stats[[10]][,8])
tp.stats.60.60 <- to_raster(tp.stats[[10]][,9])
tp.stats.60.71 <- to_raster(tp.stats[[10]][,10])
tp.stats.60.72 <- to_raster(tp.stats[[10]][,11])
tp.stats.60.80 <- to_raster(tp.stats[[10]][,12])


# coefficients[11] "51" "10" "21" "22" "23" "40" "52" "60" "71" "72" "80"
tp.stats.71.10 <- to_raster(tp.stats[[11]][,2])
tp.stats.71.21 <- to_raster(tp.stats[[11]][,3])
tp.stats.71.22 <- to_raster(tp.stats[[11]][,4])
tp.stats.71.23 <- to_raster(tp.stats[[11]][,5])
tp.stats.71.30 <- tp.zero
tp.stats.71.40 <- to_raster(tp.stats[[11]][,6])
tp.stats.71.51 <- to_raster(tp.stats[[11]][,1])
tp.stats.71.52 <- to_raster(tp.stats[[11]][,7])
tp.stats.71.53 <- tp.zero
tp.stats.71.60 <- to_raster(tp.stats[[11]][,8])
tp.stats.71.71 <- to_raster(tp.stats[[11]][,9])
tp.stats.71.72 <- to_raster(tp.stats[[11]][,10])
tp.stats.71.80 <- to_raster(tp.stats[[11]][,11])


# coefficients[12] "51" "10" "21" "23" "52" "53" "60" "72"
tp.stats.72.10 <- to_raster(tp.stats[[12]][,2])
tp.stats.72.21 <- to_raster(tp.stats[[12]][,3])
tp.stats.72.22 <- tp.zero
tp.stats.72.23 <- to_raster(tp.stats[[12]][,4])
tp.stats.72.30 <- tp.zero
tp.stats.72.40 <- tp.zero
tp.stats.72.51 <- to_raster(tp.stats[[12]][,1])
tp.stats.72.52 <- to_raster(tp.stats[[12]][,5])
tp.stats.72.53 <- to_raster(tp.stats[[12]][,6])
tp.stats.72.60 <- to_raster(tp.stats[[12]][,7])
tp.stats.72.71 <- tp.zero
tp.stats.72.72 <- to_raster(tp.stats[[12]][,8])
tp.stats.72.80 <- tp.zero


# coefficients[13] "51" "10" "21" "22" "23" "30" "52" "60" "72" "80"
tp.stats.80.10 <- to_raster(tp.stats[[13]][,2])
tp.stats.80.21 <- to_raster(tp.stats[[13]][,3])
tp.stats.80.22 <- to_raster(tp.stats[[13]][,4])
tp.stats.80.23 <- to_raster(tp.stats[[13]][,5])
tp.stats.80.30 <- to_raster(tp.stats[[13]][,6])
tp.stats.80.40 <- tp.zero
tp.stats.80.51 <- to_raster(tp.stats[[13]][,1])
tp.stats.80.52 <- to_raster(tp.stats[[13]][,7])
tp.stats.80.53 <- tp.zero
tp.stats.80.60 <- to_raster(tp.stats[[13]][,8])
tp.stats.80.71 <- tp.zero
tp.stats.80.72 <- to_raster(tp.stats[[13]][,9])
tp.stats.80.80 <- to_raster(tp.stats[[13]][,10])

remove(tp.stats)


#__4.4 -- Data frame of transition probability. Update tp.cover.ls for simulation due to change in neighborhood ratiorban  #### 

tp.cover.ls <- list()
for (i in 1:length(luLabel)){
  print (paste("Masking and stacking tp.stats.", luLabel[i], sep=""))
  
  test0 <- paste("tp.stats.", luLabel[i], sep="")
  test1 <- mget(ls(pattern = test0))
  test2 <- stack(test1)
  tp.cover.ls[[i]] <- test2
  
  remove(list = (ls(pattern = test0)))
  remove(test0, test1, test2)
}





## 5. MULTIPLE LAND USE CHANGES SIMULATION ===============

## __5.1 -- Prepare simulation parameters ####
print ('Prepare simulation parameters')

# remove unnecessary variables to free up some memory spaces
if (exists ("luSimul.df"))  remove (luSimul.df)
if (exists ("tp.cover.df")) remove (tp.cover.df)
if (exists ("tp.cover"))    remove (tp.cover)





## __5.2 -- Model Calibration: (LU1999->LU2016) VS (LU1999->SimulLU2016) ####
print("-- update the transition probability according to recent LULC")


luDummy <- to_raster(lu1999) # Initial land-use map



#____5.2.1 -- Update the tp according to initial LU-- #### 
test <- list()
for (i in 1:length(luLabel)){
  print (paste("Filter and select Land-use", i))
  
  tpDummy.df          <- as.data.frame(tp.cover.ls[[i]])
  luDummy.df          <- as.data.frame(luDummy)
  names(luDummy.df)   <- "layer"
  tpDummy.df$luDummy  <- luDummy.df$layer
  names(tpDummy.df)   <- c("tp10",
                           "tp21",
                           "tp22",
                           "tp23",
                           "tp30",
                           "tp40",
                           "tp51",
                           "tp52",
                           "tp53",
                           "tp60",
                           "tp71",
                           "tp72",
                           "tp80",
                           "luDummy")
  
  test[[i]] <- tpDummy.df %>%
    mutate (orig_rank = row_number()) %>%
    filter (luDummy == luLabel[i])
  print (i)
  
}

test2 <- bind_rows(test)

luDummy.df <- luDummy.df%>%
  mutate (orig_rank = row_number())

tp.cover          <- dplyr::right_join(test2,luDummy.df,  by = "orig_rank")
tp.cover$layer    <- c()
tp.cover$lu1999   <- lu1999.df$lu1999
tp.cover$lu2016   <- lu2016.df$lu2016
remove(test, test2, tpDummy.df, luDummy.df)




#____5.2.2 -- Cleaning tp.cover.df from NA Transition Probability #### 
tp.cover.nona.df <- tp.cover %>%
  dplyr::filter(!is.na(tp10)) %>%
  dplyr::filter(!is.na(tp21)) %>%
  dplyr::filter(!is.na(tp22)) %>%
  dplyr::filter(!is.na(tp23)) %>%
  dplyr::filter(!is.na(tp30)) %>%
  dplyr::filter(!is.na(tp40)) %>%
  dplyr::filter(!is.na(tp51)) %>%
  dplyr::filter(!is.na(tp52)) %>%
  dplyr::filter(!is.na(tp53)) %>%
  dplyr::filter(!is.na(tp60)) %>%
  dplyr::filter(!is.na(tp71)) %>%
  dplyr::filter(!is.na(tp72)) %>%
  dplyr::filter(!is.na(tp80)) 


#____5.2.3 -- Start simulation here#### 
nSimulation   <- 10 # number of simulation run for each period (17 years)
luSimulStack  <- c()
tp.simul      <- tp.cover.nona.df # transition probability used for simulation

for (i in 1:nSimulation){
  print(paste("Simulation no ", i, " of ", nSimulation, sep = ""))
  tp.simul$luDynmc <- crossprod(apply(tp.simul[, c(1:13)], 1, function(x) rmultinom(1,size = 1,x)),luLabel)
  
  test0 <- tp.simul %>% 
    dplyr:: select(orig_rank,luDynmc ) 
  
  tp.cover$luDynmc <- c()
  tp.cover<- dplyr::left_join(tp.cover, test0, by = "orig_rank")
  plot(to_raster(tp.cover$luDynmc), breaks=breakpoints,col=colors)
  title(paste("Simulation map of 2016, i =", i))
  # luSimulStack[[i]] <- tp.cover.df$luDynmc
  luSimulStack <- cbind(luSimulStack, tp.cover$luDynmc)
}





## __5.3 -- Forward simulation -- ####
print("-- update the transition probability according to recent LULC")

tSimul  <- 1 #5-year simulation period start with 2016. tSimul = 10, means 2016+5*tSimul = 2066
t       <- 0  # Initial land-use map is 2016

for (t in 0:tSimul) {
  
  ##____5.3.1 -- Update the tp according to initi LU-- ####

  print(paste("Simulating landuse of ",(5*t+2016)))
  
  ifelse ( t == 0,  
           luDummy <- lu2016.df$lu2016,   # at t=0
           luDummy <- tp.cover$luDynmc)   # at t!=0 
  
  if (exists ("luSimul.df"))    remove (luSimul.df)
  if (exists ("tp.cover.df"))   remove (tp.cover.df)
  if (exists ("tp.cover"))      remove (tp.cover)
  if (exists ("luSimulStack"))  remove (luSimulStack)
  
  ##______5.3.1.1 -- Update the dynamic neighborhood urban ratio ####
  
  luDummy.rs        <- to_raster(luDummy)
  luDummy.nu        <- to_neighUrb(luDummy.rs, 5, 5)
  luDummy.nu.df     <- as.data.frame(luDummy.nu)
  names(luDummy.nu.df) <- "NeighUrb"
  MacroVar$NeighUrb <- luDummy.nu.df$NeighUrb 
  stackMacroVar.df$NeighUrb <- to_zero_one(MacroVar$NeighUrb)
  stackMacroVar.df$NeighUrb [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
  
  ## ______5.3.1.2 -- Predict transition probabilities maps using stats::predict ####
  
  for (i in 1:13 ){
    # coefficients[[i]]$xlevels$sa4fact <- levels(stackMacroVar.df$sa4fact)
    # coefficients[[i]]$xlevels$UFfact  <- levels(stackMacroVar.df$UFfact)
    coefficients[[i]]$xlevels$plan2010fact  <- levels(stackMacroVar.df$plan2010fact)
  }
  
  tp.stats   <- lapply (coefficients, to_predict, newdata.df = stackMacroVar.df)
  
  ## ______5.3.1.3 -- Re-write the transition probabilities maps to corresponding LU changes ########
  
  # coefficients[1] "51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
  tp.stats.10.10 <- to_raster(tp.stats[[1]][,2])
  tp.stats.10.21 <- to_raster(tp.stats[[1]][,3])
  tp.stats.10.22 <- to_raster(tp.stats[[1]][,4])
  tp.stats.10.23 <- to_raster(tp.stats[[1]][,5])
  tp.stats.10.30 <- to_raster(tp.stats[[1]][,6])
  tp.stats.10.40 <- to_raster(tp.stats[[1]][,7])
  tp.stats.10.51 <- to_raster(tp.stats[[1]][,1])
  tp.stats.10.52 <- to_raster(tp.stats[[1]][,8])
  tp.stats.10.53 <- to_raster(tp.stats[[1]][,9])
  tp.stats.10.60 <- to_raster(tp.stats[[1]][,10])
  tp.stats.10.71 <- to_raster(tp.stats[[1]][,11])
  tp.stats.10.72 <- to_raster(tp.stats[[1]][,12])
  tp.stats.10.80 <- to_raster(tp.stats[[1]][,13])
  
  
  # coefficients[2] "51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
  tp.stats.21.10 <- to_raster(tp.stats[[2]][,2])
  tp.stats.21.21 <- to_raster(tp.stats[[2]][,3])
  tp.stats.21.22 <- to_raster(tp.stats[[2]][,4])
  tp.stats.21.23 <- to_raster(tp.stats[[2]][,5])
  tp.stats.21.30 <- to_raster(tp.stats[[2]][,6])
  tp.stats.21.40 <- to_raster(tp.stats[[2]][,7])
  tp.stats.21.51 <- to_raster(tp.stats[[2]][,1])
  tp.stats.21.52 <- to_raster(tp.stats[[2]][,8])
  tp.stats.21.53 <- to_raster(tp.stats[[2]][,9])
  tp.stats.21.60 <- to_raster(tp.stats[[2]][,10])
  tp.stats.21.71 <- to_raster(tp.stats[[2]][,11])
  tp.stats.21.72 <- to_raster(tp.stats[[2]][,12])
  tp.stats.21.80 <- to_raster(tp.stats[[2]][,13])
  
  
  # coefficients[3] "51" "10" "21" "22" "23" "30" "40" "52"  "60" "71" "72" "80"
  tp.stats.22.10 <- to_raster(tp.stats[[3]][,2])
  tp.stats.22.21 <- to_raster(tp.stats[[3]][,3])
  tp.stats.22.22 <- to_raster(tp.stats[[3]][,4])
  tp.stats.22.23 <- to_raster(tp.stats[[3]][,5])
  tp.stats.22.30 <- to_raster(tp.stats[[3]][,6])
  tp.stats.22.40 <- to_raster(tp.stats[[3]][,7])
  tp.stats.22.51 <- to_raster(tp.stats[[3]][,1])
  tp.stats.22.52 <- to_raster(tp.stats[[3]][,8])
  tp.stats.22.53 <- tp.zero
  tp.stats.22.60 <- to_raster(tp.stats[[3]][,9])
  tp.stats.22.71 <- to_raster(tp.stats[[3]][,10])
  tp.stats.22.72 <- to_raster(tp.stats[[3]][,11])
  tp.stats.22.80 <- to_raster(tp.stats[[3]][,12])
  
  
  # coefficients[4]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
  tp.stats.23.10 <- to_raster(tp.stats[[4]][,2])
  tp.stats.23.21 <- to_raster(tp.stats[[4]][,3])
  tp.stats.23.22 <- to_raster(tp.stats[[4]][,4])
  tp.stats.23.23 <- to_raster(tp.stats[[4]][,5])
  tp.stats.23.30 <- to_raster(tp.stats[[4]][,6])
  tp.stats.23.40 <- to_raster(tp.stats[[4]][,7])
  tp.stats.23.51 <- to_raster(tp.stats[[4]][,1])
  tp.stats.23.52 <- to_raster(tp.stats[[4]][,8])
  tp.stats.23.53 <- to_raster(tp.stats[[4]][,9])
  tp.stats.23.60 <- to_raster(tp.stats[[4]][,10])
  tp.stats.23.71 <- to_raster(tp.stats[[4]][,11])
  tp.stats.23.72 <- to_raster(tp.stats[[4]][,12])
  tp.stats.23.80 <- to_raster(tp.stats[[4]][,13])
  
  
  # coefficients[5]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72"
  tp.stats.30.10 <- to_raster(tp.stats[[5]][,2])
  tp.stats.30.21 <- to_raster(tp.stats[[5]][,3])
  tp.stats.30.22 <- to_raster(tp.stats[[5]][,4])
  tp.stats.30.23 <- to_raster(tp.stats[[5]][,5])
  tp.stats.30.30 <- to_raster(tp.stats[[5]][,6])
  tp.stats.30.40 <- to_raster(tp.stats[[5]][,7])
  tp.stats.30.51 <- to_raster(tp.stats[[5]][,1])
  tp.stats.30.52 <- to_raster(tp.stats[[5]][,8])
  tp.stats.30.53 <- to_raster(tp.stats[[5]][,9])
  tp.stats.30.60 <- to_raster(tp.stats[[5]][,10])
  tp.stats.30.71 <- to_raster(tp.stats[[5]][,11])
  tp.stats.30.72 <- to_raster(tp.stats[[5]][,12])
  tp.stats.30.80 <- tp.zero
  
  
  # coefficients[6]"51" "10" "21" "22" "23" "30" "40" "52" "53" "60" "71" "72" "80"
  tp.stats.40.10 <- to_raster(tp.stats[[6]][,2])
  tp.stats.40.21 <- to_raster(tp.stats[[6]][,3])
  tp.stats.40.22 <- to_raster(tp.stats[[6]][,4])
  tp.stats.40.23 <- to_raster(tp.stats[[6]][,5])
  tp.stats.40.30 <- to_raster(tp.stats[[6]][,6])
  tp.stats.40.40 <- to_raster(tp.stats[[6]][,7])
  tp.stats.40.51 <- to_raster(tp.stats[[6]][,1])
  tp.stats.40.52 <- to_raster(tp.stats[[6]][,8])
  tp.stats.40.53 <- to_raster(tp.stats[[6]][,9])
  tp.stats.40.60 <- to_raster(tp.stats[[6]][,10])
  tp.stats.40.71 <- to_raster(tp.stats[[6]][,11])
  tp.stats.40.72 <- to_raster(tp.stats[[6]][,12])
  tp.stats.40.80 <- to_raster(tp.stats[[6]][,13])
  
  
  # coefficients[7]"51" "10" "23" "40" "52" "53" "60" "71" "72"
  tp.stats.51.10 <- to_raster(tp.stats[[7]][,2])
  tp.stats.51.21 <- tp.zero
  tp.stats.51.22 <- tp.zero
  tp.stats.51.23 <- to_raster(tp.stats[[7]][,3])
  tp.stats.51.30 <- tp.zero
  tp.stats.51.40 <- to_raster(tp.stats[[7]][,4])
  tp.stats.51.51 <- to_raster(tp.stats[[7]][,1])
  tp.stats.51.52 <- to_raster(tp.stats[[7]][,5])
  tp.stats.51.53 <- to_raster(tp.stats[[7]][,6])
  tp.stats.51.60 <- to_raster(tp.stats[[7]][,7])
  tp.stats.51.71 <- to_raster(tp.stats[[7]][,8])
  tp.stats.51.72 <- to_raster(tp.stats[[7]][,9])
  tp.stats.51.80 <- tp.zero
  
  
  # coefficients[8] "51" "23" "52" "53" "60"
  tp.stats.52.10 <- tp.zero
  tp.stats.52.21 <- tp.zero
  tp.stats.52.22 <- tp.zero
  tp.stats.52.23 <- to_raster(tp.stats[[8]][,2])
  tp.stats.52.30 <- tp.zero
  tp.stats.52.40 <- tp.zero
  tp.stats.52.51 <- to_raster(tp.stats[[8]][,1])
  tp.stats.52.52 <- to_raster(tp.stats[[8]][,3])
  tp.stats.52.53 <- to_raster(tp.stats[[8]][,4])
  tp.stats.52.60 <- to_raster(tp.stats[[8]][,5])
  tp.stats.52.71 <- tp.zero
  tp.stats.52.72 <- tp.zero
  tp.stats.52.80 <- tp.zero
  
  
  # coefficients[9] "51" "52" "53" "60"
  tp.stats.53.10 <- tp.zero
  tp.stats.53.21 <- tp.zero
  tp.stats.53.22 <- tp.zero
  tp.stats.53.23 <- tp.zero
  tp.stats.53.30 <- tp.zero
  tp.stats.53.40 <- tp.zero
  tp.stats.53.51 <- to_raster(tp.stats[[9]][,1])
  tp.stats.53.52 <- to_raster(tp.stats[[9]][,2])
  tp.stats.53.53 <- to_raster(tp.stats[[9]][,3])
  tp.stats.53.60 <- to_raster(tp.stats[[9]][,4])
  tp.stats.53.71 <- tp.zero
  tp.stats.53.72 <- tp.zero
  tp.stats.53.80 <- tp.zero
  
  
  # coefficients[10] "51" "10" "21" "22" "23" "40" "52" "53" "60" "71" "72" "80"
  tp.stats.60.10 <- to_raster(tp.stats[[10]][,2])
  tp.stats.60.21 <- to_raster(tp.stats[[10]][,3])
  tp.stats.60.22 <- to_raster(tp.stats[[10]][,4])
  tp.stats.60.23 <- to_raster(tp.stats[[10]][,5])
  tp.stats.60.30 <- tp.zero
  tp.stats.60.40 <- to_raster(tp.stats[[10]][,6])
  tp.stats.60.51 <- to_raster(tp.stats[[10]][,1])
  tp.stats.60.52 <- to_raster(tp.stats[[10]][,7])
  tp.stats.60.53 <- to_raster(tp.stats[[10]][,8])
  tp.stats.60.60 <- to_raster(tp.stats[[10]][,9])
  tp.stats.60.71 <- to_raster(tp.stats[[10]][,10])
  tp.stats.60.72 <- to_raster(tp.stats[[10]][,11])
  tp.stats.60.80 <- to_raster(tp.stats[[10]][,12])
  
  
  # coefficients[11] "51" "10" "21" "22" "23" "40" "52" "60" "71" "72" "80"
  tp.stats.71.10 <- to_raster(tp.stats[[11]][,2])
  tp.stats.71.21 <- to_raster(tp.stats[[11]][,3])
  tp.stats.71.22 <- to_raster(tp.stats[[11]][,4])
  tp.stats.71.23 <- to_raster(tp.stats[[11]][,5])
  tp.stats.71.30 <- tp.zero
  tp.stats.71.40 <- to_raster(tp.stats[[11]][,6])
  tp.stats.71.51 <- to_raster(tp.stats[[11]][,1])
  tp.stats.71.52 <- to_raster(tp.stats[[11]][,7])
  tp.stats.71.53 <- tp.zero
  tp.stats.71.60 <- to_raster(tp.stats[[11]][,8])
  tp.stats.71.71 <- to_raster(tp.stats[[11]][,9])
  tp.stats.71.72 <- to_raster(tp.stats[[11]][,10])
  tp.stats.71.80 <- to_raster(tp.stats[[11]][,11])
  
  
  # coefficients[12] "51" "10" "21" "23" "52" "53" "60" "72"
  tp.stats.72.10 <- to_raster(tp.stats[[12]][,2])
  tp.stats.72.21 <- to_raster(tp.stats[[12]][,3])
  tp.stats.72.22 <- tp.zero
  tp.stats.72.23 <- to_raster(tp.stats[[12]][,4])
  tp.stats.72.30 <- tp.zero
  tp.stats.72.40 <- tp.zero
  tp.stats.72.51 <- to_raster(tp.stats[[12]][,1])
  tp.stats.72.52 <- to_raster(tp.stats[[12]][,5])
  tp.stats.72.53 <- to_raster(tp.stats[[12]][,6])
  tp.stats.72.60 <- to_raster(tp.stats[[12]][,7])
  tp.stats.72.71 <- tp.zero
  tp.stats.72.72 <- to_raster(tp.stats[[12]][,8])
  tp.stats.72.80 <- tp.zero
  
  
  # coefficients[13] "51" "10" "21" "22" "23" "30" "52" "60" "72" "80"
  tp.stats.80.10 <- to_raster(tp.stats[[13]][,2])
  tp.stats.80.21 <- to_raster(tp.stats[[13]][,3])
  tp.stats.80.22 <- to_raster(tp.stats[[13]][,4])
  tp.stats.80.23 <- to_raster(tp.stats[[13]][,5])
  tp.stats.80.30 <- to_raster(tp.stats[[13]][,6])
  tp.stats.80.40 <- tp.zero
  tp.stats.80.51 <- to_raster(tp.stats[[13]][,1])
  tp.stats.80.52 <- to_raster(tp.stats[[13]][,7])
  tp.stats.80.53 <- tp.zero
  tp.stats.80.60 <- to_raster(tp.stats[[13]][,8])
  tp.stats.80.71 <- tp.zero
  tp.stats.80.72 <- to_raster(tp.stats[[13]][,9])
  tp.stats.80.80 <- to_raster(tp.stats[[13]][,10])
  
  remove(tp.stats)
  
  ## ______5.3.1.4 -- Data frame of transition probability.  #### 
  
  tp.cover.ls <- list()
  for (i in 1:length(luLabel)){
    print (paste("Masking and stacking tp.stats.", luLabel[i], sep=""))
    
    test0 <- paste("tp.stats.", luLabel[i], sep="")
    test1 <- mget(ls(pattern = test0))
    test2 <- stack(test1)
    tp.cover.ls[[i]] <- test2
    
    remove(list = (ls(pattern = test0)))
    remove(test0, test1, test2)
  }
  
  #____5.3.2 -- Masking the updated tp according to current LU #### 
  
  
  test <- list()
  for (i in 1:length(luLabel)){
    print (paste("Filter and select Land-use", i))
    
    tpDummy.df          <- as.data.frame(tp.cover.ls[[i]])
    luDummy.df          <- as.data.frame(luDummy)
    names(luDummy.df)   <- "layer"
    tpDummy.df$luDummy  <- luDummy.df$layer
    names(tpDummy.df)   <- c("tp10",
                             "tp21",
                             "tp22",
                             "tp23",
                             "tp30",
                             "tp40",
                             "tp51",
                             "tp52",
                             "tp53",
                             "tp60",
                             "tp71",
                             "tp72",
                             "tp80",
                             "luDummy")
    
    test[[i]] <- tpDummy.df %>%
      mutate (orig_rank = row_number()) %>%
      filter (luDummy == luLabel[i])
    print (i)
    
  }
  
  test2 <- bind_rows(test)
  
  luDummy.df <- luDummy.df%>%
    mutate (orig_rank = row_number())
  
  tp.cover          <- dplyr::right_join(test2,luDummy.df,  by = "orig_rank")
  tp.cover$layer    <- c()
  tp.cover$lu1999   <- lu1999.df$lu1999
  tp.cover$lu2016   <- lu2016.df$lu2016
  remove(test, test2, tpDummy.df, luDummy.df)
  
  
  
  #____5.3.3 -- Cleaning tp.cover.df from NA Transition Probability #### 
  tp.cover.nona.df <- tp.cover %>%
    dplyr::filter(!is.na(tp10)) %>%
    dplyr::filter(!is.na(tp21)) %>%
    dplyr::filter(!is.na(tp22)) %>%
    dplyr::filter(!is.na(tp23)) %>%
    dplyr::filter(!is.na(tp30)) %>%
    dplyr::filter(!is.na(tp40)) %>%
    dplyr::filter(!is.na(tp51)) %>%
    dplyr::filter(!is.na(tp52)) %>%
    dplyr::filter(!is.na(tp53)) %>%
    dplyr::filter(!is.na(tp60)) %>%
    dplyr::filter(!is.na(tp71)) %>%
    dplyr::filter(!is.na(tp72)) %>%
    dplyr::filter(!is.na(tp80)) 
  
  
  
  ##____5.3.4 -- Preparing simulation's parameters-- ####
  ## Create transition probability of one at t=0 based on lu1999.
  
  ## Annual tp. Based on multiyear change-rate (modified)
  tp.End    <- tp.cover.nona.df[,1:13] 
  nYear     <- 17
  nYearGap  <- 5
  eps       <- .Machine$double.eps^0.5 # default tolerance for small tp
  
  tp.Ratio      <- as.data.frame(1-((1-tp.End)^(nYearGap/nYear)))
  tp.Ratio[tp.Ratio < eps] <- 0
  tp.Ratio$Sum  <- rowSums(tp.Ratio)
  test          <- tp.Ratio
  
  ## sum tp of one. accroding to current land use
  for (i in 1:length(luLabel) ){
    idTP          <- which(tp.cover.nona.df$luDummy == luLabel[i]) 
    idTP          <- idTP[which(!is.na(idTP))]
    test[idTP,i]  <- 1- tp.Ratio$Sum [idTP] + tp.Ratio[idTP,i] 
  }
  
  test [test<0] <- 0
  test$Sum <- c()
  test$Sum <- rowSums(test)
  tp.Ratio <- test
  remove(tp.End, test, idTP)
  
  
  
  
  
  
  
  
  
  #____5.3.5 -- Start simulation here#### 
  nSimulation   <- 1     # number of simulation instances for each period (17 years)
  luSimulStack  <- c()
  tp.simul      <- cbind(tp.Ratio, tp.cover.nona.df[,14:17]) # transition probability used for simulation
  
  
  for (i in 1:nSimulation){
    print(paste("Simulation no ", i, " of ", nSimulation, sep = ""))
    tp.simul$luDynmc <- crossprod(apply(tp.simul[, c(1:13)], 1, function(x) rmultinom(1,size = 1,x)),luLabel)
    
    test0 <- tp.simul %>% 
      dplyr:: select(orig_rank,luDynmc ) 
    
    tp.cover$luDynmc <- c()
    tp.cover<- dplyr::left_join(tp.cover, test0, by = "orig_rank")
    plot(to_raster(tp.cover$luDynmc), breaks=breakpoints,col=colors)
    title(paste("Simulation map of", t*5+2016))
    # luSimulStack[[i]] <- tp.cover.df$luDynmc
    luSimulStack <- cbind(luSimulStack, tp.cover$luDynmc)
  }
  
  remove(test0)
}





















## 6. ACCURACY ASSESSMENT USING KAPPA simulation (CELL TO CELL)=================


## __6.1 -- Single accuracy assessment  ####

intialLU <- tp.cover$lu1999
actualLu <- tp.cover$lu2016
simultLu <- tp.cover$luDynmc

source("R_functions/kappasimulation.R")
ctable        <- to_crosstab(intialLU, simultLu)
kappa.output  <- Kappa (ctable)
print(kappa.output)




source("R_functions/kappa_rossiter.R")
kappa.Rositer <- kappa(ctable)
summary.kappa(kappa.Rositer, alpha=0.05)


source("R_functions/missedHit.R")
tp.cover$luDynmc  <- as.integer(tp.cover$luDynmc)
sapply(tp.cover.df, class)


tp.cover$luChange <- mutate(tp.cover, luChange = 0) %>%  
    mutate(luChange = ifelse((lu1999 == lu2016) & (lu1999 == luDynmc) ,  11,                  # 11 Correct non-changed lu
                           ifelse((lu1999 == lu2016) & (lu1999 != luDynmc), 12,               # 12 False alarm
                                  ifelse((lu1999 != lu2016) & (lu1999 == luDynmc), 21, 22)))) # 21 Missed-hit
                                                                                              # 22 correct changes


obs         <- to_raster(tp.cover$lu2016)
simulation  <- to_raster(tp.cover$luChange)
missedHit(obs, simulation)







## __6.2 -- Looping accuracy assessment on n simulation maps ####

kappa.join = c()

for (i in 1:nSimulation){
  print(i)
  
  simulDummy  <- luSimulStack[[i]]
  kappa.test  <- Kappa (obs = lu2016.df$landuse16reclsuburb4,pred = simulDummy)
  kappa.m     <- matrix(c(kappa.test$po, 
                          kappa.test$pe, 
                          kappa.test$pe_max, 
                          kappa.test$KAPPA, 
                          kappa.test$kappa_histogram, 
                          kappa.test$kappa_location))
  
  kappa.join  <- cbind(kappa.join, kappa.m)
  
  
  
}

mean.kappa.join <- rowMeans(kappa.join, na.rm = FALSE, dims = 1)
sd.kappa.join   <- apply(kappa.join,1,sd)
  


kappaR.user.naive.join  = c()
kappaR.prod.naive.join  = c()
kappaR.user.kappa.join  = c()
kappaR.user.kvar.join   = c()
kappaR.prod.kappa.join  = c()
kappaR.prod.kvar.join   = c()


for (i in 1:nSimulation){
  print(i)
  
  simulDummy      <- luSimulStack[[i]]
  crosstab16Simul <- (crosstabm(lu2016, to_raster(simulDummy)))
  kappa16simul    <- kappa(crosstab16Simul)

  kappaR.user.naive.join  = cbind(kappaR.user.naive.join, kappa16simul$user.naive)
  kappaR.prod.naive.join  = cbind(kappaR.prod.naive.join, kappa16simul$prod.naive)
  kappaR.user.kappa.join  = cbind(kappaR.user.kappa.join, kappa16simul$user.kappa)
  kappaR.user.kvar.join   = cbind(kappaR.user.kvar.join,  kappa16simul$user.kvar)
  kappaR.prod.kappa.join  = cbind(kappaR.prod.kappa.join, kappa16simul$prod.kappa)
  kappaR.prod.kvar.join   = cbind(kappaR.prod.kvar.join,  kappa16simul$prod.kvar)
  
}



ctable.join = c()
simulHit  = c()

for (i in 1:nSimulation){
  print(i)
  
  simulDummy      <- luSimulStack[[i]]
  
  simulHit <- cbind(lu1999.df, lu2016.df, simulDummy)
  names(simulHit) <- c("lu1999","lu2016", "luDynmc")
  
  simulHit <- simulHit %>% 
    mutate(luChange = 0) %>% 
    mutate(luChange = ifelse((lu1999 == lu2016) & (lu1999 == luDynmc) ,  11, 
                             ifelse((lu1999 == lu2016) & (lu1999 != luDynmc), 12,
                                    ifelse((lu1999 != lu2016) & (lu1999 == luDynmc), 21, 22))))
  
  ctable.dummy <- missedHit(obs=lu2016, simulation = to_raster(simulHit$luChange))
  
  test<- as.vector(t(ctable.dummy[(1:13),(1:4)]))
  ctable.join <- cbind(ctable.join,test)
}

ctable <- rowMeans(ctable.join)
dim(ctable) <- c(4,13)
View(t(ctable))





## 7. EXPORT MAP FOR VISUAL PRESENTATION in ArcGIS =============================


lengthLU        <- dim(luSimulStack[[1]])[1]
luSimulStack.df <- data.frame(matrix(unlist(luSimulStack), nrow=lengthLU, byrow=T))
luSimulStack.df$modLU   <- apply(luSimulStack.df,1, function(x) to_get_mode(x))





filen       <- paste("output/20181114/lu_simul_", 1999+i, ".asc", sep="")	
writeRaster(x = to_raster(luSimulStack[[1]]), filename = paste(filen), format = "ascii", overwrite=TRUE)


filen       <- paste("output/20181114/lu_simul_change.asc", sep="")	
writeRaster(x = to_raster(simulHit$luChange), filename = paste(filen), format = "ascii", overwrite=TRUE)
writeRaster(x = to_raster(luSimul.df$luChange), filename = paste(filen), format = "ascii", overwrite=TRUE)


