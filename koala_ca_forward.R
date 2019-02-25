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
if (!require("snowfall")) install.packages("snowfall"); library("snowfall")
if (!require("foreach"))  install.packages("foreach") ; library("foreach")
if (!require("doParallel"))install.packages("doParallel"); library("doParallel")






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
  stats::predict(coeff, newdata = newdata.df, type = "probs", se = TRUE, na.action = na.exclude)
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

# to determine the cut off. Obselete, we dont use cut off with MLR method
to_count_cutOff <- function (cutOff, datamap) {
  dataDummy <- as.data.frame(datamap)
  z     <- dataDummy >= cutOff
  sumZ  <- sum(z, na.rm=TRUE)
  return (sumZ)
}


# to transform original values into zero one range
to_zero_one <- function (datasetDummy) {
  minDummy <- min(as.matrix(datasetDummy), na.rm = TRUE)
  maxDummy <- max(as.matrix(datasetDummy), na.rm = TRUE)
  if (minDummy < 0) {minDummy=0}
  converted_dataset <- (datasetDummy - minDummy)/(maxDummy - minDummy)
  return (converted_dataset)
}

# to get the mode of a matrix
to_get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# to create a cross tabulation based on two raster maps with categorical values (does not need to have similar amount of classes)
to_crosstab <- function (obs, pred) {
  ctable <- (crosstabm( to_raster(obs), to_raster(pred)))
  View(ctable)
  return (ctable)
  
}

# to recalculate the urban neighborhood ratio (max 100) around the defined moving windows 
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





## 3. LOAD WORKING MAPS ================================================= 


##__3.1 -- Set working path $ DO CHANGE THIS PATH ####
setwd("~/UQ-Research (uq.edu.au)/KOALA2018-A0206/04 Model/CA-KoalaOffset")
# setwd ("M:/Projects/koala_offsets/04 Model/CA-KoalaOffset") ## server gpem-lsec2


##__3.2 -- Load working maps ####
saArea             <- raster("input/maps/seq_kpa_region.asc")
sa4                <- raster("input/maps/seq_sa4_code11.asc")
lga                <- raster("input/maps/seq_lga.asc")
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
plan2017           <- raster( "input/maps/seq_planning_scheme_2017b.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$seq_planningScheme2017b for description

plan2017.tb        <- read.csv("input/table/planningScheme2017.csv", header = TRUE) # planning scheme constraints table

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
names(plan2017)         <- ("plan2017")

names(lu1999)           <- ("lu1999")
names(lu2016)           <- ("lu2016")


# Dataset non its original unit values in data frame
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
                                         plan2010,
                                         plan2017,
                                         lu1999,
                                         lu2016))

# Conversion to "factor" data type on qualitative independent variables
MacroVar$sa4fact      <- factor(MacroVar$sa4)
MacroVar$UFfact       <- factor(MacroVar$UF)
MacroVar$ptfact       <- factor(MacroVar$protectArea)
MacroVar$rcfact       <- factor(MacroVar$recreArea)
MacroVar$plan2010fact <- factor(MacroVar$plan2010)
MacroVar$plan2017fact <- factor(MacroVar$plan2017)


# Land use data in data frame type
lu1999.df     <- as.data.frame(lu1999)
lu2016.df     <- as.data.frame(lu2016)
freq1999      <- as.data.frame(freq(lu1999))
freq2016      <- as.data.frame(freq(lu2016))
tp.zero       <- lu1999 - lu1999 # zero map


names(lu1999.df) <- "lu1999"
names(lu2016.df) <- "lu2016"

# remove orig variables to save mem
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
gc()


## __3.3 -- Conversion of factors to [0 1] , stacked, and df converted ########

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
stackMacroVar.df$plan2017 [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$plan2017fact[(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$plan2017fact[(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA


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





## 4. SIMULATION ===================================================== 

## __4.1 -- Prepare simulation parameters ####
print ('Prepare simulation parameters')

red           <- c(0,198,51,0,0,255,255,255,255,255,217,166,91)
green         <- c(92,224,204,153,128,217,199,139,83,235,217,166,155)
blue          <- c(0,182,51,0,0,222,206,153,103,156,217,166,213)
colors        <- rgb(red, green, blue, maxColorValue = 255)
breakpoints   <- c(0,10,21,22,23,30,40,51,52,53,60,71,72,80)
luLabel       <- c(10,21,22,23,30,40,51,52,53,60,71,72,80)
urbanDemand   <- c(71584, 63243, 11269 ) ## 51 52 53 urban land demand; \KOALA2018-A0206\04 Model\CA-KoalaOffset\output\table$bencmark cells Z42-44
luStay        <- c(80) ## Land classes that stay unchanged during the simulation run


initLU.df     <- lu2016.df$lu2016 
initYear      <- 2016   
nYearGap      <- 17
tSimul        <- 200000  
outputFold    <- ""

luSimul.ls    <- list()
nSimulation   <- 40     # number of simulation instances for nYearGap period. Minimum 30
freqLU        <- freq(to_raster(initLU.df))



# nYearGap simulation period start with initLU. finalYearLu = initLU+(nYearGap*tSimul) 
# example. For simulation with initial LU in 1999 and we want to simulate LU in 2016, 
# maximum nYearGap should be 17 year. This is the base on nYearGap of observed lu maps

# with single simulation loop of 17 years
# initLU.df = lu1999.df
# nYearGap  = 17
# tSimul    = 1
# finalYearLU = 1999 + (17*1) = 2016

# for forward simulation with 5 year gaps
# initLU.df = lu2016.df
# nYearGap  = 5
# tSimul    = 10
# finalYearLU = 2016 + (5*10) = 2066





## __4.2 -- Preparing the transition probability (tp) -- ####
print("-- update the transition probability according to recent LULC")



for (t in 0:tSimul) {
  
  ##____4.2.1 -- Update the tp according to initi LU-- ####
  
  ifelse(t == 0,
         print(paste("Map of landuse ",(nYearGap*t+initYear))), 
         print(paste("Simulating landuse of ",(nYearGap*t+initYear))))
  
  ifelse ( t == 0,  
           luDummy <- initLU.df ,         # at t=0, integer
           luDummy <- tp.cover$luDynmc)   # at t!=0, integer
  
  if (exists ("luSimul.df"))    remove (luSimul.df)
  if (exists ("tp.cover.df"))   remove (tp.cover.df)
  if (exists ("tp.cover"))      remove (tp.cover)
  if (exists ("tp.cover.ls"))   remove (tp.cover.ls)
  gc()
  
  
  
  ## ____4.2.2 -- Select sample point based on the current land use class ####
  
  orig_rank                   <- mutate(stackMacroVar.df, orig_rank = row_number()) #cell ID
  stackMacroVar.df$orig_rank  <- orig_rank$orig_rank
  stackMacroVar.df$luDummy    <- luDummy 
  remove(orig_rank, luDummy)
  gc()
  
  mlr_Dummy.df.ls <- list()  #sample points for regression 
  for (i in 1:length(luLabel)){
    print(paste("Select sampling point ", luLabel[i]))
    
    
    mlr_Dummy.df.ls[[i]] <- stackMacroVar.df %>% 
      dplyr::filter(luDummy == luLabel[i] )
    
  }
  
  
  
  
  ##____4.2.3 -- Predict the transition probability  ####
  
  tp.stats.ls       <- list()
  tp.stats.names.ls <- list()
  
  for (i in 1:length(luLabel)){
    print(paste("Estimate transition probability of ", luLabel[i]))
    
    macroVar                <- mlr_Dummy.df.ls[[i]]
    macroVar$LCfact         <- factor(macroVar$lu2016)  
    macroVar$sa4fact        <- factor(macroVar$sa4) 
    macroVar$UFfact         <- factor(macroVar$UF)
    macroVar$plan2010fact   <- factor(macroVar$plan2010)
    macroVar$plan2017fact   <- factor(macroVar$plan2017)
    

    fileName = paste("./input/mlrsummary_NeighUrb_UF/coefficient", i, ".rda", sep="") 
    load(fileName) 
    tp.dummy      <- to_predict(test, macroVar)
       
    # tp.dummy[is.na(tp.dummy)] <- 0
    
	
    tp.stats.ls[[i]]            <- tp.dummy # the estimated transition probability
    colnames(tp.stats.ls[[i]] ) <- dimnames(tp.dummy)[[2]]
    tp.stats.names.ls[[i]]      <- dimnames(tp.dummy)[[2]]
  }
  
  
  
  
  
  
  ## ____4.2.4 -- Assigning the TP to original data frame  ####
  
  tp.cover            <- stackMacroVar.df
  tp.names            <- c( "10", "21", "22", "23", "30", "40", "51","52", "53", "60", "71", "72", "80")
  tp.cover[,tp.names] <- 0
  
  
  for (i in 1:13 ){
    print(paste("Assigning estimated transition probability to dataframe ", luLabel[i]))
    indexDummy    <- which(tp.cover$luDummy == luLabel[i])
    dummy.df      <- tp.stats.names.ls[[i]]
    fieldMatch    <- match(dummy.df, names(tp.cover))
    tp.dummy      <- tp.stats.ls[[i]]
    tp.cover[indexDummy, fieldMatch]  <- tp.dummy
  }
  
  
  
  ##____4.2.5 -- Filter transition probability 0, return to original land class CONSTRAINTS  #### 
  print("Return NaN or sum zero transition probability into current land use")
  
  id10 = which(names(tp.cover)=="10")
  id51 = which(names(tp.cover)=="51")
  id52 = which(names(tp.cover)=="52")
  id53 = which(names(tp.cover)=="53")
  id80 = which(names(tp.cover)=="80")
  
  
  sumTP         <- rowSums(tp.cover[, id10:id80])
  
  for (i in 1:length(luLabel) ){
    idTP                          <- which(tp.cover$luDummy == luLabel[i] & sumTP== 0)
    tp.cover[idTP,id10+i-1]       <- 1
    idTP                          <- which(tp.cover$luDummy == luLabel[i] & is.na(sumTP))
    tp.cover[idTP,c(id10:id80)]   <- 0
    tp.cover[idTP,id10+i-1]       <- 1
    
    if (luLabel[i] %in% luStay){
      idTP                          <- which(tp.cover$luDummy == luLabel[i])
      tp.cover[idTP,c(id10:id80)]   <- 0
      tp.cover[idTP,id10+i-1]       <- 1}
    
  }
  
  
  
  
  ##____4.2.6 -- Applying Planning scheme as constraints####
  print("Applying planning scheme as constraints")
  
  tp.forward = tp.cover # duplicate tp.cover into tp with planning scheme as constraint.
  
  for (i in 1:dim(plan2017.tb)[1] ){
    for (j in 1:(dim(plan2017.tb)[2]-1) ){
    
      index1 = which(tp.forward$plan2017 == plan2017.tb$planningscheme[i] )
      consId = plan2017.tb[i,j+1]
      tp.forward[index1,id10+j-1] = tp.cover[index1,id10+j-1] * consId
    }
  }
  
  
  ## sum tp of one. according to current land use
  sumTP         <- rowSums(tp.forward[, id10:id80])
  
  for (i in 1:length(luLabel) ){
    idTP                          <- which(tp.forward$luDummy == luLabel[i]) 
    tp.forward[idTP,(id10+i-1)]   <- 1- sumTP [idTP] + tp.forward[idTP,(id10+i-1)] 
  }
  
  
  
  ##____4.2.7 -- Applying urban land demand Limit####
  print("Applying urban land demand limit")
  
  tp.luDemand = tp.forward # duplicate tp.forward to limit urban land demand.
  
  freq.CurrentLU    <- freq(to_raster(tp.forward$luDummy))
  freq51            <- freq.CurrentLU[7,-1]
  freq52            <- freq.CurrentLU[8,-1]
  freq53            <- freq.CurrentLU[9,-1]
  
  if (freq53 > urbanDemand[3] & t!=0){
    idTP                            <- which(tp.forward$luDummy == 53)
    tp.luDemand[idTP,c(id10:id80)]  <- 0
    tp.luDemand[idTP,id53]          <- 1
    
    idTP                            <- which(tp.forward$luDummy != 53)
    tp.luDemand[idTP,id53]          <- 0
    
    # tp.luDemand[idTP,c(id10:id80)]  <- prop.table(as.matrix(tp.luDemand[idTP,c(id10:id80)]), margin = 1)
    sumTP                           <- rowSums(tp.luDemand[, id10:id80])
    for (i in 1:length(luLabel) ){
      idTP                          <- which(tp.luDemand$luDummy == luLabel[i]) 
      tp.luDemand[idTP,(id10+i-1)]   <- 1- sumTP [idTP] + tp.luDemand[idTP,(id10+i-1)] 
    }
    
    if(freq52 > urbanDemand[2] & t!=0){
      idTP                            <- which(tp.forward$luDummy == 52)
      tp.luDemand[idTP,c(id10:id80)]  <- 0
      tp.luDemand[idTP,id52]          <- 1
      
      idTP                            <- which(tp.forward$luDummy != 52)
      tp.luDemand[idTP,id52]          <- 0
      
      sumTP                           <- rowSums(tp.luDemand[, id10:id80])
      for (i in 1:length(luLabel) ){
        idTP                          <- which(tp.luDemand$luDummy == luLabel[i]) 
        tp.luDemand[idTP,(id10+i-1)]   <- 1- sumTP [idTP] + tp.luDemand[idTP,(id10+i-1)] 
      }
      
    }
    
    
    
  }
  
  
  
  
  ## transition probability without NaN values
  tp.cover.nona.df <- tp.luDemand %>%
    mutate(tpSum = rowSums(.[id10:id80])) %>% 
    dplyr::filter(tpSum!=0) 
  
  
  
  
  
  
  
  
  
  
  ## __4.3 -- Updating TP according to the required nYearGap-- ####
  ## Create transition probability of one at t=0 based on lu1999.
  
  ## Annual tp. Based on multiyear change-rate (modified) "tp.ratio"
  tp.End    <- tp.cover.nona.df[,id10:id80] 
  nYear     <- 17
  eps       <- .Machine$double.eps^0.5 # default tolerance for small tp
  
  
  
  ifelse ( t == 0,  
           tp.Ratio      <- as.data.frame(tp.End - tp.End),   # at t=0, tp zero everywhere except on existing LU=1
           tp.Ratio      <- as.data.frame(1-((1-tp.End)^(nYearGap/nYear))))   # at t!=0 
  
  tp.Ratio[tp.Ratio < eps] <- 0
  tp.Ratio$Sum  <- rowSums(tp.Ratio)
  test          <- tp.Ratio
  
  ## sum tp of one. according to current land use
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
  gc()
  
  
  
  
  
  
  
  
  
  ##__4.4 -- Start simulation here####
  print("Start generating rnorm and simulation based on transition probability")
  
  
  ##____4.4.1 -- Generating rnom based on transition probability####
  no_cores      <- detectCores() - 1
  cl            <- makeCluster(no_cores)  # initiate parallel computing

  
  tp.simul      <- cbind(tp.cover.nona.df[,1:id10-1], tp.Ratio) # transition probability used for simulation
  luSimulStack  <- c() 
  
  ifelse ( t == 0,  
           nSimulationDummy <- 1 ,          # at t=0, no need to repeat simulation instances 
           nSimulationDummy <- nSimulation) 
  
  registerDoParallel(cl)  # use multicore, set to the number of our cores
  luSimulStack <- foreach (i=1:nSimulationDummy, .combine=cbind) %dopar% {                       # colomn bind the simulated LU map 
    crossprod(apply(tp.simul[, c(id10:id80)], 1, function(x) rmultinom(1,size = 1,x)),luLabel)}  # selection of land class based on TP
  stopImplicitCluster()
  gc()
  
  
  
  
  
  ##____4.4.2 -- Removing salt and pepper ####
  
  ## remove the salt and pepper using to_get_mode function
  
  print("Removing salt and pepper")
  
  
  
  luSimul.ls[[t+1]] <- luSimulStack
  
  
  ifelse ( t == 0,  
           luSimulMode   <- as.numeric(luSimulStack) ,          # at t=0, no need to repeat simulation instances 
           luSimulMode   <- parRapply(cl, luSimulStack, to_get_mode) ) # application of apply by row on parallel computing
  
  stopCluster(cl)  # stop parallel computing
  
  test0 <- tp.simul %>%  
    dplyr:: select(orig_rank )
  
  test0 <- cbind(test0, luSimulMode)
  names(test0)[2] <- "luDynmc"
  
  tp.cover$luDynmc <- c() 
  tp.cover<- dplyr::left_join(tp.cover, test0, by = "orig_rank")
  
  
  
  
  plot(to_raster(tp.cover$luDynmc), breaks=breakpoints,col=colors)
  title(paste("Simulation map of", t*nYearGap+initYear ))
  
  
  ##__4.5 -- Update the dynamic neighborhood urban ratio ####

  luDummy.rs        <- to_raster(tp.cover$luDynmc)
  luDummy.nu        <- to_neighUrb(luDummy.rs, 5, 5)
  luDummy.nu.df     <- as.data.frame(luDummy.nu)
  names(luDummy.nu.df) <- "NeighUrb"
  MacroVar$NeighUrb <- luDummy.nu.df$NeighUrb 
  stackMacroVar.df$NeighUrb <- to_zero_one(MacroVar$NeighUrb)
  stackMacroVar.df$NeighUrb [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
  
  freqLU <- cbind(freqLU, freq(to_raster(tp.cover$luDynmc))[,2])
  colnames(freqLU)[t+3] <- as.character(t)
  
  print(freqLU)
  remove(test0, luDummy.nu.df, luDummy.rs, luDummy.num, tp.luDemand, tp.forward, tp.dummy, tp.cover.nona.df, tp.Ratio, tp.simul, luSimulStack, macroVar)
  gc() # release memory after looping and removing variables
}





















## 6. EXPORT MAP FOR VISUAL PRESENTATION AND ACCURASY ASSESSMENT=============================

simultLu <- tp.cover$luDynmc
simultLu <- as.integer(simultLu)



dateDummy <- Sys.Date()
subDir    <- format(dateDummy, format="%Y%m%d")
mainDir   <- "output"
outputDir <- dir.create(file.path(getwd(), mainDir, subDir), showWarnings = FALSE)


# lengthLU        <- dim(luSimulStack[[1]])[1]
# luSimulStack.df <- data.frame(matrix(unlist(luSimulStack), nrow=lengthLU, byrow=T))
# luSimulStack.df$modLU   <- apply(luSimulStack.df,1, function(x) to_get_mode(x))



filen       <- paste( mainDir,"/", subDir , "/lu_simul_", initYear+(nYearGap*t),  sep="")
writeRaster(x = to_raster(simultLu), filename = paste(filen, ".asc",  sep=""), format = "ascii", overwrite=TRUE)
writeRaster(x = to_raster(simultLu), filename = paste(filen, ".tif",  sep=""), format = "GTiff", overwrite=TRUE)
writeRaster(x = to_raster(simultLu), filename = paste(filen, ".grd",  sep=""), overwrite=TRUE)


filen       <- paste( mainDir,"/", subDir ,  "/", sep="")	
write.csv(freqLU, paste(filen, "freqLU.csv",  sep=""))

















