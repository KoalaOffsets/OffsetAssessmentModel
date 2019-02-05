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

stackMacroVar.df$lu1999   [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
stackMacroVar.df$lu2016   [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
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

initLU.df     <- lu1999.df$lu1999  #lu2016.df$lu2016 
initYear      <- 1999 # 2016   
nYearGap      <- 6   # maximum n year gap is 17
tSimul        <- 3  
outputFold    <- ""

luSimul.ls    <- list()
nSimulation   <- 30     # number of simulation instances for nYearGap period. Minimum 30



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





## __4.2 -- Prepariong the transition probability (tp) -- ####
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
    
    levels(macroVar$LCfact)
    
    macroVar$LCsort <- relevel(macroVar$LCfact, ref = "51") # sort? kind of? reference level
    
    fileName = paste("./input/mlrsummary_NeighUrb_UF/coefficient", i, ".rda", sep="") 
    
    load(fileName) 
    # the following supresses error message when new level factor appears on sampling data. Warning: Bias
    # test$xlevels$plan2017fact <- levels(macroVar$plan2017fact) 
    idDummy       <- macroVar$plan2017 %in% as.numeric(test$xlevels$plan2017fact)
    macroVar.flt  <- macroVar[idDummy,]
    
    tp.dummy <- to_predict(test, macroVar.flt)
    
    

    # tp.dummy[is.na(tp.dummy)] <- 0
    
    tp.dummy.a                  <- matrix(0, nrow= dim(macroVar)[1], ncol = length(dimnames(tp.dummy)[[2]]))
    colnames(tp.dummy.a)        <- dimnames(tp.dummy)[[2]]
    tp.dummy.a[idDummy,]        <- tp.dummy
    tp.stats.ls[[i]]            <- tp.dummy.a # the estimated transition probability
    colnames(tp.stats.ls[[i]] ) <- dimnames(tp.dummy)[[2]]
    
    tp.stats.names.ls[[i]]      <- dimnames(tp.dummy)[[2]]
  }
  
  
  
  
  
  
  ## ____4.2.5 -- Assigning the TP to original data frame  ####
  
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
  
  
  
  ##____4.2.6 -- Filter transition probability 0  #### 
  
  id10 = which(names(tp.cover)=="10")
  id80 = which(names(tp.cover)=="80")
  
  tp.cover.nona.df <- tp.cover %>%
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
  tp.simul      <- cbind(tp.cover.nona.df[,1:id10-1], tp.Ratio) # transition probability used for simulation
  luSimulStack  <- c() 
  
  ifelse ( t == 0,  
           nSimulationDummy <- 1 ,          # at t=0, no need to repeat simulation instances 
           nSimulationDummy <- nSimulation) 
  
  for (i in 1:nSimulationDummy){
    print(paste("Simulation no ", i, " of ", nSimulationDummy, sep = ""))
    tp.simul$luDynmc <- crossprod(apply(tp.simul[, c(id10:id80)], 1, function(x) rmultinom(1,size = 1,x)),luLabel)
    
    test0 <- tp.simul %>% 
      dplyr:: select(orig_rank,luDynmc ) 
    
    tp.cover$luDynmc <- c()
    tp.cover<- dplyr::left_join(tp.cover, test0, by = "orig_rank")
    
    
    luSimulStack <- cbind(luSimulStack, tp.cover$luDynmc) 
    
  }
  
  ## colomn bind the simulated LU map 
  ## remove the salt and pepper using to_get_mode function
  
  luSimul.ls[[t+1]] <- luSimulStack
  
  ifelse ( t == 0,  
           luSimulMode   <- as.numeric(tp.cover$luDynmc) ,          # at t=0, no need to repeat simulation instances 
           luSimulMode   <- apply(luSimulStack,1, function(x) to_get_mode(x)) )
  
  plot(to_raster(luSimulMode), breaks=breakpoints,col=colors)
  title(paste("Simulation map of", t*nYearGap+initYear ))
  
  
  ##__4.5 -- Update the dynamic neighborhood urban ratio ####
  tp.cover$luDynmc  <- luSimulMode
  luDummy.rs        <- to_raster(tp.cover$luDynmc)
  luDummy.nu        <- to_neighUrb(luDummy.rs, 5, 5)
  luDummy.nu.df     <- as.data.frame(luDummy.nu)
  names(luDummy.nu.df) <- "NeighUrb"
  MacroVar$NeighUrb <- luDummy.nu.df$NeighUrb 
  stackMacroVar.df$NeighUrb <- to_zero_one(MacroVar$NeighUrb)
  stackMacroVar.df$NeighUrb [(stackMacroVar.df$protectArea != 0) | (stackMacroVar.df$recreArea != 0) ] <- NA
  
  remove(test0, luDummy.nu.df, luDummy.rs, luDummy.nu)
  gc() # release memory after looping and removing variables
}





















## 5. ACCURACY ASSESSMENT USING KAPPA simulation (CELL TO CELL)=================


## __5.1 -- Kappa index  ####

intialLU <- initLU.df
actualLu <- tp.cover$lu2016
simultLu <- tp.cover$luDynmc
simultLu <- as.integer(simultLu)

intialLU[is.na(simultLu)] <- NA
actualLu[is.na(simultLu)] <- NA

# plot(stack(to_raster(actualLu), to_raster(simultLu)), breaks=breakpoints,col=colors)



source("R_functions/kappasimulation.R")
ctable.act.sim <- to_crosstab(actualLu, simultLu)   # crosstab between actual vs simulated LU map
ctable.int.sim <- to_crosstab(intialLU, simultLu)   # crosstab between intial vs simulated LU map
ctable.int.act <- to_crosstab(intialLU, actualLu)   # crosstab between intial vs actual LU map

kappa.output  <- Kappa (ctable.act.sim)
print(kappa.output)

kappa.txt <- cbind( po=kappa.output$po,
                    pe=kappa.output$pe,
                    pm=kappa.output$pe_max,
                    ko=kappa.output$KAPPA,
                    kh=kappa.output$kappa_histogram,
                    kl=kappa.output$kappa_location)




source("R_functions/kappa_rossiter.R")
kappa.Rositer <- kappa(ctable.act.sim)
summary.kappa(kappa.Rositer, alpha=0.05)
kappa.Rositer.txt <- cbind(UserNaive = kappa.Rositer$user.naive, 
                           ProdNaive = kappa.Rositer$prod.naive,
                           UserKappa = kappa.Rositer$user.kappa, 
                           ProdKappa = kappa.Rositer$prod.kappa)


## __5.2 -- Missed-hit accuracy assessment  ####
source("R_functions/missedHit.R")
tp.cover$luDynmc  <- as.integer(tp.cover$luDynmc)


# For calibrating observed 1999-2016 vs simulated 1999-2016
missedHitLU <- mutate(tp.cover, luChange = 0) %>%  
  mutate(luChange = ifelse((lu1999 == lu2016) & (lu1999 == luDynmc) ,  11,                    # 11 Correct non-changed lu
                           ifelse((lu1999 == lu2016) & (lu1999 != luDynmc), 12,               # 12 False alarm
                                  ifelse((lu1999 != lu2016) & (lu1999 == luDynmc), 21, 22)))) # 21 Missed-hit
# 22 correct changes
obs           <- to_raster(actualLu)
changeLu      <- to_raster(as.integer(missedHitLU$luChange ))
missedHit.txt <- missedHit(obs, changeLu) # assessment based on actualLU









## 6. EXPORT MAP FOR VISUAL PRESENTATION AND ACCURASY ASSESSMENT=============================

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


filen       <- paste( mainDir,"/", subDir , "/lu_simul_change_", initYear, "_", initYear+(nYearGap*t), sep="")	
writeRaster(x = to_raster(changeLu), filename = paste(filen, ".asc",  sep=""), format = "ascii", overwrite=TRUE)
writeRaster(x = to_raster(changeLu), filename = paste(filen, ".tif",  sep=""), format = "GTiff", overwrite=TRUE)
writeRaster(x = to_raster(changeLu), filename = paste(filen, ".grd",  sep=""), overwrite=TRUE)


filen       <- paste( mainDir,"/", subDir ,  "/", sep="")	
write.csv(kappa.Rositer.txt, paste(filen, "kappa.Rositer.csv",  sep=""))
write.csv(kappa.txt,         paste(filen, "kappa.csv",  sep=""))
write.csv(missedHit.txt,     paste(filen, "missedHit.csv",  sep=""))
write.csv(ctable.act.sim,    paste(filen, "ctable.act.sim.csv",  sep=""))
write.csv(ctable.int.sim,    paste(filen, "ctable.int.sim.csv",  sep=""))
write.csv(ctable.int.act,    paste(filen, "ctable.int.act.csv",  sep=""))




