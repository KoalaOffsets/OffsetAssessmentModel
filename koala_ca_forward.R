# CODE TO RUN THE FORWARD OFFSET SCENARIOS

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
library(snowfall)
library(foreach)
library(doParallel)
library(tictoc)

# load functions
source("functions.r")

# set up simulation inputs

# load input tables
Tables <- list()
Tables$plan2017tb <- read.csv("input/tables/planningScheme2017.csv", header = TRUE) # planning scheme constraints table for 2017
Tables$DwelDentb <- read.csv("input/tables/change_dwellings.csv", header = TRUE) # changes in dwelling density / ha for each transitions (based on average dwelling densities in 2016 from the 2016 meshblocks)
Tables$UFfacttb <- read.csv("input/tables/shapingseq_cons.csv", header = TRUE) # Land-use transitions in Shaping SEQ land uses not estimated (transitions to 52 and 53 in rural land use area not observed, so set to zero probability in simulation)
Tables$LVChangetb <- read.csv("input/tables/landval_change.csv", header = TRUE) # increase in land values from 2012 to 2021 for each LGA as sourced from https://www.data.qld.gov.au/dataset/historical-trends-in-land-valuations/resource/03f430d3-de9b-44ba-bc8b-a147ad00c080

## load input rasters
Rasters <- list()
Rasters$luarea <- raster("input/maps/luarea.asc") # 1 ha grids defining the study area
Rasters$lu2016 <- raster("input/maps/landuse16reclsuburb4.asc") # land use 2016
Rasters$plan2017 <- raster("input/maps/seq_planning_scheme_2017b.asc") # planning schemes for 2017
Rasters$slope <- raster("input/maps/slpfinal1.asc") # slope
Rasters$elev <- raster("input/maps/demfinal.asc") # elevation
Rasters$road <- raster("input/maps/SEQ_distRoad.asc") # distance to main roads
Rasters$city <- raster("input/maps/seq_cityDist.asc") # distance to cities
Rasters$roadDen <- raster("input/maps/seq_roadDens.asc") # road density (all roads)
Rasters$awc <- raster("input/maps/seq_awcMeans.asc") # soil mean available water content
Rasters$awc[is.na(Rasters$luarea)] <- NA # remove zeros outside the study area
Rasters$cly <- raster("input/maps/seq_clymeans1.asc") # soil clay content
Rasters$NeighUrb <- raster("input/maps/lu16rdevn1.asc") # proportion of residential dev (51,52,53) in a 5 x 5 moving window (ignoring NAs)
Rasters$NeighInd <- raster("input/maps/lu16indn1.asc") # proportion of commercial and industrial (60,72) in a 5 x 5 moving window (ignoring NAs)
Rasters$UFfact <- raster("input/maps/seq_urbanfootprint_2017fin.asc") # [0:regional 40:rural 50:urban] Shaping SEQ
Rasters$lgasfact <- raster("input/maps/lgas.asc") # LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan]
Rasters$LGAExistUrb <- raster("input/maps/lgaexisturb.asc") # LGAs and whether existing urban areas or not 1 = LGA 1 expansion, 2 = LGA 1 consolidation, ...., 15 = LGA 8 expansion, 16 = LGA 8 consolidation. LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan] & existing urban areas as defined in Shaping SEQ [1 = existing urban area, 0 = not existing urban area]
Rasters$HabHM <- raster("input/maps/habhm.asc") # High to medium core habitat area - HSM rules 351, 341, 331, 350, 340, 330
Rasters$HabML <- raster("input/maps/habml.asc") # Medium to low core habitat area - HSM rules 251, 241, 231
Rasters$HabVL <- raster("input/maps/habvl.asc") # Very low core habitat area - HSM rules 250, 240, 230
Rasters$HabCore <- Rasters$HabHM + Rasters$HabML + Rasters$HabVL # Total core habitat area
Rasters$HabNonCore <- raster("input/maps/habnc.asc") # Non-core habitat area - HSM rules 151, 150, 141, 140, 131, 130, 321, 320, 221, 220, 121
Rasters$HabHMPre <- raster("input/maps/habhmp.asc") # High to medium core habitat area pre-clearing - HSM rules 351, 341, 331, 350, 340, 330
Rasters$HabMLPre <- raster("input/maps/habmlp.asc") # Medium to low core habitat area pre-clearing - HSM rules 251, 241, 231
Rasters$HabVLPre <- raster("input/maps/habvlp.asc") # Very low core habitat area pre-clearing - HSM rules 250, 240, 230
Rasters$HabCorePre <- Rasters$HabHMPre + Rasters$HabMLPre + Rasters$HabVLPre # Total core habitat area pre-clearing
Rasters$HabNonCorePre <- raster("input/maps/habncp.asc") # Non-core habitat area pre-clearing - HSM rules 151, 150, 141, 140, 131, 130, 321, 320, 221, 220, 121
Rasters$KDen <- raster("input/maps/denmod6f.asc") # koala density from Model 6 (Rhodes et al. 2015)
Rasters$KadaBOU <- raster("input/maps/kadabnurbf.asc") # koala habitat protected - bushland habitat in KADA area outside of the urban footprint and outside of urban use (urban use defined as classes 4, 36, 18, 10, 20, 23, 26, 27, 12, 2, 3, 14, 19, 1, 29, 13, 17, 22, 39, 6 in the 2017 planning scheme)
Rasters$PKadaB <- raster("input/maps/pkadabf.asc") # koala habitat protected - bushland habitat in PKADA
Rasters$KadaBHMR <- raster("input/maps/kadabhmrf.asc") # koala habitat impacts to be offset (if not already protected) - all koala bushland habitat and high and medium values rehab
Rasters$KadaHMR <- raster("input/maps/kadahmrf.asc") # areas suitable for offsets - all high and medium values rehab
Rasters$KadaLR <- raster("input/maps/kadalrf.asc") # back-up areas suitable for offsets - all low value rehab
Rasters$HabKpa <- raster("input/maps/habkpaf.asc") # habitat in koala priority areas
Rasters$HabNotKpa <- raster("input/maps/habnkpaf.asc") # habitat outside of koala priority areas
Rasters$RestKpa <- raster("input/maps/restkpaf.asc") # restoration areas in koala priority areas
Rasters$RestNotKpa <- raster("input/maps/restnkpaf.asc") # restoration areas outside koala priority areas
Rasters$Kpa <- raster("input/maps/kpaidf.asc") # ID of koala priority area (eight KPAs with IDs = 0,2,3,4,5,7,8,9)
Rasters$KpaClo <- raster("input/maps/kpaclof.asc") # ID of closest koala priority area
Rasters$KpaSecClo <- raster("input/maps/kpa2clof.asc") # ID of second closest koala priority area
Rasters$Pda <- raster("input/maps/pda_fin.asc") # koala habitat not protected nor offsets required - Priority Development Area
Rasters$Kra <- raster("input/maps/krafin.asc") # koala habitat not protected nor offsets required - Key Resource Area
Rasters$Kbha <- raster("input/maps/kbhafin.asc") # koala habitat not protected, but offsets required - Koala Broad Hectare Areas
Rasters$Sda <- raster("input/maps/sdafin.asc") # State Development Areas
Rasters$LandVal <- raster("input/maps/landval.asc") # unimproved land value from 2012 (QVAS data). Here have estimated missing or zero values using an inverse distance weighted interpolation (exponential with exponent 2 which had the lowest RMS) and assumed national parks and state land have land values of zero. This is used to estimate the incentive payment actually required for an offset site at a particular location.
Rasters$LandVal <- to_raster((as.data.frame(Rasters$LandVal)$layer * (as.data.frame(Rasters$lgasfact) %>% left_join(Tables$LVChangetb, by = c("layer" = "LGA")))$Change) + 1, Rasters$lu2016) # increase land values by the increase in unimproved land values between 2012 and 2021
Rasters$FinIncent <- raster("input/maps/finincent.asc") # financial incentive payment per hectare for each LGA for cost of financial offsets calculation (taken from Queensland Environmental Offsets Policy v1.1 2021 =- Table 4.5.3)
Rasters$TenurePrivate <- raster("input/maps/tenfhll.asc") # private freehold and leasehold land (Based on most common tenure in each grid) - used to define land potentially available to developers for offset sites
Rasters$TenurePrivateState <- raster("input/maps/tenfhllnpsl.asc") # private freehold and leasehold land, plus national parks and state lands (Based on most common tenure in each grid) - used to define land potentially available to State Government for offset sites

# apply names to rasters
names(Rasters$luarea) <- "luarea"
names(Rasters$lu2016) <- "lu2016"
names(Rasters$plan2017) <- "plan2017"
names(Rasters$slope) <- "slope"
names(Rasters$elev) <- "elev"
names(Rasters$road) <- "road"
names(Rasters$city) <- "city"
names(Rasters$roadDen) <- "roadDen"
names(Rasters$awc) <- "awc"
names(Rasters$cly) <- "cly"
names(Rasters$NeighUrb) <- "NeighUrb"
names(Rasters$NeighInd) <- "NeighInd"
names(Rasters$UFfact) <- "UFfact"
names(Rasters$lgasfact) <- "lgasfact"
names(Rasters$LGAExistUrb) <- "LGAExistUrb"
names(Rasters$HabHM) <- "HabHM"
names(Rasters$HabML) <- "HabML"
names(Rasters$HabVL) <- "HabVL"
names(Rasters$HabCore) <- "HabCore"
names(Rasters$HabNonCore) <- "HabNonCore"
names(Rasters$HabHMPre) <- "HabHMPre"
names(Rasters$HabMLPre) <- "HabMLPre"
names(Rasters$HabVLPre) <- "HabVLPre"
names(Rasters$HabCorePre) <- "HabCorePre"
names(Rasters$HabNonCorePre) <- "HabNonCorePre"
names(Rasters$KDen) <- "KDen"
names(Rasters$KadaBOU) <- "KadaBOU"
names(Rasters$PKadaB) <- "PKadaB"
names(Rasters$KadaBHMR) <- "KadaBHMR"
names(Rasters$KadaHMR) <- "KadaHMR"
names(Rasters$KadaLR) <- "KadaLR"
names(Rasters$HabKpa) <- "HabKpa"
names(Rasters$HabNotKpa) <- "HabNotKpa"
names(Rasters$RestKpa) <- "RestKpa"
names(Rasters$RestNotKpa) <- "RestNotKpa"
names(Rasters$Kpa) <- "Kpa"
names(Rasters$KpaClo) <- "KpaClo"
names(Rasters$KpaSecClo) <- "KpaSecClo"
names(Rasters$Pda) <- "Pda"
names(Rasters$Kra) <- "Kra"
names(Rasters$Kbha) <- "Kbha"
names(Rasters$Sda) <- "Sda"
names(Rasters$LandVal) <- "LandVal"
names(Rasters$FinIncent) <- "FinIncent"
names(Rasters$TenurePrivate) <- "TenurePrivate"
names(Rasters$TenurePrivateState) <- "TenurePrivateState"

# load input land-use transition estimates and habitat loss estimates models
Models <- list()
# clearing probability model
load("input/clearing/models/clearing_predictions.Rda")
Models$ClearPred <- ClearPred
rm(ClearPred)
# land-use transition probability model
luLabel <- c(21, 22, 23, 40, 51, 52, 53, 60, 71, 72)
luLabelList <- list()
TMods <- list()
for (i in 1:length(luLabel)){
  fileName = paste("input/mlrsummary/model", i, ".rda", sep="")
  load(fileName)
  TMods[[i]] <- temp_mod
  luLabelList[[i]] <- luLabel[i]
}
Models$TMods <- TMods
Models$luLabel <- luLabel
Models$luLabelList <- luLabelList
rm(luLabel)
rm(luLabelList)
rm(TMods)

# set up simulations

ValuesConsol <- c(100, 75, 50)
ValuesReg <- c(FALSE, TRUE)
ValuesOff <- c("none", "proponent", "financialhab", "financialkoala")
ValuesAvail <- c(100, 90, 80, 70, 60, 50, 40, 30, 20, 17.5, 15, 12.5 10, 7.5 5, 2.5, 1)
ValuesNPS <- c(FALSE, TRUE)

# set up parallel processing
cl <- makeCluster(10)  # initiate parallel computing, set to the number of cores
registerDoParallel(cl)  # use multicore

for (i in ValuesConsol) {
  for (j in ValuesReg) {
    if (j == FALSE) {
      # set simulation parameters
      Params <- list()
      Params$MaxIter <- 1000 # the maximum number of iterations that each simulation can run for
      Params$ReportSteps <- 5 # how often (how many iterations) to report and record outputs
      Params$Horizon <- 2041 # time horizon relative to Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
      Params$PercConsol <- i # the percentage of the consolidation dwelling targets assumed (reducing the values below 100 results in a more expansion-like scenario)
      Params$Reg <- j # whether regulation considered or not - TRUE or FALSE
      Params$OffType <- "none" # offset rule used - "none" = no offsets, "proponent" = proponent driven offsets (assumes developers are cost minimisers in choosing offset sites), "financialkoala" = financial offsets (assumed government maximises koala numbers cost effectiveness), or "financialhab" = financial offsets (assumed government maximises habitat area cost effectiveness)
      Params$NPSLAvail <- FALSE # whether national parks and state lands are available for financial offset site selection - TRUE or FALSE
      Params$Multiplier <- 3 # the multiplier applied
      Params$PercAvail <- 100 # percent of potential offset sites available
      Params$RestSucc <- 1 # probability that restoration is successful in the long run
      Params$OnGroundCostHa <- 20000 # on-ground cost per hectare restored (used for proponent driven and financial offsets)
      Params$PercAdminCost <- 25 # administration costs as percentage of on-ground costs
      Params$MaxFinCost <- 230000 # maximum cost per impact hectare for financial offsets

      foreach (s = 1:10, .packages = c("tidyverse", "raster", "nnet", "tictoc")) %dopar% {
        Output <- RunOffSim(TablesSim = Tables, RastersSim = Rasters, ModelsSim = Models, ParamsSim = Params)
        saveRDS(Output, paste("sim_results/", "rep_", s, "_reg_", Params$Reg, "_consol_", Params$PercConsol, ".rds", sep = ""))
        rm(Output)
        gc()
      }
    } else {
      for(k in ValuesOff) {
        if (k == "none") {
          # set simulation parameters
          Params <- list()
          Params$MaxIter <- 1000 # the maximum number of iterations that each simulation can run for
          Params$ReportSteps <- 5 # how often (how many iterations) to report and record outputs
          Params$Horizon <- 2041 # time horizon relative to Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
          Params$PercConsol <- i # the percentage of the consolidation dwelling targets assumed (reducing the values below 100 results in a more expansion-like scenario)
          Params$Reg <- j # whether regulation considered or not - TRUE or FALSE
          Params$OffType <- k # offset rule used - "none" = no offsets, "proponent" = proponent driven offsets (assumes developers are cost minimisers in choosing offset sites), "financialkoala" = financial offsets (assumed government maximises koala numbers cost effectiveness), or "financialhab" = financial offsets (assumed government maximises habitat area cost effectiveness)
          Params$NPSLAvail <- FALSE # whether national parks and state lands are available for financial offset site selection - TRUE or FALSE
          Params$Multiplier <- 3 # the multiplier applied
          Params$PercAvail <- 100 # percent of potential offset sites available
          Params$RestSucc <- 1 # probability that restoration is successful in the long run
          Params$OnGroundCostHa <- 20000 # on-ground cost per hectare restored (used for proponent driven and financial offsets)
          Params$PercAdminCost <- 25 # administration costs as percentage of on-ground costs (used for proponent driven and financial offsets)
          Params$MaxFinCost <- 230000 # maximum cost per impact hectare for financial offsets

          foreach (s = 1:10, .packages = c("tidyverse", "raster", "nnet", "tictoc")) %dopar% {
            Output <- RunOffSim(TablesSim = Tables, RastersSim = Rasters, ModelsSim = Models, ParamsSim = Params)
            saveRDS(Output, paste("sim_results/", "rep_", s, "_reg_", Params$Reg, "_consol_", Params$PercConsol, "_offtype_", Params$OffType, ".rds", sep = ""))
            rm(Output)
            gc()
          }
        } else {
          if (k == "proponent") {
            for (l in ValuesAvail) {
              # set simulation parameters
              Params <- list()
              Params$MaxIter <- 1000 # the maximum number of iterations that each simulation can run for
              Params$ReportSteps <- 5 # how often (how many iterations) to report and record outputs
              Params$Horizon <- 2041 # time horizon relative to Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
              Params$PercConsol <- i # the percentage of the consolidation dwelling targets assumed (reducing the values below 100 results in a more expansion-like scenario)
              Params$Reg <- j # whether regulation considered or not - TRUE or FALSE
              Params$OffType <- k # offset rule used - "none" = no offsets, "proponent" = proponent driven offsets (assumes developers are cost minimisers in choosing offset sites), "financialkoala" = financial offsets (assumed government maximises koala numbers cost effectiveness), or "financialhab" = financial offsets (assumed government maximises habitat area cost effectiveness)
              Params$NPSLAvail <- FALSE # whether national parks and state lands are available for financial offset site selection - TRUE or FALSE
              Params$Multiplier <- 3 # the multiplier applied
              Params$PercAvail <- l # percent of potential offset sites available
              Params$RestSucc <- 1 # probability that restoration is successful in the long run
              Params$OnGroundCostHa <- 20000 # on-ground cost per hectare restored (used for proponent driven and financial offsets)
              Params$PercAdminCost <- 25 # administration costs as percentage of on-ground costs (used for proponent driven and financial offsets)
              Params$MaxFinCost <- 230000 # maximum cost per impact hectare for financial offsets

              foreach (s = 1:10, .packages = c("tidyverse", "raster", "nnet", "tictoc")) %dopar% {
                Output <- RunOffSim(TablesSim = Tables, RastersSim = Rasters, ModelsSim = Models, ParamsSim = Params)
                saveRDS(Output, paste("sim_results/", "rep_", s, "_reg_", Params$Reg, "_consol_", Params$PercConsol, "_offtype_", Params$OffType, "_percavail_", Params$PercAvail, ".rds", sep = ""))
                rm(Output)
                gc()
              }
            }
          }
          else {
            for (l in ValuesAvail) {
                for (m in ValuesNPS) {
                # set simulation parameters
                Params <- list()
                Params$MaxIter <- 1000 # the maximum number of iterations that each simulation can run for
                Params$ReportSteps <- 5 # how often (how many iterations) to report and record outputs
                Params$Horizon <- 2041 # time horizon relative to Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
                Params$PercConsol <- i # the percentage of the consolidation dwelling targets assumed (reducing the values below 100 results in a more expansion-like scenario)
                Params$Reg <- j # whether regulation considered or not - TRUE or FALSE
                Params$OffType <- k # offset rule used - "none" = no offsets, "proponent" = proponent driven offsets (assumes developers are cost minimisers in choosing offset sites), "financialkoala" = financial offsets (assumed government maximises koala numbers cost effectiveness), or "financialhab" = financial offsets (assumed government maximises habitat area cost effectiveness)
                Params$NPSLAvail <- m # whether national parks and state lands are available for financial offset site selection - TRUE or FALSE
                Params$Multiplier <- 3 # the multiplier applied
                Params$PercAvail <- l # percent of potential offset sites available
                Params$RestSucc <- 1 # probability that restoration is successful in the long run
                Params$OnGroundCostHa <- 20000 # on-ground cost per hectare restored (used for proponent driven and financial offsets)
                Params$PercAdminCost <- 25 # administration costs as percentage of on-ground costs
                Params$MaxFinCost <- 230000 # maximum cost per impact hectare for financial offsets

                foreach (s = 1:10, .packages = c("tidyverse", "raster", "nnet", "tictoc")) %dopar% {
                  Output <- RunOffSim(TablesSim = Tables, RastersSim = Rasters, ModelsSim = Models, ParamsSim = Params)
                  saveRDS(Output, paste("sim_results/", "rep_", s, "_reg_", Params$Reg, "_consol_", Params$PercConsol, "_offtype_", Params$OffType, "_percavail_", Params$PercAvail, "_npslavail_", Params$NPSLAvail, ".rds", sep = ""))
                  rm(Output)
                  gc()
                }
              }
            }
          }
        }
      }
    }
  }
}

# close cores
stopImplicitCluster()
