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

# Set working directory (***CHANGE TO PATH WHERE DATA ARE***)
setwd("R:/KOALA2018-A0206/model/CA-KoalaOffset")
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# INSTALL PACKAGES

if (!require("tidyverse")) devtools::install_github("tidyverse/tidyverse"); library("tidyverse")
if (!require("raster")) install.packages("raster"); library("raster")
if (!require("nnet")) install.packages("nnet"); library("nnet")
if (!require("snowfall")) install.packages("snowfall"); library("snowfall")
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")

# FUNCTIONS

# function to set transition to zero based on land transitions that are not possible for all cells
set_trans_zero <- function(Preds, ClassesNotPoss) {
# Preds in the predicted probabilities
# ClassesNotPoss is a character string of the class that cannot transition to

  # set probabilities to zero
  Preds[,intersect(names(Preds), ClassesNotPoss)] <- 0

  # rescale so probs sum to 1
  Sums <- rowSums(Preds)
  Preds <- Preds / Sums

  return(Preds)
}

# function to create raster from data frame
to_raster <- function(df, rast_template) {
  NewRast <- raster(t(matrix(df, ncol = dim(rast_template)[1], nrow = dim(rast_template)[2])))
  extent(NewRast) <- extent(rast_template)
  projection(NewRast) <- projection(rast_template)
  return(NewRast)
}

# function to allocate offset sites in an LGA - returns the indices of allocated sites, the cost of land purchase, and any remaining impacts not offset yet
get_off_sites_lga <- function(Lga, Lgas.df, OffArea.df, OffAreaB.df, Costs, Target) {

  # check if offsets required
  if (Target > 0) {
    # get cost effectiveness - hectares purchased per $1000
    CostE <- 1000 / Costs
    CostEB <- 1000 / Costs

    #consider only those areas where an offset is possible
    CostE[which(is.na(OffArea.df))] <- NA
    CostEB[which(is.na(OffAreaB.df))] <- NA

    # get order based on cost effectiveness
    Order <- order(CostE, OffArea.df, decreasing = TRUE, na.last = TRUE)
    OrderB <- order(CostEB, OffAreaB.df, decreasing = TRUE, na.last = TRUE)

    OrderLGA <- Order[which(Lgas.df[Order] == Lga)]
    OrderBLGA <- OrderB[which(Lgas.df[OrderB] == Lga)]

    SortAreasLGA <- OffArea.df[OrderLGA]
    SortAreasBLGA <- OffAreaB.df[OrderBLGA]

    # get summed areas ranked from most to least cost effectiveness
    SortAreasSumLGA <- cumsum(SortAreasLGA)
    SortAreasBSumLGA <- cumsum(SortAreasBLGA)

    # there is enough area for restoration needed in LGA
    if (max(SortAreasSumLGA, na.rm = TRUE) >= Target) {

      #  get the closest value that is at least as large as the target
      DiffTarg <- (SortAreasSumLGA - Target) ^ 2
      MinIndex <- which.min(DiffTarg)

      # allocate needed in offset sites
      Needed <- OrderLGA[1:MinIndex]
      Areas <- OffArea.df[Needed]
      Left <- Target - sum(Areas, na.rm = T)
      ExitFlag <- 1
    }
    # not enough area for restoration so use the backup
    else {
      # get all allocations in the offset sites area
      Needed1 <- OrderLGA[1:which.max(SortAreasSumLGA)]

      # there is enough area for restoration needed in LGA in the backup area
      if (max(SortAreasBSumLGA, na.rm = TRUE) >= (Target - max(SortAreasSumLGA, na.rm = TRUE))) {

        #  get the closest value that is at least as large as the target for the Backup sites
        DiffTarg <- (SortAreasBSumLGA - (Target - max(SortAreasSumLGA, na.rm = TRUE))) ^ 2
        MinIndex <- which.min(DiffTarg)

        # allocate needed in backup sites
        Needed2 <- OrderBLGA[1:MinIndex]
        Areas <- c(OffArea.df[Needed1], OffAreaB.df[Needed2])
        Left <- Target - sum(Areas, na.rm = T)
        ExitFlag <- 1
      }
      else {
        # get all allocations in the offset sites backup area
        Needed2 <- OrderBLGA[1:which.max(SortAreasBSumLGA)]
        Areas <- c(OffArea.df[Needed1], OffAreaB.df[Needed2])
        Left <- Target - sum(Areas, na.rm = T)
        ExitFlag <- -1
      }

      #allocate needed
      Needed <- c(Needed1, Needed2)
    }

    # get the costs and areas
    Area <- sum(Areas, na.rm = T)
    NotAlloc <- Target - Area
    Cost <- sum((Costs[Needed] + 90901 + 14600) * Areas, na.rm = TRUE) # restoration costs from Rhodes et al. (2019)

    # get output
    Output <- list(Needed, NotAlloc, Cost, ExitFlag)
    names(Output) <- c("OffsetLoc", "NotAlloc", "Cost", "ExitFlag")
    return(Output)
  } else {
    # get output
    Output <- list(vector(), 0, 0, 1)
    names(Output) <- c("OffsetLoc", "NotAlloc", "Cost", "ExitFlag")
    return(Output)
  }
}

# function to allocate offset sites in an LGA - returns the indices of allocated sites, the cost of land purchase, and any remaining impacts not offset yet
get_off_sites_anywhere <- function(OffArea.df, OffAreaB.df, Costs, Target) {

# check if offsets required
if (Target > 0) {

    # get cost effectiveness - hectares purchased per $1000
    CostE <- 1000 / Costs
    CostEB <- 1000 / Costs

    #consider only those areas where an offset is possible
    CostE[which(is.na(OffArea.df))] <- NA
    CostEB[which(is.na(OffAreaB.df))] <- NA

    # get order based on cost effectiveness
    Order <- order(CostE, OffArea.df, decreasing = TRUE, na.last = TRUE)
    OrderB <- order(CostEB, OffAreaB.df, decreasing = TRUE, na.last = TRUE)

    SortAreas <- OffArea.df[Order]
    SortAreasB <- OffAreaB.df[OrderB]

    # get summed areas ranked from most to least cost effectiveness
    SortAreasSum <- cumsum(SortAreas)
    SortAreasBSum <- cumsum(SortAreasB)

    # there is enough area for restoration needed
    if (max(SortAreasSum, na.rm = TRUE) >= Target) {

      #  get the closest value that is at least as large as the target
      DiffTarg <- (SortAreasSum - Target) ^ 2
      MinIndex <- which.min(DiffTarg)

      # allocate needed in offset sites
      Needed <- Order[1:MinIndex]
      Areas <- OffArea.df[Needed]
      Left <- Target - sum(Areas, na.rm = T)
      ExitFlag <- 1
    }
    # not enough area for restoration so use the backup
    else {
      # get all allocations in the offset sites area
      Needed1 <- Order[1:which.max(SortAreasSum)]

      # there is enough area for restoration needed in the backup area
      if (max(SortAreasBSum, na.rm = TRUE) >= (Target - max(SortAreasSum, na.rm = TRUE))) {

        #  get the closest value that is at least as large as the target for the Backup sites
        DiffTarg <- (SortAreasBSum - (Target - max(SortAreasSum, na.rm = TRUE))) ^ 2
        MinIndex <- which.min(DiffTarg)

        # allocate needed in backup sites
        Needed2 <- OrderB[1:MinIndex]
        Areas <- c(OffArea.df[Needed1], OffAreaB.df[Needed2])
        Left <- Target - sum(Areas, na.rm = T)
        ExitFlag <- 1
      }
      else {
        # get all allocations in the offset sites backup area
        Needed2 <- OrderB[1:which.max(SortAreasBSum)]
        Areas <- c(OffArea.df[Needed1], OffAreaB.df[Needed2])
        Left <- Target - sum(Areas, na.rm = T)
        ExitFlag <- -1
      }

      #allocate needed
      Needed <- c(Needed1, Needed2)
    }

    # get the costs and areas
    Area <- sum(Areas)
    NotAlloc <- Target - Area
    Cost <- sum((Costs[Needed] + 90901 + 14600) * Areas, na.rm = TRUE) # restoration costs from Rhodes et al. (2019)

    # get output
    Output <- list(Needed, NotAlloc, Cost, ExitFlag)
    names(Output) <- c("OffsetLoc", "NotAlloc", "Cost", "ExitFlag")
    return(Output)
  }
  else {
    # get output
    Output <- list(vector(), 0, 0, 1)
    names(Output) <- c("OffsetLoc", "NotAlloc", "Cost", "ExitFlag")
    return(Output)
  }
}

RunOffSim <- function(MaxInter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon) {

  # determine where is protected, where impacts can be offset, and where offset sites can be located under the previous or current regulation/policy
  if (Reg == "previous") {
    Prot <- max(stack(PKadaB, KadaBOU)) * PdaInv * KraInv # previous protected areas
    names(Prot) <- "Prot"
    ProtInv <- reclassify(Prot, matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2))
    names(ProtInv) <- "ProtInv"
    ImpOff <- KadaBHMR * ProtInv * PdaInv * KraInv # areas where impacts to be offset
    names(ImpOff) <- "ImpOff"
    OffSite <- KadaHMR # areas for offset sites
    names(OffSite) <- "OffSite"
    OffSiteB <- KadaLR # backup areas for offset sites
    names(OffSiteB) <- "OffSiteB"
  } else if (Reg == "current") {
    Prot <- HabKpa * PdaInv * KraInv # current protected areas
    names(Prot) <- "Prot"
    ProtInv <- reclassify(Prot, matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2))
    names(ProtInv) <- "ProtInv"
    ImpOff <- HabNotKpa * ProtInv *  * KraInv # areas where impacts to be offset
    names(ImpOff) <- "ImpOff"
    OffSite <- RestKpa # areas for offset sites
    names(OffSite) <- "OffSite"
    OffSiteB <- RestNotKpa # backup areas for offset sites
    names(OffSiteB) <- "OffSiteB"
  } else {
    Prot <- HabKpa * PdaInv * KraInv # current protected areas
    names(Prot) <- "Prot"
    Prot[Prot == 1] <- 0
    ProtInv <- reclassify(Prot, matrix(c(0, 1, 1, 0), nrow = 2, ncol = 2))
    names(ProtInv) <- "ProtInv"
    ImpOff <- HabNotKpa * ProtInv * PdaInv * KraInv # areas where impacts to be offset
    names(ImpOff) <- "ImpOff"
    ImpOff[ImpOff == 1] <- 0
    OffSite <- RestKpa # areas for offset sites
    names(OffSite) <- "OffSite"
    OffSite[OffSite == 1] <- 0
    OffSiteB <- RestNotKpa # backup areas for offset sites
    names(OffSiteB) <- "OffSiteB"
    OffSiteB[OffSiteB == 1] <- 0
  }

  # set up record of offsets
  OffsetImpactSites <- lu2016
  OffsetImpactSites[OffsetImpactSites > 0] <- 0
  names(OffsetImpactSites) <- "OffsetImpactSites"
  OffsetSites <- lu2016
  OffsetSites[OffsetSites > 0] <- 0
  names(OffsetSites) <- "OffsetSites"

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
    fileName = paste("input/mlrsummary_2020/model", i, ".rda", sep="")
    load(fileName)
    TMods[[i]] <- temp_mod
    luLabelList[[i]] <- luLabel[i]
  }

  # get clearing models
  load("input/clearing/models/clearing_predictions.Rda")

  # define urban demand based on ShapingSEQ (2017).
  # the matrix represents new dwelling demand for LGAs and consolidation and expansion combinations
  # LGAs and whether existing urban areas or not (LGAExistUrb) 1 = LGA 1 not urban (expansion), 2 = LGA 1 urban (consolidation), ...., 15 = LGA 8 not urban (expansion), 16 = LGA 8 urban (consolidation). LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan] & existing urban areas as defined in Shaping SEQ [1 = existing urban area, 0 = not existing urban area]
  if (Horizon == 2031) {
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(25600,29300,1500,2600,3400,8900,20700,28900,4900,105700,20600,70000,43000,14500,33300,11500)) #2031 demand
  } else if (Horizon == 2041) {
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(40100,48200,1600,4800,4700,12500,33300,53700,11400,176800,31000,127900,83800,27900,70000,19900)) #2041 demand
  } else {
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf)) #2041 demand
  }

  # create raster to hold new spatial dwelling growth
  DwellGrowth <- lu2016
  DwellGrowth[DwellGrowth > 0] <- 0
  names(DwellGrowth) <- "DGrowth"

  # create data frame version of LGA and existing urban layer
  LGAExistUrb.df <- as.data.frame(LGAExistUrb)
  names(LGAExistUrb.df) <- "LGAExistUrb"
  # create data from to hold simulated dwelling growth for each LGA urban/non-urban combination
  DwellGrowthReport <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Growth0" = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  # create vector to hold tests of whether dwelling targets have been met or not
  TargetTest <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  #set up data frame to hold where target constraints are to prevent transitions to 51, 52, and 53
  TargetCons <- cbind(LGAExistUrb.df$LGAExistUrb, as.data.frame(matrix(rep(1,dim(LGAExistUrb.df)[[1]] * 13), nrow = dim(LGAExistUrb.df)[[1]], ncol = 13)))
  dimnames(TargetCons)[[2]] <- c("LGAExistUrb", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # set up static constraints based on the planning scheme and Shaping SEQ for 2017
  # compile planning scheme constraints into data frame for each land use
  plan2017.df <- as.data.frame(plan2017)
  PlanCons <- plan2017.df %>% left_join(plan2017tb, by = "plan2017")
  dimnames(PlanCons)[[2]] <- c("plan2017", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # get transition probability constraints that are assumed not possible based the land use classes in ShapingSEQ
  # these constraints are that transitions to medium and high density residential development are only permitted in the urban footprint
  UFfact.df <- as.data.frame(UFfact)
  UFCons <- UFfact.df %>% left_join(UFfacttb, by = "UFfact")
  dimnames(UFCons)[[2]] <- c("UFfact", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # set up current land use
  lucurr <- lu2016 ##set current land use
  lu2016.df <- as.data.frame(lu2016) # land use in data from format
  lucurr.df <- lu2016.df #set current land use as a data frame
  names(lucurr) <- "lucurr"
  names(lu2016.df) <- "lucurr"
  names(lucurr.df) <- "lucurr"

  # set up habitat data frames
  HabCore.df <- as.data.frame(HabCore)
  HabNonCore.df <- as.data.frame(HabNonCore)
  HabCorePre.df <- as.data.frame(HabCorePre)
  HabNonCorePre.df <- as.data.frame(HabNonCorePre)

  # set up regulation, offset, and land value dataframes
  Prot.df <- as.data.frame(Prot)
  ImpOff.df <- as.data.frame(ImpOff)
  OffSite.df <- as.data.frame(OffSite)
  OffSiteB.df <- as.data.frame(OffSiteB)
  OffsetImpactSites.df <- as.data.frame(OffsetImpactSites)
  OffsetSites.df <- as.data.frame(OffsetSites)
  lgasfact.df <- as.data.frame(lgasfact)
  LandVal.df <- as.data.frame(LandVal)

  # current land use raster
  LandUseList <- list()
  LandUseList[[1]] <- lucurr
  # dwelling growth raster
  DwellGrowthList <- list()
  DwellGrowthList[[1]] <- DwellGrowth
  # offsetable impacts raster
  OffsetImpactSitesList <- list()
  OffsetImpactSitesList[[1]] <- OffsetImpactSites
  # offset sites raster
  OffsetSitesList <- list()
  OffsetSitesList[[1]] <- OffsetSites
  # habitat raters
  HabCoreList <- list()
  HabNonCoreList <- list()
  HabCoreList[[1]] <- HabCore
  HabNonCoreList[[1]] <- HabNonCore
  # koala numbers raster
  KNumList <- list()
  KNumList[[1]] <- KNum * (HabCore + HabNonCore)

  # set offset cost and areas not allocated to zero
  OffCost <- 0
  OffCostList <- list()
  OffCostList[[1]] <- 0
  NotAllocated <- 0
  NotAllocatedList <- list()
  NotAllocatedList[[1]] <- 0

  #loop through iterations to run simulation

  for (i in 1:MaxIter) {

    # constraints on transition to 51, 52, and 53 where dwelling targets already met
    # if some targets met then adjust constraints so we get no further transitions to 51, 52, and 52
    if (length(which(TargetTest == 1)) > 0) {
        for (j in which(TargetTest == 1)) {
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 51)), "51"] <- 0
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 52)), "52"] <- 0
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 53)), "53"] <- 0
       }
    }
    TargetCons[which(is.na(TargetCons$LGAExistUrb)), c("10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")] <- NA

    # get transition probability predictions, incorporating fixed planning scheme and ShapingSEQ constraints if regulation assumed
    if (Reg == "none") {
      Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables] * TargetCons[which(lucurr.df$lucurr == x), Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})
    } else {
      Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables] * PlanCons[which(lucurr.df$lucurr == x), Variables] * UFCons[which(lucurr.df$lucurr == x), Variables] * TargetCons[which(lucurr.df$lucurr == x), Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})
    }

    # set transitions that are not possible to zero
    # assume the following rules
    # 1. Rural residential (40) cannot transition to grazing (21), crops (22), or transition (23)
    # 2. Low density urban (51) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    # 3. Medium density urban (52) cannot transition to low density urban (51), rural residential (40), grazing (21), crops (22), or transition (23)
    # 4. High density urban (53) cannot transition to Medium density urban (52), low density urban (51), rural residential (40), grazing (21), crops (22), or transition (23)
    # 5. Commercial (60) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    # 6. Industrial (72) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    Predictions[[4]] <- set_trans_zero(Predictions[[4]], c("21","22","23"))
    Predictions[[5]] <- set_trans_zero(Predictions[[5]], c("40","21","22","23"))
    Predictions[[6]] <- set_trans_zero(Predictions[[6]], c("51","40","21","22","23"))
    Predictions[[7]] <- set_trans_zero(Predictions[[7]], c("52","51","40","21","22","23"))
    Predictions[[8]] <- set_trans_zero(Predictions[[8]], c("40","21","22","23"))
    Predictions[[10]] <- set_trans_zero(Predictions[[10]], c("40","21","22","23"))

    # simulate transitions
    NewLU.df <- lucurr.df
    NewLUTemp <- unlist(lapply(Predictions, FUN = function(y) {apply(y, 1, FUN = function(x) {if (any(is.na(x))) {NA} else {as.integer(names(x)[which(rmultinom(1, 1, x) == 1)])}})}))
    NewLU.df$lucurr[as.integer(names(NewLUTemp[which(!is.na(NewLUTemp))]))] <- NewLUTemp[which(!is.na(NewLUTemp))]

    # get initial land use change
    LUChange.df <- cbind(lucurr.df,NewLU.df)
    names(LUChange.df) <- c("lucurr","lunew")

    # join habitat to land use change
    LUChangeH.df <- LUChange.df %>% left_join(ClearPred, by = "lucurr")
    dimnames(LUChangeH.df)[[2]] <- c("lucurr", "lunew", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

    # update habitat and get forest loss
    HabLoss.df <- as.data.frame(apply(LUChangeH.df, MARGIN = 1, FUN = function(x) {if (is.na(x["lucurr"])) {return(NA)} else {if (x["lucurr"] == x["lunew"]) {return(0)} else {return(x[as.character(x["lunew"])])}}}))
    names(HabLoss.df) <- "HabLoss"

    # prevent koala tree loss and land use change associated with koala habitat loss where koala habitat is protected if regulation assumed
    # adjust habitat and land use change
    if (Reg != "none") {
      HabLoss.df[which((HabLoss.df > 0) & (Prot.df == 1)), "HabLoss"] <- 0
      LUChange.df[which((HabLoss.df > 0) & (Prot.df == 1)), "lunew"] <- LUChange.df[which((HabLoss.df > 0) & (Prot.df == 1)), "lucurr"]
      NewLU.df$lucurr <- LUChange.df$lunew
    }

    # get the amount of koala habitat lost
    KoalaTreeLost.df <- (HabCore.df + HabNonCore.df) * HabLoss.df
    names(KoalaTreeLost.df) <- "KoalaTreeLost"

    # get new habitat amounts without offsets
    HabCore.df <- HabCore.df * (1 - HabLoss.df)
    names(HabCore.df) <- "HabCore"
    HabNonCore.df <- HabNonCore.df * (1 - HabLoss.df)
    names(HabNonCore.df) <- "HabNonCore"

    if (OffRule != "none") {
    # offsets
      # identify where offsets are required and allocate offset sites for restoration
      # get offset impacts
      KoalaTreeLostOff.df <- KoalaTreeLost.df
      KoalaTreeLostOff.df$KoalaTreeLost[which(ImpOff.df$ImpOff == 0)] <- 0
      # get the opportunities and back-up opportunities for the amount of koala habitat that could be restored in places that are in the permitted restoration areas and are of land uses 10, 21, 22, 23, 30, 40
      RestoreOpp.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
      names(RestoreOpp.df) <- "RestoreOpp"
      RestoreOpp.df[which((!((OffSite.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOpp.df <= 0)), "RestoreOpp"] <- NA
      RestoreOppB.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
      names(RestoreOppB.df) <- "RestoreOppB"
      RestoreOppB.df[which((!((OffSiteB.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppB.df <= 0)), "RestoreOppB"] <- NA

      # Allocate offset sites based on the cheapest to most expensive. First try to allocate to the offset locations and then try to allocate to the backup locations.
      if (OffRule == "lga") {
      # within LGA rule
        # get amount of tree loss in sites that need offsetting in each LGA
        TreeLostOffLGA <- zonal(to_raster(KoalaTreeLostOff.df$KoalaTreeLost, lucurr), lgasfact, fun = 'sum')

        # get allocation in LGA
        Allocation <- apply(matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow = 8, ncol = 1), MARGIN = 1, FUN = function(x){get_off_sites_lga(x, Lgas.df = lgasfact.df$lgasfact, OffArea.df = RestoreOpp.df$RestoreOpp, OffAreaB.df = RestoreOpp.df$RestoreOpp, Costs = LandVal.df$LandVal, Target =  TreeLostOffLGA[x,2] * Multiplier)})

        # increment offset costs
        OffCost <- OffCost + sum(unlist(lapply(Allocation, FUN = function(x){x$Cost})))

        # update offset locations, impact and offset sites, protected areas, and offset area, and core and non core habitat
        OffIndex <- unlist(lapply(Allocation, FUN = function(x){x$OffsetLoc}))
        Success <- rbinom(length(OffIndex), 1, RestSucc) # whether the offset is successful or not
        OffsetImpactSites.df$OffsetImpactSites <- OffsetImpactSites.df$OffsetImpactSites + KoalaTreeLostOff.df$KoalaTreeLost
        OffsetSites.df$OffsetSites[OffIndex] <- OffsetSites.df$OffsetSites[OffIndex] + (((HabCorePre.df$HabCorePre[OffIndex] + HabNonCorePre.df$HabNonCorePre[OffIndex]) - (HabCore.df$HabCore[OffIndex] + HabNonCore.df$HabNonCore[OffIndex])) * Success)
        Prot.df$Prot[OffIndex] <- 1
        OffSite.df$OffSite[OffIndex] <- 0
        OffSiteB.df$OffSiteB[OffIndex] <- 0
        HabCore.df$HabCore[OffIndex] <- (HabCorePre.df$HabCorePre[OffIndex] * Success) + (HabCore.df$HabCore[OffIndex] * ( 1- Success))
        HabNonCore.df$HabNonCore[OffIndex] <- (HabNonCorePre.df$HabNonCorePre[OffIndex] * Success) + (HabNonCore.df$HabNonCore[OffIndex] * ( 1- Success))

        # get the remaining area needed after allocation within LGAs
        Remaining <- sum(unlist(lapply(Allocation, FUN = function(x){x$NotAlloc})))
        ExitF <- any(unlist(lapply(Allocation, FUN = function(x){x$NotAlloc})) == -1)

        # allocate remaining anywhere
        if ((Remaining > 0) & (ExitF == TRUE)) {
          # get the new opportunities and back-up opportunities for the amount of koala habitat that could be restored in places that are in the permitted restoration areas and are of land uses 10, 21, 22, 23, 30, 40
          RestoreOpp.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
          names(RestoreOpp.df) <- "RestoreOpp"
          RestoreOpp.df[which((!((OffSite.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOpp.df <= 0)), "RestoreOpp"] <- NA
          RestoreOppB.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
          names(RestoreOppB.df) <- "RestoreOppB"
          RestoreOppB.df[which((!((OffSiteB.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppB.df <= 0)), "RestoreOppB"] <- NA

          # get allocation anywhere
          Allocation2 <- get_off_sites_anywhere(OffArea.df = RestoreOpp.df$RestoreOpp, OffAreaB.df = RestoreOpp.df$RestoreOpp, Costs = LandVal.df$LandVal, Target =  Remaining)

          # increment offset costs
          OffCost <- OffCost + Allocation2$Cost

          # update offset locations, impact and offset sites, protected areas, and offset area, and core and non core habitat
          OffIndex2 <- Allocation2$OffsetLoc
          Success <- rbinom(length(OffIndex2), 1, RestSucc) # whether the offset is successful or not
          OffsetSites.df$OffsetSites[OffIndex2] <- OffsetSites.df$OffsetSites[OffIndex2] + (((HabCorePre.df$HabCorePre[OffIndex2] + HabNonCorePre.df$HabNonCorePre[OffIndex2]) - (HabCore.df$HabCore[OffIndex2] + HabNonCore.df$HabNonCore[OffIndex2])) * Success)
          Prot.df$Prot[OffIndex2] <- 1
          OffSite.df$OffSite[OffIndex2] <- 0
          OffSiteB.df$OffSiteB[OffIndex2] <- 0
          HabCore.df$HabCore[OffIndex2] <- (HabCorePre.df$HabCorePre[OffIndex2] * Success) + (HabCore.df$HabCore[OffIndex2] * ( 1- Success))
          HabNonCore.df$HabNonCore[OffIndex2] <- (HabNonCorePre.df$HabNonCorePre[OffIndex2] * Success) + (HabNonCore.df$HabNonCore[OffIndex2] * ( 1 - Success))

          # get the remaining area needed after allocation within LGAs
          Remaining <- Allocation2$NotAlloc

          # combine allocations and offset indices
          Allocation <- c(Allocation, list(Allocation2))
          OffIndex <- c(OffIndex, OffIndex2)
        }

        # update the area of offsets not allocated
        NotAllocated <- NotAllocated + Remaining
      } else if (OffRule == "anywhere") {
      # anywhere rule
        # get allocation anywhere
        Allocation <- get_off_sites_anywhere(OffArea.df = RestoreOpp.df$RestoreOpp, OffAreaB.df = RestoreOpp.df$RestoreOpp, Costs = LandVal.df$LandVal, Target =  sum(KoalaTreeLostOff.df, na.rm = TRUE) * Multiplier)

        # increment offset costs
        OffCost <- OffCost + Allocation$Cost

        # update offset locations, impact and offset sites, protected areas, and offset area, and core and non core habitat
        OffIndex <- Allocation$OffsetLoc
        Success <- rbinom(length(OffIndex), 1, RestSucc) # whether the offset is successful or not
        OffsetImpactSites.df$OffsetImpactSites <- OffsetImpactSites.df$OffsetImpactSites + KoalaTreeLostOff.df$KoalaTreeLost
        OffsetSites.df$OffsetSites[OffIndex] <- OffsetSites.df$OffsetSites[OffIndex] + (((HabCorePre.df$HabCorePre[OffIndex] + HabNonCorePre.df$HabNonCorePre[OffIndex]) - (HabCore.df$HabCore[OffIndex] + HabNonCore.df$HabNonCore[OffIndex])) * Success)
        Prot.df$Prot[OffIndex] <- 1
        OffSite.df$OffSite[OffIndex] <- 0
        OffSiteB.df$OffSiteB[OffIndex] <- 0
        HabCore.df$HabCore[OffIndex] <- (HabCorePre.df$HabCorePre[OffIndex] * Success) + (HabCore.df$HabCore[OffIndex] * ( 1- Success))
        HabNonCore.df$HabNonCore[OffIndex] <- (HabNonCorePre.df$HabNonCorePre[OffIndex] * Success) + (HabNonCore.df$HabNonCore[OffIndex] * ( 1- Success))

        # get the remaining area needed after allocation
        Remaining <-  Allocation$NotAlloc

        # update the area of offsets not allocated
        NotAllocated <- NotAllocated + Remaining
      }
    }

    # record offsets in list
    OffsetImpactSites <- to_raster(OffsetImpactSites.df$OffsetImpactSites, lucurr)
    names(OffsetImpactSites) <- "OffsetImpactSites"
    OffsetSites <- to_raster(OffsetSites.df$OffsetSites, lucurr)
    names(OffsetSites) <- "OffsetSites"
    if (i %% RepSteps == 0) {
      OffsetImpactSitesList[[floor(i / RepSteps) + 1]] <- OffsetImpactSites
      OffsetSitesList[[floor(i / RepSteps) + 1]] <- OffsetSites
      OffCostList[[floor(i / RepSteps) + 1]] <- OffCost
      NotAllocatedList[[floor(i / RepSteps) + 1]] <- NotAllocated
    }

    # record habitat in list
    HabCoreNew <- to_raster(HabCore.df$HabCore, lucurr)
    names(HabCoreNew) <- "HabCore"
    HabNonCoreNew <- to_raster(HabNonCore.df$HabNonCore, lucurr)
    names(HabNonCoreNew) <- "HabNonCore"
    KNumNew <- KNum * (HabCoreNew + HabNonCoreNew)
    names(KNumNew) <- "KNum"
    if (i %% RepSteps == 0) {
      HabCoreList[[floor(i / RepSteps) + 1]] <- HabCoreNew
      HabNonCoreList[[floor(i / RepSteps) + 1]] <- HabNonCoreNew
      KNumList[[floor(i / RepSteps) + 1]] <- KNumNew
    }

    # update dwellings
    # join dwellings to land use change
    LUChangeD.df <- LUChange.df %>% left_join(DwelDentb, by = "lucurr")
    dimnames(LUChangeD.df)[[2]] <- c("lucurr", "lunew", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")
    DwellGrowth.df <- as.data.frame(DwellGrowth)
    NewDwellGrowth.df <- as.data.frame(apply(LUChangeD.df, MARGIN = 1, FUN = function(x) {if (is.na(x["lucurr"])) {return(NA)} else {if (x["lucurr"] == x["lunew"]) {return(0)} else {return(x[as.character(x["lunew"])])}}}))
    names(NewDwellGrowth.df) <- "DGrowth"
    DwellGrowth.df$DGrowth <- DwellGrowth.df$DGrowth + NewDwellGrowth.df$DGrowth
    # create raster of dwelling growth
    DwellGrowth <- to_raster(DwellGrowth.df$DGrowth, lucurr)
    names(DwellGrowth) <- "DGrowth"
    # record dwelling growth in list
    if (i %% RepSteps == 0) {
      DwellGrowthList[[floor(i / RepSteps) + 1]] <- DwellGrowth
    }

    # update test for meeting dwelling growth targets and adjust transition probabilities where necessary
    # if met dwelling growth targets then set transitions to 51, 52, and 53 to zero
    ZonalDwellGrowth <- as.data.frame(zonal(DwellGrowth, LGAExistUrb, fun = "sum", na.rm = TRUE))
    TargetTest <- ifelse(ZonalDwellGrowth$sum >= UrbanDemand$Demand, 1, 0)
    Label <- paste("Growth", i, sep = "")
    DwellGrowthReport <- cbind(DwellGrowthReport, ZonalDwellGrowth[,2])
    names(DwellGrowthReport)[length(names(DwellGrowthReport))] <- Label

    # assign new land use to current land use data frame
    lucurr.df <- NewLU.df
    names(lucurr.df) <- "lucurr"
    # record new land use in list
    lucurr <- to_raster(lucurr.df$lucurr, lucurr)
    if (i %% RepSteps == 0) {
      LandUseList[[floor(i / RepSteps) + 1]] <- lucurr
    }

    # update urban residential neighbourhood
    UrbLand <- lucurr
    UrbLand[lucurr == 51 | lucurr == 52 | lucurr == 53] <- 1
    UrbLand[lucurr != 51 & lucurr != 52 & lucurr != 53 & !is.na(lucurr)] <- 0
    FocalUrb <- (as.data.frame(focal(UrbLand, matrix(1, nrow = 5, ncol = 5), fun = mean, na.rm = T)) - attributes(Predictors$NeighUrb)$`scaled:center`) / attributes(Predictors$NeighUrb)$`scaled:scale`
    names(FocalUrb) <- "NeighUrb"
    FocalUrb$NeighUrb[which(is.na(lucurr.df$lucurr))] <- NA
    FocalUrb <- as.matrix(FocalUrb)
    attributes(FocalUrb) <- attributes(Predictors$NeighUrb)
    Predictors$NeighUrb <- FocalUrb

    # update commercial and industrial neighbourhood
    IndLand <- lucurr
    IndLand[lucurr == 60 | lucurr == 72] <- 1
    IndLand[lucurr != 60 & lucurr != 72 & !is.na(lucurr)] <- 0
    FocalInd <- (as.data.frame(focal(IndLand, matrix(1, nrow = 5, ncol = 5), fun = mean, na.rm = T)) - attributes(Predictors$NeighInd)$`scaled:center`) / attributes(Predictors$NeighInd)$`scaled:scale`
    names(FocalInd) <- "NeighInd"
    FocalInd$NeighInd[which(is.na(lucurr.df$lucurr))] <- NA
    FocalInd <- as.matrix(FocalInd)
    attributes(FocalInd) <- attributes(Predictors$NeighInd)
    Predictors$NeighInd <- FocalInd

    # check if total dwelling growth has reached target
    if (sum(ZonalDwellGrowth[,2]) >= sum(UrbanDemand$Demand)) {
        if (i %% RepSteps != 0) {
          OffsetImpactSitesList[[floor(i / RepSteps) + 2]] <- OffsetImpactSites
          OffsetSitesList[[floor(i / RepSteps) + 2]] <- OffsetSites
          OffCostList[[floor(i / RepSteps) + 2]] <- OffCost
          NotAllocatedList[[floor(i / RepSteps) + 2]] <- NotAllocated
          HabCoreList[[floor(i / RepSteps) + 2]] <- HabCoreNew
          HabNonCoreList[[floor(i / RepSteps) + 2]] <- HabNonCoreNew
          KNumList[[floor(i / RepSteps) + 1]] <- KNumNew
          DwellGrowthList[[floor(i / RepSteps) + 2]] <- DwellGrowth
          LandUseList[[floor(i / RepSteps) + 2]] <- lucurr
        }
        break
    }
  }

  Output <- list(LandUseList, DwellGrowthList, HabCoreList, HabNonCoreList, KNumList, OffsetImpactSitesList, OffsetSitesList, OffCostList, NotAllocatedList)
  names(Output) <- c("LandUse", "DwellNum", "HabCore", "HabNonCore", "KNum", "OffsetImpactSites", "OffsetSites", "OffCost", "NotAllocated")

  return(Output)
}

# RUN SCENARIOS

# FIRST GET DATA

## Load maps, data, and lookup tables ####
lu1999 <- raster("input/maps/landuse99reclsuburb4.asc") # land use in 1999
lu2016 <- raster("input/maps/landuse16reclsuburb4.asc") # land use 2016
plan2010 <- raster("input/maps/seq_planning_scheme_2010.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$planning_scheme_2010 for description - planning schemes for 2010
plan2017 <- raster("input/maps/seq_planning_scheme_2017b.asc") #04 Model\CA-KoalaOffset\output\table\Land_reclassification.xlsx$seq_planningScheme2017b for description - planning schemes for 2017
plan2017tb <- read.csv("input/table/planningScheme2017.csv", header = TRUE) # planning scheme constraints table for 2017
slope <- raster("input/maps/seq_slope.asc") # slope
elev <- raster("input/maps/seq_dem.asc") # elevation
road <- raster("input/maps/SEQ_distRoad.asc") # distance to main roads
city <- raster("input/maps/seq_cityDist.asc") # distance to cities
roadDen <- raster("input/maps/SEQ_distRoad.asc") # distance to all roads
awc <- raster("input/maps/seq_awcMeans.asc") # soil mean available water content
awc[is.na(lu1999)] <- NA # remove zeros outside the study area
cly <- raster("input/maps/seq_clymeans1.asc") # soil clay content
NeighUrb <- raster("input/maps/lu99rdevn1.asc") # proportion of residential dev (51,52,53) in a 5 x 5 moving window (ignoring NAs)
NeighInd <- raster("input/maps/lu99indn1.asc") # proportion of commercial and industrial (60,72) in a 5 x 5 moving window (ignoring NAs)
UFfact <- raster("input/maps/seq_urbanfootprint2017.asc") # [0:regional 40:rural 50:urban] Shaping SEQ
UFfacttb <- read.csv("input/table/shapingseq_cons.csv", header = TRUE) # Shaping SEQ land uses
lgasfact <- raster("input/maps/lgas.asc") # LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan]
DwelDentb <- read.csv("input/table/change_dwellings.csv", header = TRUE) # changes in dwelling density / ha for each transitions (based on average dwelling densities in 2016 from the 2016 meshblocks)
LGAExistUrb <- raster("input/maps/lgaexisturb.asc") # LGAs and whether existing urban areas or not 1 = LGA 1 not urban, 2 = LGA 1 urban, ...., 15 = LGA 8 not urban, 16 = LGA 8 urban. LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan] & existing urban areas as defined in Shaping SEQ [1 = existing urban area, 0 = not existing urban area]
HabCore <- raster("input/maps/corehab.asc") # area of core habitat in each gird cell
HabNonCore <- raster("input/maps/noncorehab.asc") #area of non-core habitat in each gird cell
HabCorePre <- raster("input/maps/corehabpre.asc") # area of core habitat per clearing in each gird cell
HabNonCorePre <- raster("input/maps/noncorehabpre.asc") # area of non-core habitat pre-clearing in each gird cell
KNum <- raster("input/maps/denmod6f.asc") # koala density from Model 6 (Rhodes et al. 2015)
KadaBOU <- raster("input/maps/kadabnurbf.asc") # koala habitat protected - bushland habitat in KADA area outside of the urban footprint and outside of urban use (urban use defined as classes 4, 36, 18, 10, 20, 23, 26, 27, 12, 2, 3, 14, 19, 1, 29, 13, 17, 22, 39, 6 in the 2017 planning scheme)
PKadaB <- raster("input/maps/pkadabf.asc") # koala habitat protected - bushland habitat in PKADA
KadaBHMR <- raster("input/maps/kadabhmrf.asc") # koala habitat impacts to be offset (if not already protected)- all koala bushland habitat and high and medium values rehab
KadaHMR <- raster("input/maps/kadahmrf.asc") # areas suitable for offsets - all high and medium values rehab
KadaLR <- raster("input/maps/kadalrf.asc") # back-up areas suitable for offsets - all low value rehab
HabKpa <- raster("input/maps/habkpaf.asc") # protected habitat in koala priority areas
HabNotKpa <- raster("input/maps/habnkpaf.asc") # habitat outside of koala priority areas
RestKpa <- raster("input/maps/restkpaf.asc") # restoration areas in koala priority areas
RestNotKpa <- raster("input/maps/restnkpaf.asc") # restoration areas outside koala priority areas
Pda <- raster("input/maps/pda_fin.asc") # koala habitat not protected nor offsets required - Priority Development Area
PdaInv <- reclassify(Pda, matrix(c(0,1,1,0),nrow=2,ncol=2))
Kra <- raster("input/maps/krafin.asc") # koala habitat not protected nor offsets required - Key Resource Area
KraInv <- reclassify(Kra, matrix(c(0,1,1,0),nrow=2,ncol=2))
LandVal <- (raster("input/maps/lvalsimf.asc") + 1) # unimproved land value from 2012 (QVAS data) - here add $1 to avoid zeros. Here have estimated missing or zero values using an inverse distance weighted interpolation (exponential with exponent 2 which had the lowest RMS) and assumed national parks and state land had land vlaues of zero to relfect cost to the state government.

# apply names
names(lu1999) <- "lu1999"
names(lu2016) <- "lu2016"
names(plan2010) <- "plan2010"
names(plan2017) <- "plan2017"
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
names(LGAExistUrb) <- "LGAExistUrb"
names(HabCore) <- "HabCore"
names(HabNonCore) <- "HabNonCore"
names(HabCorePre) <- "HabCorePre"
names(HabNonCorePre) <- "HabNonCorePre"
names(KNum) <- "KNum"
names(KadaBOU) <- "KadaBOU"
names(PKadaB) <- "PKadaB"
names(KadaBHMR) <- "KadaBHMR"
names(KadaHMR) <- "KadaHMR"
names(KadaLR) <- "KadaLR"
names(HabKpa) <- "HabKpa"
names(HabNotKpa) <- "HabNotKpa"
names(RestKpa) <- "RestKpa"
names(RestNotKpa) <- "restNotKpa"
names(Pda) <- "Pda"
names(PdaInv) <- "PdaInv"
names(Kra) <- "Kra"
names(KraInv) <- "KraInv"
names(LandVal) <- "LandVal"

## SET UP AND RUN SCENARIOS

# set up parallel processing
cl <- makeCluster(4)  # initiate parallel computing
registerDoParallel(cl)  # use multicore, set to the number of our cores

# SCENARIO 1 - no regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "none" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 2 - previous regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 3 - previous regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 4 - previous regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 6 - current regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# SCENARIO 7 - current regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf

foreach (i = 1:20, .packages = c("tidyverse", "raster", "nnet")) %dopar% {
  Test <- RunOffSim(MaxIter, RepSteps, Reg, OffRule, Multiplier, RestSucc, Horizon)
  saveRDS(Test, paste("E:/analysis/offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  rm(Test)
  gc()
  NA
}

# close cores
stopImplicitCluster()
