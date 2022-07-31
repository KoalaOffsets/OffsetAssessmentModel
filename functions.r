# FUNCTIONS

# function to calculate mode
Mode <- function(x) {
    ux <- unique(x)
    ux=ux[!is.na(ux)]
    ux[which.max(tabulate(match(x, ux)))]
}

# function to arrange shared legend for multi-plots
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

  }

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

# function to create a cross tabulation based on two raster maps with categorical values (does not need to have the same number of classes)
# columns are observed, rows are predicted
to_crosstab <- function (obs, pred, rast_template) {
  ctable <- crosstabm(to_raster(pred, rast_template), to_raster(obs, rast_template))
  return (ctable)
}

# function to allocate offset sites for proponent driven offsets
alloc_offsets_proponent <- function(NeedAvail, SortedOffSites, KpaOrLga) {

  # set NAs to zero in NeedAvail
  if(is.na(NeedAvail[2])) {NeedAvail[2] <- 0}
  if(is.na(NeedAvail[3])) {NeedAvail[3] <- 0}

  if (NeedAvail[3] > 0) {
  # offsets are needed in kpa or lga
    if (NeedAvail[2] - NeedAvail[3] >= 0)  {
    # enough to allocate - allocate up to amount needed

      # get the cummulative sum of offsets site areas
      SortAreasSum <- cumsum(SortedOffSites$RestArea[which(SortedOffSites[KpaOrLga] == NeedAvail[1])])

      # get the index with the closest value to but larger than the target offset area required
      Diff <- SortAreasSum - NeedAvail[3]
      DiffTarg <- which(Diff >= 0)
      MinIndex <- DiffTarg[1]

      # allocate offset sites
      Allocated <- which(SortedOffSites[KpaOrLga] == NeedAvail[1])[1:MinIndex]
      Areas <- SortedOffSites$RestArea[Allocated]
      Left <- as.numeric(NeedAvail[3]) - sum(Areas, na.rm = T)
      # adjust areas restored for any over investment in restoration
      if (Left < 0) {
        Areas[length(Areas)] <- Areas[length(Areas)] * (1 + (Left / Areas[length(Areas)]))
      }
      Cost <- sum(SortedOffSites$Cost[Allocated] * Areas, na.rm = TRUE)
      Left <- as.numeric(NeedAvail[3]) - sum(Areas, na.rm = T)
    } else {
    # not enough to allocate - allocate as many offsets as possible
      # allocate offset sites
      if (any(!is.na(SortedOffSites$RestArea[which(SortedOffSites[KpaOrLga] == NeedAvail[1])]))) {
      # all offset sites needed
        Allocated <- which((SortedOffSites[KpaOrLga] == NeedAvail[1]) & !is.na(SortedOffSites$RestArea))
        Areas <- SortedOffSites$RestArea[Allocated]
        Cost <- sum(SortedOffSites$Cost[Allocated] * Areas, na.rm = TRUE)
        Left <- as.numeric(NeedAvail[3]) - sum(Areas, na.rm = T)
      } else {
      # no offset sites to allocate
        Allocated <- NULL
        Areas <- NULL
        Cost <- 0
        Left <- as.numeric(NeedAvail[3])
      }
    }
  }
  else {
  # offsets not needed in KPA or LGA
    Allocated <- NULL
    Areas <- NULL
    Cost <- 0
    Left <- 0
  }

  return(list(AllocIDs = Allocated, AllocAreas = Areas, Cost = Cost, AreaLeft = Left))
}

# function to offset sites for proponent driven offsets - returns the indices of allocated sites, the cost of land purchase, and any remaining impacts not offset yet
get_off_sites_proponent <- function(ImpactSites, OffsetSitesA, OffsetSitesB, OffsetSitesC, Kpa, Lga, IncentiveCost, OnGroundCost, AdminCost, Multiplier) {

  # set up compiled output variables
  OffIDsCompiled <- NULL
  RestAreasCompiled <- NULL
  CostCompiled <- NULL

  # record initial impact sites
  ImpactSitesInit <- ImpactSites

  # order potential offset sites in order of cheapest to most expensive based on cost

  # consider costs only in those areas where an offset is possible for closest KPA
  CostA <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostA) <- "Cost"
  CostA[which(is.na(OffsetSitesA$RestoreOppP)), "Cost"] <- NA

  # get order of offset site priority based on cost minimisation for closest KPA
  OrderA <- order(CostA, OffsetSitesA$RestoreOppP, decreasing = FALSE, na.last = TRUE)

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for closest KPA
  SortDataA <- as.data.frame(OffsetSitesA$RestoreOppP[OrderA])
  names(SortDataA) <- "RestArea"
  SortDataA$Cost <- CostA[OrderA, "Cost"]
  SortDataA$Kpa <- Kpa[OrderA, "Kpa"]

  # identify how much restoration needs to be done in each closest KPA
  # identify how much restoration is possible in each KPA
  # combine into a single dataframe
  NeedAvailCloKpa <- bind_cols(SortDataA %>% filter(!is.na(Kpa)) %>% group_by(Kpa) %>% summarise(AreaAvail = sum(RestArea, na.rm = TRUE)),
                ImpactSites %>% filter(!is.na(CloKpa)) %>% group_by(CloKpa) %>% summarise(AreaNeed = sum(KoalaTreeLost, na.rm = TRUE)) %>% mutate(AreaNeed = AreaNeed * Multiplier) %>% dplyr::select(AreaNeed))
  # allocate offsets to closest KPA
  if (sum(NeedAvailCloKpa$AreaNeed, na.rm = TRUE) > 0) {
  # offsets needed
    OffCloKpa <- apply(NeedAvailCloKpa, MARGIN = 1, FUN = alloc_offsets_proponent, SortedOffSites = SortDataA, KpaOrLga = "Kpa")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffCloKpa)) {
        # update impact sites
      if (round(OffCloKpa[[i]]$AreaLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] * (OffCloKpa[[i]]$AreaLeft / NeedAvailCloKpa$AreaNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffCloKpa[[i]]$AllocIDs)) {
        OffsetSitesA$RestoreOppP[OrderA[OffCloKpa[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffCloKpa[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderA[OffCloKpa[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderA[OffCloKpa[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffCloKpa[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffCloKpa[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffCloKpa[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffCloKpa[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffCloKpa[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffCloKpa[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of cheapest to most expensive based on cost

  # consider costs only in those areas where an offset is possible for second closest KPA
  CostA <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostA) <- "Cost"
  CostA[which(is.na(OffsetSitesA$RestoreOppP)), "Cost"] <- NA

  # get order of offset site priority based on cost minimisation for second closest KPA
  OrderA <- order(CostA, OffsetSitesA$RestoreOppP, decreasing = FALSE, na.last = TRUE)

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for second closest KPA
  SortDataA <- as.data.frame(OffsetSitesA$RestoreOppP[OrderA])
  names(SortDataA) <- "RestArea"
  SortDataA$Cost <- CostA[OrderA, "Cost"]
  SortDataA$Kpa <- Kpa[OrderA, "Kpa"]

  # identify how much restoration needs to be done in each second closest KPA
  # identify how much restoration is possible in each KPA
  # combine into a single dataframe
  NeedAvailSecCloKpa <- bind_cols(SortDataA %>% filter(!is.na(Kpa)) %>% group_by(Kpa) %>% summarise(AreaAvail = sum(RestArea, na.rm = TRUE)),
                ImpactSites %>% filter(!is.na(SecCloKpa)) %>% group_by(SecCloKpa) %>% summarise(AreaNeed = sum(KoalaTreeLost, na.rm = TRUE)) %>% mutate(AreaNeed = AreaNeed * Multiplier) %>% dplyr::select(AreaNeed))

  # allocate offsets to second closest KPA
  if (sum(NeedAvailSecCloKpa$AreaNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffSecCloKpa <- apply(NeedAvailSecCloKpa, MARGIN = 1, FUN = alloc_offsets_proponent, SortedOffSites = SortDataA, KpaOrLga = "Kpa")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffSecCloKpa)) {
      # update impact sites
      if (round(OffSecCloKpa[[i]]$AreaLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] * (OffSecCloKpa[[i]]$AreaLeft / NeedAvailSecCloKpa$AreaNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffSecCloKpa[[i]]$AllocIDs)) {
        OffsetSitesA$RestoreOppP[OrderA[OffSecCloKpa[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffSecCloKpa[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderA[OffSecCloKpa[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderA[OffSecCloKpa[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffSecCloKpa[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffSecCloKpa[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffSecCloKpa[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffSecCloKpa[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffSecCloKpa[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffSecCloKpa[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of cheapest to most expensive based on cost

  # consider costs only in those areas where an offset is possible in restoration areas in each LGA
  CostB <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostB) <- "Cost"
  CostB[which(is.na(OffsetSitesB$RestoreOppPB)), "Cost"] <- NA

  # get order of offset site priority based on cost minimisation for restoration areas in each LGA
  OrderB <- order(CostB, OffsetSitesB$RestoreOppPB, decreasing = FALSE, na.last = TRUE)

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for restoration areas in each LGA
  SortDataB <- as.data.frame(OffsetSitesB$RestoreOppPB[OrderB])
  names(SortDataB) <- "RestArea"
  SortDataB$Cost <- CostB[OrderB, "Cost"]
  SortDataB$Lga <- Lga[OrderB, "lgasfact"]

  # identify how much restoration is possible in restoration areas in each LGA
  # combine into a single dataframe
  NeedAvailRestLga <- bind_cols(SortDataB %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(AreaAvail = sum(RestArea, na.rm = TRUE)),
                ImpactSites %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(AreaNeed = sum(KoalaTreeLost, na.rm = TRUE)) %>% mutate(AreaNeed = AreaNeed * Multiplier) %>% dplyr::select(AreaNeed))

  # allocate offsets to restoration areas in LGAs
  if (sum(NeedAvailRestLga$AreaNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffRestLga <- apply(NeedAvailRestLga, MARGIN = 1, FUN = alloc_offsets_proponent, SortedOffSites = SortDataB, KpaOrLga = "Lga")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffRestLga)) {
      # update impact sites
      if (round(OffRestLga[[i]]$AreaLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] * (OffRestLga[[i]]$AreaLeft / NeedAvailRestLga$AreaNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffRestLga[[i]]$AllocIDs)) {
        OffsetSitesB$RestoreOppPB[OrderB[OffRestLga[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffRestLga[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderB[OffRestLga[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderB[OffRestLga[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffRestLga[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffRestLga[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffRestLga[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffRestLga[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffRestLga[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffRestLga[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of cheapest to most expensive based on cost

  # consider costs only in those areas where an offset is possible for non-restoration areas in each LGA
  CostC <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostC) <- "Cost"
  CostC[which(is.na(OffsetSitesC$RestoreOppPC)), "Cost"] <- NA

  # get order of offset site priority based on cost minimisation for non-restoration areas in each LGA
  OrderC <- order(CostC, OffsetSitesC$RestoreOppPC, decreasing = FALSE, na.last = TRUE)

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for non-restoration areas in each LGA
  SortDataC <- as.data.frame(OffsetSitesC$RestoreOppPC[OrderC])
  names(SortDataC) <- "RestArea"
  SortDataC$Cost <- CostC[OrderC, "Cost"]
  SortDataC$Lga <- Lga[OrderC, "lgasfact"]

  # identify how much restoration is possible in restoration areas in each LGA
  # combine into a single dataframe
  NeedAvailNonRestLga <- bind_cols(SortDataC %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(AreaAvail = sum(RestArea, na.rm = TRUE)),
                ImpactSites %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(AreaNeed = sum(KoalaTreeLost, na.rm = TRUE)) %>% mutate(AreaNeed = AreaNeed * Multiplier) %>% dplyr::select(AreaNeed))

  # allocate offsets to restoration areas in LGAs
  if (sum(NeedAvailNonRestLga$AreaNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffNonRestLga <- apply(NeedAvailNonRestLga, MARGIN = 1, FUN = alloc_offsets_proponent, SortedOffSites = SortDataC, KpaOrLga = "Lga")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffNonRestLga)) {
      # update impact sites
      if (round(OffNonRestLga[[i]]$AreaLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] * (OffNonRestLga[[i]]$AreaLeft / NeedAvailNonRestLga$AreaNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffNonRestLga[[i]]$AllocIDs)) {
        OffsetSitesC$RestoreOppPC[OrderC[OffNonRestLga[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffNonRestLga[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderC[OffNonRestLga[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderC[OffNonRestLga[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffNonRestLga[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffNonRestLga[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffNonRestLga[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffNonRestLga[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffNonRestLga[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffNonRestLga[[i]]$Cost
        }
      }
    }
  }

  # set NAs in and ImpactSitesInit$KoalaTreeLost and ImpactSites$KoalaTreeLost to zero
  ImpactSitesInit$KoalaTreeLost[which(is.na(ImpactSitesInit$KoalaTreeLost))] <- 0
  ImpactSites$KoalaTreeLost[which(is.na(ImpactSites$KoalaTreeLost))] <- 0

  return(list(Locations = OffIDsCompiled, Areas = RestAreasCompiled, Cost = CostCompiled, Impacts = ImpactSitesInit$KoalaTreeLost, ImpactsNotAlloc = ImpactSites$KoalaTreeLost))
}

# function to allocate offset sites for financial offsets
alloc_offsets_financial <- function(NeedAvail, SortedOffSites, KpaOrLga) {

  # set NAs to zero in NeedAvail
  if(is.na(NeedAvail[2])) {NeedAvail[2] <- 0}
  if(is.na(NeedAvail[3])) {NeedAvail[3] <- 0}

  # get total offset costs per site
  SortedOffSites$TotalCosts <- SortedOffSites$RestArea * SortedOffSites$Cost

  if (NeedAvail[3] > 0) {
  # offsets are needed in kpa or lga
    if (NeedAvail[2] - NeedAvail[3] >= 0)  {
    # enough to allocate - allocate up to amount needed

      # get the cummulative sum of offsets site costs
      SortCostSum <- cumsum(SortedOffSites$TotalCosts[which(SortedOffSites[KpaOrLga] == NeedAvail[1])])

      # get the index with the closest value to but larger than the target offset cost required
      Diff <- SortCostSum - NeedAvail[3]
      DiffTarg <- which(Diff >= 0)
      MinIndex <- DiffTarg[1]

      # allocate offset sites
      Allocated <- which(SortedOffSites[KpaOrLga] == NeedAvail[1])[1:MinIndex]
      Areas <- SortedOffSites$RestArea[Allocated]
      Costs <- SortedOffSites$TotalCost[Allocated]
      Left <- as.numeric(NeedAvail[3]) - sum(Costs, na.rm = T)
      # adjust areas restored for any over investment in restoration
      if (Left < 0) {
        Areas[length(Areas)] <- Areas[length(Areas)] * (1 + (Left / Costs[length(Costs)]))
        Costs[length(Costs)] <- Costs[length(Costs)] * (1 + (Left / Costs[length(Costs)]))
      }
      Cost <- sum(Costs, na.rm = TRUE)
      Left <- as.numeric(NeedAvail[3]) - Cost
    } else {
    # not enough to allocate - allocate as many offsets as possible
      # allocate offset sites
      if (any(!is.na(SortedOffSites$RestArea[which(SortedOffSites[KpaOrLga] == NeedAvail[1])]))) {
      # all offset sites needed
        Allocated <- which((SortedOffSites[KpaOrLga] == NeedAvail[1]) & !is.na(SortedOffSites$RestArea))
        Areas <- SortedOffSites$RestArea[Allocated]
        Costs <- SortedOffSites$TotalCosts[Allocated]
        Cost <- sum(Costs, na.rm = TRUE)
        Left <- as.numeric(NeedAvail[3]) - Cost
      } else {
      # no offset sites to allocate
        Allocated <- NULL
        Areas <- NULL
        Cost <- 0
        Left <- as.numeric(NeedAvail[3])
      }
    }
  }
  else {
  # offsets not needed in KPA or LGA
    Allocated <- NULL
    Areas <- NULL
    Cost <- 0
    Left <- 0
  }

  return(list(AllocIDs = Allocated, AllocAreas = Areas, Cost = Cost, ExpendLeft = Left))
}

# function to offset sites for financial driven offsets - returns the indices of allocated sites, the cost of land purchase, and any remaining impacts not offset yet
get_off_sites_financial <- function(ImpactSites, OffsetSitesA, OffsetSitesB, OffsetSitesC, KoalaDen, Kpa, Lga, IncentiveCost, OnGroundCost, AdminCost, Multiplier, OffType) {

  # set up compiled output variables
  OffIDsCompiled <- NULL
  RestAreasCompiled <- NULL
  CostCompiled <- NULL

  # record initial impact sites
  ImpactSitesInit <- ImpactSites

  # order potential offset sites in order of most cost efficient increase in koala numbers per unit area restored based on incentive payment cost

  # consider costs only in those areas where an offset is possible for closest KPA
  CostA <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostA) <- "Cost"
  CostA[which(is.na(OffsetSitesA$RestoreOppF)), "Cost"] <- NA

  # get order of offset site priority
  if (OffType == "financialhab") {
  # habitat area cost efficiency
    OrderA <- order(CostA, OffsetSitesA$RestoreOppF, decreasing = FALSE, na.last = TRUE)
  }
  else if (OffType == "financialkoala") {
  # koala numbers cost efficiency
    OrderA <- order(CostA / KoalaDen, OffsetSitesA$RestoreOppF, decreasing = FALSE, na.last = TRUE)
  }

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for closest KPA
  SortDataA <- as.data.frame(OffsetSitesA$RestoreOppF[OrderA])
  names(SortDataA) <- "RestArea"
  SortDataA$Cost <- CostA[OrderA, "Cost"]
  SortDataA$Kpa <- Kpa[OrderA, "Kpa"]

  # identify how much restoration needs to be done in each closest KPA
  # identify how much restoration is possible in each KPA
  # combine into a single dataframe
  NeedAvailCloKpa <- bind_cols(SortDataA %>% mutate(TotCost = RestArea * Cost) %>% filter(!is.na(Kpa)) %>% group_by(Kpa) %>% summarise(ExpendAvail = sum(TotCost, na.rm = TRUE)),
                ImpactSites %>% mutate(TotCost = KoalaTreeLost * FCost) %>% filter(!is.na(CloKpa)) %>% group_by(CloKpa) %>% summarise(ExpendNeed = sum(TotCost, na.rm = TRUE)) %>% dplyr::select(ExpendNeed))

  # allocate offsets to closest KPA
  if (sum(NeedAvailCloKpa$ExpendNeed, na.rm = TRUE) > 0) {
  # offsets needed
    OffCloKpa <- apply(NeedAvailCloKpa, MARGIN = 1, FUN = alloc_offsets_financial, SortedOffSites = SortDataA, KpaOrLga = "Kpa")
    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffCloKpa)) {
      # update impact sites
      if (round(OffCloKpa[[i]]$ExpendLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$CloKpa == NeedAvailCloKpa$Kpa[i])] * (OffCloKpa[[i]]$ExpendLeft / NeedAvailCloKpa$ExpendNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffCloKpa[[i]]$AllocIDs)) {
        OffsetSitesA$RestoreOppF[OrderA[OffCloKpa[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffCloKpa[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderA[OffCloKpa[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderA[OffCloKpa[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffCloKpa[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffCloKpa[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffCloKpa[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffCloKpa[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffCloKpa[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffCloKpa[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of most cost efficient increase in koala numbers per unit area restored based on cost

  # consider costs only in those areas where an offset is possible for second closest KPA
  CostA <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostA) <- "Cost"
  CostA[which(is.na(OffsetSitesA$RestoreOppF)), "Cost"] <- NA

  # get order of offset site priority
  if (OffType == "financialhab") {
  # habitat area cost efficiency
    OrderA <- order(CostA, OffsetSitesA$RestoreOppF, decreasing = FALSE, na.last = TRUE)
  }
  else if (OffType == "financialkoala") {
  # koala numbers cost efficiency
    OrderA <- order(CostA / KoalaDen, OffsetSitesA$RestoreOppF, decreasing = FALSE, na.last = TRUE)
  }

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for second closest KPA
  SortDataA <- as.data.frame(OffsetSitesA$RestoreOppF[OrderA])
  names(SortDataA) <- "RestArea"
  SortDataA$Cost <- CostA[OrderA, "Cost"]
  SortDataA$Kpa <- Kpa[OrderA, "Kpa"]

  # identify how much restoration needs to be done in each second closest KPA
  # identify how much restoration is possible in each KPA
  # combine into a single dataframe
  NeedAvailSecCloKpa <- bind_cols(SortDataA %>% mutate(TotCost = RestArea * Cost) %>% filter(!is.na(Kpa)) %>% group_by(Kpa) %>% summarise(ExpendAvail = sum(TotCost, na.rm = TRUE)),
                ImpactSites %>% mutate(TotCost = KoalaTreeLost * FCost) %>% filter(!is.na(SecCloKpa)) %>% group_by(SecCloKpa) %>% summarise(ExpendNeed = sum(TotCost, na.rm = TRUE), 3) %>% dplyr::select(ExpendNeed))

  # allocate offsets to second closest KPA
  if (sum(NeedAvailSecCloKpa$ExpendNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffSecCloKpa <- apply(NeedAvailSecCloKpa, MARGIN = 1, FUN = alloc_offsets_financial, SortedOffSites = SortDataA, KpaOrLga = "Kpa")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffSecCloKpa)) {
      # update impact sites
      if (round(OffSecCloKpa[[i]]$ExpendLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$SecCloKpa == NeedAvailSecCloKpa$Kpa[i])] * (OffSecCloKpa[[i]]$ExpendLeft / NeedAvailSecCloKpa$ExpendNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffSecCloKpa[[i]]$AllocIDs)) {
        OffsetSitesA$RestoreOppF[OrderA[OffSecCloKpa[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffSecCloKpa[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderA[OffSecCloKpa[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderA[OffSecCloKpa[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffSecCloKpa[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffSecCloKpa[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffSecCloKpa[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffSecCloKpa[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffSecCloKpa[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffSecCloKpa[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of most cost efficient increase in koala numbers per unit area restored based on cost

  # consider costs only in those areas where an offset is possible in restoration areas in each LGA
  CostB <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostB) <- "Cost"
  CostB[which(is.na(OffsetSitesB$RestoreOppFB)), "Cost"] <- NA

  # get order of offset site priority
  if (OffType == "financialhab") {
  # habitat area cost efficiency
    OrderB <- order(CostB, OffsetSitesB$RestoreOppFB, decreasing = FALSE, na.last = TRUE)
  }
  else if (OffType == "financialkoala") {
  # koala numbers cost efficiency
    OrderB <- order(CostB / KoalaDen, OffsetSitesB$RestoreOppFB, decreasing = FALSE, na.last = TRUE)
  }

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for restoration areas in each LGA
  SortDataB <- as.data.frame(OffsetSitesB$RestoreOppFB[OrderB])
  names(SortDataB) <- "RestArea"
  SortDataB$Cost <- CostB[OrderB, "Cost"]
  SortDataB$Lga <- Lga[OrderB, "lgasfact"]

  # identify how much restoration needs to be done in each LGA
  # identify how much restoration is possible in each LGA
  # combine into a single dataframe
  NeedAvailRestLga <- bind_cols(SortDataB %>% mutate(TotCost = RestArea * Cost) %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(ExpendAvail = sum(TotCost, na.rm = TRUE)),
                ImpactSites %>% mutate(TotCost = KoalaTreeLost * FCost) %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(ExpendNeed = sum(TotCost, na.rm = TRUE), 3) %>% dplyr::select(ExpendNeed))

  # allocate offsets to restoration areas in LGAs
  if (sum(NeedAvailRestLga$ExpendNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffRestLga <- apply(NeedAvailRestLga, MARGIN = 1, FUN = alloc_offsets_financial, SortedOffSites = SortDataB, KpaOrLga = "Lga")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffRestLga)) {
      # update impact sites
      if (round(OffRestLga[[i]]$ExpendLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailRestLga$Lga[i])] * (OffRestLga[[i]]$ExpendLeft / NeedAvailRestLga$ExpendNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffRestLga[[i]]$AllocIDs)) {
        OffsetSitesB$RestoreOppFB[OrderB[OffRestLga[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffRestLga[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderB[OffRestLga[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderB[OffRestLga[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffRestLga[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffRestLga[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffRestLga[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffRestLga[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffRestLga[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffRestLga[[i]]$Cost
        }
      }
    }
  }

  # order potential offset sites in order of most cost efficient increase in koala numbers per unit area restored based on cost

  # consider costs only in those areas where an offset is possible for non-restoration areas in each LGA
  CostC <- IncentiveCost + (OnGroundCost * (1 + (AdminCost / 100)))
  names(CostC) <- "Cost"
  CostC[which(is.na(OffsetSitesC$RestoreOppFC)), "Cost"] <- NA

  # get order of offset site priority
  if (OffType == "financialhab") {
  # habitat area cost efficiency
    OrderC <- order(CostC, OffsetSitesC$RestoreOppFC, decreasing = FALSE, na.last = TRUE)
  }
  else if (OffType == "financialkoala") {
  # koala numbers cost efficiency
    OrderC <- order(CostC / KoalaDen, OffsetSitesC$RestoreOppFC, decreasing = FALSE, na.last = TRUE)
  }

  # get the ordered areas, costs, KPAs, and LGAs of potential offset sites for non-restoration areas in each LGA
  SortDataC <- as.data.frame(OffsetSitesC$RestoreOppFC[OrderC])
  names(SortDataC) <- "RestArea"
  SortDataC$Cost <- CostC[OrderC, "Cost"]
  SortDataC$Lga <- Lga[OrderC, "lgasfact"]

  # identify how much restoration needs to be done in each LGA
  # identify how much restoration is possible in each LGA
  # combine into a single dataframe
  NeedAvailNonRestLga <- bind_cols(SortDataC %>% mutate(TotCost = RestArea * Cost) %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(ExpendAvail = sum(TotCost, na.rm = TRUE)),
                ImpactSites %>% mutate(TotCost = KoalaTreeLost * FCost) %>% filter(!is.na(Lga)) %>% group_by(Lga) %>% summarise(ExpendNeed = sum(TotCost, na.rm = TRUE), 3) %>% dplyr::select(ExpendNeed))

  # allocate offsets to restoration areas in LGAs
  if (sum(NeedAvailNonRestLga$ExpendNeed, na.rm = TRUE) > 0) {
  # offsets needed

    OffNonRestLga <- apply(NeedAvailNonRestLga, MARGIN = 1, FUN = alloc_offsets_financial, SortedOffSites = SortDataC, KpaOrLga = "Lga")

    # check which remaining impact sites need offsetting and set to NA where already offset or to proportion left if not fully offset
    # also update offset sites and compile offset locations, restored areas, and cost
    for (i in 1:length(OffNonRestLga)) {
      # update impact sites
      if (round(OffNonRestLga[[i]]$ExpendLeft, 3) == 0) {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] <- NA
      } else {
        ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] <- ImpactSites$KoalaTreeLost[which(ImpactSites$Lga == NeedAvailNonRestLga$Lga[i])] * (OffNonRestLga[[i]]$ExpendLeft / NeedAvailNonRestLga$ExpendNeed[i])
      }

      # update offset sites so sites already used are no longer available
      if (!is.null(OffNonRestLga[[i]]$AllocIDs)) {
        OffsetSitesC$RestoreOppFC[OrderC[OffNonRestLga[[i]]$AllocIDs]] <- NA
      }

      # compile offset locations
      if (!is.null(OffNonRestLga[[i]]$AllocIDs)) {
        if (is.null(OffIDsCompiled)) {
          OffIDsCompiled <- OrderC[OffNonRestLga[[i]]$AllocIDs]
        } else {
          OffIDsCompiled <- c(OffIDsCompiled, OrderC[OffNonRestLga[[i]]$AllocIDs])
        }
      }
      # compile offset areas restored
      if (!is.null(OffNonRestLga[[i]]$AllocAreas)) {
        if (is.null(RestAreasCompiled)) {
          RestAreasCompiled <- OffNonRestLga[[i]]$AllocAreas
        } else {
          RestAreasCompiled <- c(RestAreasCompiled, OffNonRestLga[[i]]$AllocAreas)
        }
      }
      # compile costs
      if ( OffNonRestLga[[i]]$Cost > 0) {
        if (is.null(CostCompiled)) {
          CostCompiled <- OffNonRestLga[[i]]$Cost
        } else {
          CostCompiled <- CostCompiled + OffNonRestLga[[i]]$Cost
        }
      }
    }
  }

  # set NAs in ImpactSitesInit$KoalaTreeLost and ImpactSites$KoalaTreeLost to zero
  ImpactSitesInit$KoalaTreeLost[which(is.na(ImpactSitesInit$KoalaTreeLost))] <- 0
  ImpactSites$KoalaTreeLost[which(is.na(ImpactSites$KoalaTreeLost))] <- 0

  return(list(Locations = OffIDsCompiled, Areas = RestAreasCompiled, Cost = CostCompiled, Impacts = ImpactSitesInit$KoalaTreeLost, ImpactsNotAlloc = ImpactSites$KoalaTreeLost))
}

RunOffSim <- function(TablesSim, RastersSim, ModelsSim, ParamsSim) {

  # record time
  tic()

  # organise input parameters, models, and data

  # tables
  Names <- names(TablesSim)
  for (i in 1:length(TablesSim)) {
    assign(Names[i], TablesSim[[i]])
  }
  # rasters
  Names <- names(RastersSim)
  for (i in 1:length(RastersSim)) {
    assign(Names[i], RastersSim[[i]])
  }
  # clearing and land-use transition models
  Names <- names(ModelsSim)
  for (i in 1:length(ModelsSim)) {
    assign(Names[i], ModelsSim[[i]])
  }
  # simulation parameters
  Names <- names(ParamsSim)
  for (i in 1:length(ParamsSim)) {
    assign(Names[i], ParamsSim[[i]])
  }
  rm(TablesSim)
  rm(RastersSim)
  gc()

  # determine where is protected, where impacts may need to be offset, and where offset sites can be located

  # areas that may be protected
  Prot <- HabKpa * (1 - Sda) * (1 - Pda) * (1 - Kbha) * (1 - Kra) # areas in a KPA where koala habitat is always protected
  names(Prot) <- "Prot"
  ProtKRA <- HabKpa * (1 - Sda) * (1 - Pda) * (1 - Kbha) * Kra # areas in Key Resource Areas where koala habitat in a KPA is protected if it is not a transition to Commercial land-use
                                                                  # if land-use transition is to Commercial then impacts must be offset
  names(ProtKRA) <- "ProtKRA"

  # areas where impacts may require offsets
  ImpOff <- (1 - Sda) * (1 - Pda) * ((HabKpa * Kbha) + (HabNotKpa * ((1 - Kbha) + (Kbha * (1 - Kra))))) # areas where impacts are always offset
  names(ImpOff) <- "ImpOff"
  ImpOffKRA <- HabNotKpa * (1 - Sda) * (1 - Pda) * Kbha * Kra # areas in Key Resource Areas where impacts only need to be offset if land-use transition is
                                                                  # not to Commercial
  names(ImpOffKRA) <- "ImpOffKRA"

  # areas where offset sites can be located
  # proponent driven (P) and financial (F) offsets
  # first and second choice offset site locations - inside restoration areas in KPAs (Section 2A.4 of the Offset Policy - closest, then second closest KPAs)
  OffSiteP <- RestKpa * TenurePrivate # areas for offset sites in restoration areas in KPAs for proponent driven offsets
  # check if national parks and state land available for financial offsets
  if (NPSLAvail == TRUE) {
    OffSiteF <- RestKpa * TenurePrivateState # areas for offset sites in restoration areas in KPAs for financial offsets
  } else {
    OffSiteF <- RestKpa * TenurePrivate # areas for offset sites in restoration areas in KPAs for financial offsets
  }
  names(OffSiteP) <- "OffSiteP"
  names(OffSiteF) <- "OffSiteF"
  # third choice offset site locations - anywhere in restoration areas not in or in a KPA (Section 2A.4 of the Offset Policy -  close as possible to impact site and KPA)
  OffSitePB <- RestNotKpa * TenurePrivate # areas for offset sites outside or inside of KPAs for proponent driven offsets
  # check if national parks and state land available for financial offsets
  if (NPSLAvail == TRUE) {
    OffSiteFB <- RestNotKpa * TenurePrivateState # areas for offset sites outside or inside of KPAs for financial offsets
  } else {
    OffSiteFB <- RestNotKpa * TenurePrivate # areas for offset sites outside or inside of KPAs for financial offsets
  }
  names(OffSitePB) <- "OffSitePB"
  names(OffSiteFB) <- "OffSiteFB"
  # fourth choice offset site locations - anywhere outside of restoration areas (Section 2A.4 of the Offset Policy - close as possible to impact site)
  OffSitePC <- (luarea - RestKpa - RestNotKpa) * TenurePrivate # areas for offset sites anywhere outside of restoration areas for proponent driven offsets
  # check if national parks and state land available for financial offsets
  if (NPSLAvail == TRUE) {
    OffSiteFC <- (luarea - RestKpa - RestNotKpa) * TenurePrivateState # areas for offset sites anywhere outside of restoration areas for financial offsets
  } else {
    OffSiteFC <- (luarea - RestKpa - RestNotKpa) * TenurePrivate # areas for offset sites anywhere outside of restoration areas for financial offsets
  }
  names(OffSitePC) <- "OffSitePC"
  names(OffSiteFC) <- "OffSiteFC"

  # set up record of impacts, offset site locations, and impact sites that should have been but were not offset
  OffsetImpactSites <- lu2016
  OffsetImpactSites[!is.na(OffsetImpactSites)] <- 0
  names(OffsetImpactSites) <- "OffsetImpactSites"
  # location of impact sites that should have been but were not offset
  OffsetImpactSitesNotAlloc <- lu2016
  OffsetImpactSitesNotAlloc[!is.na(OffsetImpactSitesNotAlloc)] <- 0
  names(OffsetImpactSitesNotAlloc) <- "OffsetImpactSitesNotAlloc"
  # location of offset sites
  OffsetSites <- lu2016
  OffsetSites[!is.na(OffsetSites)] <- 0
  names(OffsetSites) <- "OffsetSites"
  # iteration offset site implemented
  OffsetSitesIter <- lu2016
  OffsetSitesIter[!is.na(OffsetSitesIter)] <- NA
  names(OffsetSitesIter) <- "OffsetSitesIter"


  # compile land-use change predictors into a data frame
  Predictors <- as.data.frame(stack(c(slope, elev, road, city, roadDen, awc, cly, NeighUrb, NeighInd, UFfact, lgasfact)))
  # scale data the same way that they are scaled in the fitting process
  Predictors <- Predictors %>% mutate_at(c("slope", "elev", "road", "city", "roadDen", "awc", "cly", "NeighUrb", "NeighInd"), ~(scale(.)))
  # convert to factors
  Predictors$UFfact <- as.factor(Predictors$UFfact)
  Predictors$lgasfact <- as.factor(Predictors$lgasfact)

  # define urban demand based on ShapingSEQ (2017).
  # the matrix represents new dwelling demand for LGAs and consolidation and expansion combinations
  # LGAs and whether existing urban areas or not (LGAExistUrb) 1 = LGA 1 not urban (expansion), 2 = LGA 1 urban (consolidation), ...., 15 = LGA 8 not urban (expansion), 16 = LGA 8 urban (consolidation). LGAs for the study region for defining dwelling growth targets [1 = Moreton Bay, 2 = Noosa, 3 = Redland, 4 = Sunshine Coast, 5 = Brisbane, 6 = Gold Coast, 7 = Ipswich, 8 = Logan] & existing urban areas as defined in Shaping SEQ [1 = existing urban area, 0 = not existing urban area]
  if (Horizon == 2031) {
    # 2031 demand
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(25600,29300,1500,2600,3400,8900,20700,28900,4900,105700,20600,70000,43000,14500,33300,11500))
    # adjust for the percent consolidation
    for (i in 1:16) {
      if ((i %% 2) == 1) {
          UrbanDemand[i, "Demand"] <- UrbanDemand[i, "Demand"] + UrbanDemand[i + 1, "Demand"] * (1 - (PercConsol / 100))
      }
      else {
          UrbanDemand[i, "Demand"] <- UrbanDemand[i, "Demand"] * (PercConsol / 100)
      }
    }
  } else if (Horizon == 2041) {
    # 2041 demand
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(40100,48200,1600,4800,4700,12500,33300,53700,11400,176800,31000,127900,83800,27900,70000,19900))
    # adjust for the percent consolidation
    for (i in 1:16) {
      if ((i %% 2) == 1) {
          UrbanDemand[i, "Demand"] <- UrbanDemand[i, "Demand"] + UrbanDemand[i + 1, "Demand"] * (1 - (PercConsol / 100))
      }
      else {
          UrbanDemand[i, "Demand"] <- UrbanDemand[i, "Demand"] * (PercConsol / 100)
      }
    }
  } else {
    UrbanDemand <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Demand" = c(Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf))
  }

  # create raster to hold new spatial dwelling growth
  DwellGrowth <- lu2016
  DwellGrowth[!is.na(DwellGrowth)] <- 0
  names(DwellGrowth) <- "DGrowth"

  # create data frame version of LGAs and existing urban areas layers
  LGAExistUrb.df <- as.data.frame(LGAExistUrb)
  names(LGAExistUrb.df) <- "LGAExistUrb"
  # create data frame to hold simulated dwelling growth for each LGA urban/non-urban combination
  DwellGrowthLGAExistUrb <- data.frame("LGAExistUrb" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "Growth0" = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  # create vector to hold tests of whether dwelling targets have been met or not
  TargetTest <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  # set up data frame to hold where target constraints are met to prevent transitions to 51, 52, and 53 when targets have been met
  TargetCons <- cbind(LGAExistUrb.df$LGAExistUrb, as.data.frame(matrix(rep(1,dim(LGAExistUrb.df)[[1]] * 13), nrow = dim(LGAExistUrb.df)[[1]], ncol = 13)))
  dimnames(TargetCons)[[2]] <- c("LGAExistUrb", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # set up static constraints based on the planning scheme and Shaping SEQ for 2017
  # compile planning scheme constraints into data frame for each land use
  plan2017.df <- as.data.frame(plan2017)
  PlanCons <- plan2017.df %>% left_join(plan2017tb, by = "plan2017")
  dimnames(PlanCons)[[2]] <- c("plan2017", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # get transition probability constraints that are assumed not possible based the land use classes in ShapingSEQ
  # these constraints are that transitions to medium (52) and high density (53) residential development are not permitted in the Rural Living Area
  UFfact.df <- as.data.frame(UFfact)
  UFCons <- UFfact.df %>% left_join(UFfacttb, by = "UFfact")
  dimnames(UFCons)[[2]] <- c("UFfact", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")

  # set up current land use
  lucurr <- lu2016 # set current land use to 2016 land use
  lu2016.df <- as.data.frame(lu2016) # set 2016 land use in data frame format
  lucurr.df <- lu2016.df # set current land use as a data frame
  names(lucurr) <- "lucurr"
  names(lu2016.df) <- "lu2016"
  names(lucurr.df) <- "lucurr"

  # set up habitat and koala density data frames
  HabHM.df <- as.data.frame(HabHM)
  HabML.df <- as.data.frame(HabML)
  HabVL.df <- as.data.frame(HabVL)
  HabCore.df <- as.data.frame(HabCore)
  HabNonCore.df <- as.data.frame(HabNonCore)
  HabHMPre.df <- as.data.frame(HabHMPre)
  HabMLPre.df <- as.data.frame(HabMLPre)
  HabVLPre.df <- as.data.frame(HabVLPre)
  HabCorePre.df <- as.data.frame(HabCorePre)
  HabNonCorePre.df <- as.data.frame(HabNonCorePre)
  KDen.df <- as.data.frame(KDen)

  # set up regulation, offset, land value, and cost dataframes
  Prot.df <- as.data.frame(Prot)
  ProtKRA.df <- as.data.frame(ProtKRA)
  ImpOff.df <- as.data.frame(ImpOff)
  ImpOffKRA.df <- as.data.frame(ImpOffKRA)
  OffSiteP.df <- as.data.frame(OffSiteP)
  OffSiteF.df <- as.data.frame(OffSiteF)
  OffSitePB.df <- as.data.frame(OffSitePB)
  OffSiteFB.df <- as.data.frame(OffSiteFB)
  OffSitePC.df <- as.data.frame(OffSitePC)
  OffSiteFC.df <- as.data.frame(OffSiteFC)
  lgasfact.df <- as.data.frame(lgasfact)
  Kpa.df <- as.data.frame(Kpa)
  KpaClo.df <- as.data.frame(KpaClo)
  KpaSecClo.df <- as.data.frame(KpaSecClo)
  LandVal.df <- as.data.frame(LandVal)
  FinIncent.df <- as.data.frame(FinIncent)

  # set up data frames to record offset outcomes
  OffsetImpactSites.df <- as.data.frame(OffsetImpactSites)
  OffsetImpactSitesNotAlloc.df <- as.data.frame(OffsetImpactSitesNotAlloc)
  OffsetSites.df <- as.data.frame(OffsetSites)
  OffsetSitesIter.df <- as.data.frame(OffsetSitesIter)
  # current land use raster lists
  LandUseList <- list()
  LandUseList[[1]] <- lucurr
  # dwelling growth raster list
  DwellGrowthList <- list()
  DwellGrowthList[[1]] <- DwellGrowth

  # offsetable impacts raster list
  OffsetImpactSitesList <- list()
  OffsetImpactSitesList[[1]] <- OffsetImpactSites
  # offsetable impact sites not offset
  OffsetImpactSitesNotAllocList <- list()
  OffsetImpactSitesNotAllocList[[1]] <- OffsetImpactSitesNotAlloc
  # offset sites raster list
  OffsetSitesList <- list()
  OffsetSitesList[[1]] <- OffsetSites
  # offset sites iteration raster list
  OffsetSitesIterList <- list()
  OffsetSitesIterList[[1]] <- OffsetSitesIter


  # habitat raster lists
  HabHMList <- list()
  HabMLList <- list()
  HabVLList <- list()
  HabCoreList <- list()
  HabNonCoreList <- list()
  HabHMList[[1]] <- HabHM
  HabMLList[[1]] <- HabML
  HabVLList[[1]] <- HabVL
  HabCoreList[[1]] <- HabCore
  HabNonCoreList[[1]] <- HabNonCore
  # koala numbers raster list
  KNumList <- list()
  KNumList[[1]] <- KDen * (HabCore + HabNonCore)

  # set aggregated offset outcomes and costs
  AggOffImpact <- 0
  AggOffImpactList <- list()
  AggOffImpactList[[1]] <- 0
  AggOffImpactNotAllocated <- 0
  AggOffImpactNotAllocatedList <- list()
  AggOffImpactNotAllocatedList[[1]] <- 0
  AggOffRest <- 0
  AggOffRestList <- list()
  AggOffRestList[[1]] <- 0
  AggOffCost <- 0
  AggOffCostList <- list()
  AggOffCostList[[1]] <- 0
  AggHabHM <- sum(HabHM.df, na.rm = TRUE)
  AggHabHMList <- list()
  AggHabHMList[[1]] <- AggHabHM
  AggHabML <- sum(HabML.df, na.rm = TRUE)
  AggHabMLList <- list()
  AggHabMLList[[1]] <- AggHabML
  AggHabVL <- sum(HabVL.df, na.rm = TRUE)
  AggHabVLList <- list()
  AggHabVLList[[1]] <- AggHabVL
  AggHabCore <- sum(HabCore.df, na.rm = TRUE)
  AggHabCoreList <- list()
  AggHabCoreList[[1]] <- AggHabCore
  AggHabNonCore <- sum(HabNonCore.df, na.rm = TRUE)
  AggHabNonCoreList <- list()
  AggHabNonCoreList[[1]] <- AggHabNonCore
  AggKNum <- sum(KDen.df * (HabCore.df + HabNonCore.df), na.rm = TRUE)
  AggKNumList <- list()
  AggKNumList[[1]] <- AggKNum

  # loop through iterations to run simulation

  for (i in 1:MaxIter) {
    # constraints on transition to 51, 52, 53 where dwelling targets already met
    # if some dwelling targets met, then adjust constraints so we get no further transitions to 51, 52, and 53
    if (length(which(TargetTest == 1)) > 0) {
        for (j in which(TargetTest == 1)) {
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 51)), "51"] <- 0
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 52)), "52"] <- 0
          TargetCons[which((TargetCons[,"LGAExistUrb"] == j) & (lucurr.df[,"lucurr"] != 53)), "53"] <- 0
       }
    }
    TargetCons[which(is.na(TargetCons$LGAExistUrb)), c("10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")] <- NA

    # get transition probability predictions incorporating planning scheme urban footprint constraints only if regulation assumed (Reg = TRUE)
    if (Reg == TRUE) {
      Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables] * PlanCons[which(lucurr.df$lucurr == x), Variables] * UFCons[which(lucurr.df$lucurr == x), Variables] * TargetCons[which(lucurr.df$lucurr == x), Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})
    } else {
      Predictions <- lapply(luLabelList, FUN = function(x){Pred <- as.data.frame(stats::predict(TMods[[which(luLabel == x)]], newdata = Predictors[which(lucurr.df$lucurr == x),], type = "probs", se = TRUE, na.action = na.exclude)); Variables <- dimnames(Pred)[[2]][which(!dimnames(Pred)[[2]] == as.character(x))]; Pred[, Variables] <- Pred[, Variables] * UFCons[which(lucurr.df$lucurr == x), Variables] * TargetCons[which(lucurr.df$lucurr == x), Variables]; Sums <- rowSums(Pred); Pred <- Pred / Sums; return(Pred)})
    }

    # set transitions that are considered not possible to zero
    # assume the following rules
    # 1. Rural residential (40) cannot transition to grazing (21), crops (22), or transition (23)
    # 2. Low density urban (51) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    # 3. Medium density urban (52) cannot transition to low density urban (51), rural residential (40), grazing (21), crops (22), or transition (23)
    # 4. High density urban (53) cannot transition to Medium density urban (52), low density urban (51), rural residential (40), grazing (21), crops (22), or transition (23)
    # 5. Commercial (60) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    # 6. Intensive agriculture (71) cannot transition to grazing (21), crops (22), or transition (23)
    # 7. Industrial (72) cannot transition to rural residential (40), grazing (21), crops (22), or transition (23)
    Predictions[[4]] <- set_trans_zero(Predictions[[4]], c("21","22","23"))
    Predictions[[5]] <- set_trans_zero(Predictions[[5]], c("40","21","22","23"))
    Predictions[[6]] <- set_trans_zero(Predictions[[6]], c("51","40","21","22","23"))
    Predictions[[7]] <- set_trans_zero(Predictions[[7]], c("52","51","40","21","22","23"))
    Predictions[[8]] <- set_trans_zero(Predictions[[8]], c("40","21","22","23"))
    Predictions[[9]] <- set_trans_zero(Predictions[[9]], c("21","22","23"))
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
    # adjust habitat and land use change accordingly
    if (Reg == TRUE) {
      HabLoss.df[which(((HabLoss.df > 0) & (Prot.df == 1)) | ((HabLoss.df > 0) & (ProtKRA.df == 1) & (LUChange.df[,"lunew"] != "60"))), "HabLoss"] <- 0
      LUChange.df[which(((HabLoss.df > 0) & (Prot.df == 1)) | ((HabLoss.df > 0) & (ProtKRA.df == 1) & (LUChange.df[,"lunew"] != "60"))), "lunew"] <-
                LUChange.df[which(((HabLoss.df > 0) & (Prot.df == 1)) | ((HabLoss.df > 0) & (ProtKRA.df == 1) & (LUChange.df[,"lunew"] != "60"))), "lucurr"]
      NewLU.df$lucurr <- LUChange.df$lunew
    }

    # get the amount of koala habitat lost
    KoalaTreeLost.df <- (HabCore.df + HabNonCore.df) * HabLoss.df
    names(KoalaTreeLost.df) <- "KoalaTreeLost"

    # get new habitat amounts without offsets
    HabHM.df <- HabHM.df * (1 - HabLoss.df)
    names(HabHM.df) <- "HabHM"
    HabML.df <- HabML.df * (1 - HabLoss.df)
    names(HabML.df) <- "HabML"
    HabVL.df <- HabVL.df * (1 - HabLoss.df)
    names(HabVL.df) <- "HabVL"
    HabCore.df <- HabCore.df * (1 - HabLoss.df)
    names(HabCore.df) <- "HabCore"
    HabNonCore.df <- HabNonCore.df * (1 - HabLoss.df)
    names(HabNonCore.df) <- "HabNonCore"

    if (OffType != "none" & Reg == TRUE) {
    # offsets policy implemented - if regulation not considered then no offsets assumed possible, i.e., offsets cannot occur in isolation of the planning regulation
    # identify where offsets are required and allocate offset sites for restoration

      # get impacts that require offsetting
      KoalaTreeLostOff.df <- KoalaTreeLost.df # to record where tree loss requires offsetting
      # add to data frame with closest KPA, the second closest KPA, and the LGA
      KoalaTreeLostOff.df$CloKpa <- KpaClo.df$KpaClo
      KoalaTreeLostOff.df$SecCloKpa <- KpaSecClo.df$KpaSecClo
      KoalaTreeLostOff.df$Lga <- lgasfact.df$lgasfact
      # add financial payment if financial offsets
      if (OffType == "financialhab" | OffType == "financialkoala") {
        KoalaTreeLostOff.df$FCost <- ifelse(((FinIncent.df$FinIncent + (OnGroundCostHa * (1 + (PercAdminCost / 100)))) * Multiplier) < MaxFinCost, (FinIncent.df$FinIncent + (OnGroundCostHa * (1 + (PercAdminCost / 100)))) * Multiplier, MaxFinCost)
    }
      # set values in KoalaTreeLostOff in areas not requiring offsets to zero
      KoalaTreeLostOff.df$KoalaTreeLost[!which((ImpOff.df == 1) & ((ProtKRA.df == 1) & (LUChange.df[,"lunew"] == "60")) & ((ImpOffKRA.df == 1) & (LUChange.df[,"lunew"] != "60")))] <- 0
      # add areas that were not offset in previous iterations
      KoalaTreeLostOff.df$KoalaTreeLost <- KoalaTreeLostOff.df$KoalaTreeLost + OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc

      # get the opportunities and back-up opportunities for the amount of koala habitat that could be restored in places that are in the permitted restoration areas and are of land uses 10, 21, 22, 23, 30, 40 for proponent drive and financial offsets

      # proponent driven offsets
      if (OffType == "proponent") {
        # get all sites available and set unavailable sites to NA
        RestoreOppP.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppP.df) <- "RestoreOppP"
        RestoreOppP.df[which((!((OffSiteP.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppP.df <= 0)), "RestoreOppP"] <- NA
        RestoreOppPB.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppPB.df) <- "RestoreOppPB"
        RestoreOppPB.df[which((!((OffSitePB.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppPB.df <= 0)), "RestoreOppPB"] <- NA
        RestoreOppPC.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppPC.df) <- "RestoreOppPC"
        RestoreOppPC.df[which((!((OffSitePC.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppPC.df <= 0)), "RestoreOppPC"] <- NA

        # get sites available if site availability < 100
        # get a random sample of the potential offset sites based on the percent available and set unavailable sites to NA
        if (PercAvail < 100) {
          NotAvailP <- sample(which(RestoreOppP.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppP.df > 0))))
          RestoreOppP.df[NotAvailP, "RestoreOppP"] <- NA
          NotAvailPB <- sample(which(RestoreOppPB.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppPB.df > 0))))
          RestoreOppPB.df[NotAvailPB, "RestoreOppPB"] <- NA
          NotAvailPC <- sample(which(RestoreOppPC.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppPC.df > 0))))
          RestoreOppPC.df[NotAvailPC, "RestoreOppPC"] <- NA
        }
      }

      # financial offsets
      if (OffType == "financialhab" | OffType == "financialkoala") {
        # get all sites available and set unavailable sites to NA
        RestoreOppF.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppF.df) <- "RestoreOppF"
        RestoreOppF.df[which((!((OffSiteF.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppF.df <= 0)), "RestoreOppF"] <- NA
        RestoreOppFB.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppFB.df) <- "RestoreOppFB"
        RestoreOppFB.df[which((!((OffSiteFB.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppFB.df <= 0)), "RestoreOppFB"] <- NA
        RestoreOppFC.df <- (HabCorePre.df + HabNonCorePre.df) - (HabCore.df + HabNonCore.df)
        names(RestoreOppFC.df) <- "RestoreOppFC"
        RestoreOppFC.df[which((!((OffSiteFC.df == 1) & ((NewLU.df == 10) | (NewLU.df == 21) | (NewLU.df == 22) | (NewLU.df == 23) | (NewLU.df == 30) | (NewLU.df == 40)))) | (RestoreOppFC.df <= 0)), "RestoreOppFC"] <- NA

        # get sites available if site availability < 100
        # get a random sample of the potential offset sites based on the percent available and set unavailable sites to NA
        if (PercAvail < 100) {
          NotAvailF <- sample(which(RestoreOppF.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppF.df > 0))))
          RestoreOppF.df[NotAvailF, "RestoreOppF"] <- NA
          NotAvailFB <- sample(which(RestoreOppFB.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppFB.df > 0))))
          RestoreOppFB.df[NotAvailFB, "RestoreOppFB"] <- NA
          NotAvailFC <- sample(which(RestoreOppFC.df > 0), size = ceiling((1 - (PercAvail / 100)) * length(which(RestoreOppFC.df > 0))))
          RestoreOppFC.df[NotAvailFC, "RestoreOppFC"] <- NA
        }
      }

      # Allocate offset sites
      if (OffType == "proponent") {
      # proponent driven offsets

        # allocate offsets
        PropOff <- get_off_sites_proponent(ImpactSites = KoalaTreeLostOff.df, OffsetSitesA = RestoreOppP.df, OffsetSitesB = RestoreOppPB.df, OffsetSitesC = RestoreOppPC.df, Kpa = Kpa.df, Lga = lgasfact.df, IncentiveCost = LandVal.df, OnGroundCost = OnGroundCostHa, AdminCost = PercAdminCost, Multiplier = Multiplier)

        # update offset locations, impact and offset sites, impacts not offset, protected areas, and offset area, and habitat areas
        if (length(PropOff$Locations) > 0) {
          # update offset impacts offset, offset sites, and impacts not offset data frames
          Success <- rbinom(length(PropOff$Locations), 1, RestSucc) # whether the offset is successful or not
          OffsetImpactSites.df$OffsetImpactSites[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] <- OffsetImpactSites.df$OffsetImpactSites[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] + PropOff$Impacts[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] - OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))]
          OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc <- PropOff$ImpactsNotAlloc
          #OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc[which(!is.na(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc))] <-PropOff$ImpactsNotAlloc[which(!is.na(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc))]
          OffsetSites.df$OffsetSites[PropOff$Locations] <- OffsetSites.df$OffsetSites[PropOff$Locations] + PropOff$Areas * Success
          OffsetSitesIter.df$OffsetSitesIter[PropOff$Locations] <- i

          # update protected status and availability for offset sites at the offset sites data frames
          Prot.df$Prot[PropOff$Locations] <- 1
          ProtKRA.df$ProtKRA[PropOff$Locations] <- 0
          OffSiteP.df$OffSiteP[PropOff$Locations] <- 0
          OffSitePB.df$OffSitePB[PropOff$Locations] <- 0
          OffSitePC.df$OffSitePC[PropOff$Locations] <- 0

          # update habitat area data frames
          ProportionRestored <- PropOff$Areas / ((HabCorePre.df$HabCorePre[PropOff$Locations] + HabNonCorePre.df$HabNonCorePre[PropOff$Locations]) - (HabCore.df$HabCore[PropOff$Locations] + HabNonCore.df$HabNonCore[PropOff$Locations]))
          HabHM.df$HabHM[PropOff$Locations] <- (HabHM.df$HabHM[PropOff$Locations] + ((HabHMPre.df$HabHMPre[PropOff$Locations] - HabHM.df$HabHM[PropOff$Locations]) * ProportionRestored) * Success) + (HabHM.df$HabHM[PropOff$Locations] * ( 1 - Success))
          HabML.df$HabML[PropOff$Locations] <- (HabML.df$HabML[PropOff$Locations] + ((HabMLPre.df$HabMLPre[PropOff$Locations] - HabML.df$HabML[PropOff$Locations]) * ProportionRestored) * Success) + (HabML.df$HabML[PropOff$Locations] * ( 1 - Success))
          HabVL.df$HabVL[PropOff$Locations] <- (HabVL.df$HabVL[PropOff$Locations] + ((HabVLPre.df$HabVLPre[PropOff$Locations] - HabVL.df$HabVL[PropOff$Locations]) * ProportionRestored) * Success) + (HabVL.df$HabVL[PropOff$Locations] * ( 1 - Success))
          HabCore.df$HabCore[PropOff$Locations] <- (HabCore.df$HabCore[PropOff$Locations] + ((HabCorePre.df$HabCorePre[PropOff$Locations] - HabCore.df$HabCore[PropOff$Locations]) * ProportionRestored) * Success) + (HabCore.df$HabCore[PropOff$Locations] * ( 1 - Success))
          HabNonCore.df$HabNonCore[PropOff$Locations] <- (HabNonCore.df$HabNonCore[PropOff$Locations] + ((HabNonCorePre.df$HabNonCorePre[PropOff$Locations] - HabNonCore.df$HabNonCore[PropOff$Locations]) * ProportionRestored) * Success) + (HabNonCore.df$HabNonCore[PropOff$Locations] * ( 1 - Success))

          # increment aggregated offset outcomes
          AggOffImpact <- sum(OffsetImpactSites.df$OffsetImpactSites, na.rm = TRUE)
          AggOffImpactNotAllocated <- sum(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc, na.rm = TRUE)
          AggOffRest <- sum(OffsetSites.df$OffsetSites, na.rm = TRUE)
          AggOffCost <- AggOffCost + PropOff$Cost
        }
      } else if (OffType == "financialhab" | OffType == "financialkoala") {
      # financial offsets - assumed maximise return on investment in koala numbers or return on investment in koala habitat area

        # allocate offsets
        FinOff <- get_off_sites_financial(ImpactSites = KoalaTreeLostOff.df, OffsetSitesA = RestoreOppF.df, OffsetSitesB = RestoreOppFB.df, OffsetSitesC = RestoreOppFC.df, KoalaDen = KDen.df, Kpa = Kpa.df, Lga = lgasfact.df, IncentiveCost = LandVal.df, OnGroundCost = OnGroundCostHa, AdminCost = PercAdminCost, Multiplier = Multiplier, OffType = OffType)

        # update offset locations, impact and offset sites, impacts not offset, protected areas, and offset area, and habitat areas
        if (length(FinOff$Locations) > 0) {
          # update offset impacts offset, offset sites, and impacts not offset data frames
          Success <- rbinom(length(FinOff$Locations), 1, RestSucc) # whether the offset is successful or not
          OffsetImpactSites.df$OffsetImpactSites[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] <- OffsetImpactSites.df$OffsetImpactSites[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] + FinOff$Impacts[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))] - OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc[which(!is.na(OffsetImpactSites.df$OffsetImpactSites))]
          OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc <- FinOff$ImpactsNotAlloc
          #OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc[which(!is.na(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc))] <- FinOff$ImpactsNotAlloc[which(!is.na(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc))]
          OffsetSites.df$OffsetSites[FinOff$Locations] <- OffsetSites.df$OffsetSites[FinOff$Locations] + FinOff$Areas * Success
          OffsetSitesIter.df$OffsetSitesIter[FinOff$Locations] <- i

          # update protected status and availability for offset sites at the offset sites data frames
          Prot.df$Prot[FinOff$Locations] <- 1
          ProtKRA.df$ProtKRA[FinOff$Locations] <- 0
          OffSiteP.df$OffSiteP[FinOff$Locations] <- 0
          OffSitePB.df$OffSitePB[FinOff$Locations] <- 0
          OffSitePC.df$OffSitePC[FinOff$Locations] <- 0

          # update habitat area data frames
          ProportionRestored <- FinOff$Areas / ((HabCorePre.df$HabCorePre[FinOff$Locations] + HabNonCorePre.df$HabNonCorePre[FinOff$Locations]) - (HabCore.df$HabCore[FinOff$Locations] + HabNonCore.df$HabNonCore[FinOff$Locations]))
          HabHM.df$HabHM[FinOff$Locations] <- (HabHM.df$HabHM[FinOff$Locations] + ((HabHMPre.df$HabHMPre[FinOff$Locations] - HabHM.df$HabHM[FinOff$Locations]) * ProportionRestored) * Success) + (HabHM.df$HabHM[FinOff$Locations] * ( 1 - Success))
          HabML.df$HabML[FinOff$Locations] <- (HabML.df$HabML[FinOff$Locations] + ((HabMLPre.df$HabMLPre[FinOff$Locations] - HabML.df$HabML[FinOff$Locations]) * ProportionRestored) * Success) + (HabML.df$HabML[FinOff$Locations] * ( 1 - Success))
          HabVL.df$HabVL[FinOff$Locations] <- (HabVL.df$HabVL[FinOff$Locations] + ((HabVLPre.df$HabVLPre[FinOff$Locations] - HabVL.df$HabVL[FinOff$Locations]) * ProportionRestored) * Success) + (HabVL.df$HabVL[FinOff$Locations] * ( 1 - Success))
          HabCore.df$HabCore[FinOff$Locations] <- (HabCore.df$HabCore[FinOff$Locations] + ((HabCorePre.df$HabCorePre[FinOff$Locations] - HabCore.df$HabCore[FinOff$Locations]) * ProportionRestored) * Success) + (HabCore.df$HabCore[FinOff$Locations] * ( 1 - Success))
          HabNonCore.df$HabNonCore[FinOff$Locations] <- (HabNonCore.df$HabNonCore[FinOff$Locations] + ((HabNonCorePre.df$HabNonCorePre[FinOff$Locations] - HabNonCore.df$HabNonCore[FinOff$Locations]) * ProportionRestored) * Success) + (HabNonCore.df$HabNonCore[FinOff$Locations] * ( 1 - Success))

          # increment aggregated offset outcomes
          AggOffImpact <- sum(OffsetImpactSites.df$OffsetImpactSites, na.rm = TRUE)
          AggOffImpactNotAllocated <- sum(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc, na.rm = TRUE)
          AggOffRest <- sum(OffsetSites.df$OffsetSites, na.rm = TRUE)
          AggOffCost <- AggOffCost + FinOff$Cost
        }
      } else if (OffType == "both") {
      # proponents have choice between A proponent driven and A financial offset payment

        # NOT IMPLEMENTED YET

      }
    }

    # record offset outcomes in list
    OffsetImpactSites <- to_raster(OffsetImpactSites.df$OffsetImpactSites, lucurr)
    names(OffsetImpactSites) <- "OffsetImpactSites"
    OffsetImpactSitesNotAlloc <- to_raster(OffsetImpactSitesNotAlloc.df$OffsetImpactSitesNotAlloc, lucurr)
    names(OffsetImpactSitesNotAlloc) <- "OffsetImpactSitesNotAlloc"
    OffsetSites <- to_raster(OffsetSites.df$OffsetSites, lucurr)
    names(OffsetSites) <- "OffsetSites"
    OffsetSitesIter <- to_raster(OffsetSitesIter.df$OffsetSitesIter, lucurr)
    names(OffsetSitesIter) <- "OffsetSitesIter"
    if (i %% ReportSteps == 0) {
      OffsetImpactSitesList[[floor(i / ReportSteps) + 1]] <- OffsetImpactSites
      OffsetImpactSitesNotAllocList[[floor(i / ReportSteps) + 1]] <- OffsetImpactSitesNotAlloc
      OffsetSitesList[[floor(i / ReportSteps) + 1]] <- OffsetSites
      OffsetSitesIterList[[floor(i / ReportSteps) + 1]] <- OffsetSitesIter
      AggOffImpactList[[floor(i / ReportSteps) + 1]] <- AggOffImpact
      AggOffImpactNotAllocatedList[[floor(i / ReportSteps) + 1]] <- AggOffImpactNotAllocated
      AggOffRestList[[floor(i / ReportSteps) + 1]] <- AggOffRest
      AggOffCostList[[floor(i / ReportSteps) + 1]] <- AggOffCost
    }

    # record habitat in list
    HabHMNew <- to_raster(HabHM.df$HabHM, lucurr)
    names(HabHM) <- "HabHM"
    HabMLNew <- to_raster(HabML.df$HabML, lucurr)
    names(HabML)  <- "HabML"
    HabVLNew <- to_raster(HabVL.df$HabVL, lucurr)
    names(HabVL) <- "HabVL"
    HabCoreNew <- to_raster(HabCore.df$HabCore, lucurr)
    names(HabCoreNew) <- "HabCore"
    HabNonCoreNew <- to_raster(HabNonCore.df$HabNonCore, lucurr)
    names(HabNonCoreNew) <- "HabNonCore"
    KNumNew <- KDen * (HabCoreNew + HabNonCoreNew)
    names(KNumNew) <- "KNum"
    AggHabHM <- sum(HabHM.df, na.rm = TRUE)
    AggHabML <- sum(HabML.df, na.rm = TRUE)
    AggHabVL <- sum(HabVL.df, na.rm = TRUE)
    AggHabCore <- sum(HabCore.df, na.rm = TRUE)
    AggHabNonCore <- sum(HabNonCore.df, na.rm = TRUE)
    AggKNum <- sum(KDen.df * (HabCore.df + HabNonCore.df), na.rm = TRUE)
    if (i %% ReportSteps == 0) {
      HabHMList[[floor(i / ReportSteps) + 1]] <- HabHMNew
      HabMLList[[floor(i / ReportSteps) + 1]] <- HabMLNew
      HabVLList[[floor(i / ReportSteps) + 1]] <- HabVLNew
      HabCoreList[[floor(i / ReportSteps) + 1]] <- HabCoreNew
      HabNonCoreList[[floor(i / ReportSteps) + 1]] <- HabNonCoreNew
      KNumList[[floor(i / ReportSteps) + 1]] <- KNumNew
      AggHabHMList[[floor(i / ReportSteps) + 1]] <-AggHabHM
      AggHabMLList[[floor(i / ReportSteps) + 1]] <-AggHabML
      AggHabVLList[[floor(i / ReportSteps) + 1]] <-AggHabVL
      AggHabCoreList[[floor(i / ReportSteps) + 1]] <-AggHabCore
      AggHabNonCoreList[[floor(i / ReportSteps) + 1]] <-AggHabNonCore
      AggKNumList[[floor(i / ReportSteps) + 1]] <-AggKNum
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
    if (i %% ReportSteps == 0) {
      DwellGrowthList[[floor(i / ReportSteps) + 1]] <- DwellGrowth
    }

    # update test for meeting dwelling growth targets and adjust transition probabilities where necessary
    # if met dwelling growth targets then set transitions to 51, 52, and 53 to zero
    ZonalDwellGrowth <- as.data.frame(zonal(DwellGrowth, LGAExistUrb, fun = "sum", na.rm = TRUE))
    TargetTest <- ifelse(ZonalDwellGrowth$sum >= UrbanDemand$Demand, 1, 0)
    if (i %% ReportSteps == 0) {
      Label <- paste("Growth", i, sep = "")
      DwellGrowthLGAExistUrb <- cbind(DwellGrowthLGAExistUrb, ZonalDwellGrowth[,2])
      names(DwellGrowthLGAExistUrb)[length(names(DwellGrowthLGAExistUrb))] <- Label
    }

    # assign new land use to current land use data frame
    lucurr.df <- NewLU.df
    names(lucurr.df) <- "lucurr"
    # record new land use in list
    lucurr <- to_raster(lucurr.df$lucurr, lucurr)
    if (i %% ReportSteps == 0) {
      LandUseList[[floor(i / ReportSteps) + 1]] <- lucurr
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
        if (i %% ReportSteps != 0) {
          OffsetImpactSitesList[[floor(i / ReportSteps) + 2]] <- OffsetImpactSites
          OffsetImpactSitesNotAllocList[[floor(i / ReportSteps) + 2]] <- OffsetImpactSitesNotAlloc
          OffsetSitesList[[floor(i / ReportSteps) + 2]] <- OffsetSites
          OffsetSitesIterList[[floor(i / ReportSteps) + 2]] <- OffsetSitesIter
          AggOffImpactList[[floor(i / ReportSteps) + 2]] <- AggOffImpact
          AggOffImpactNotAllocatedList[[floor(i / ReportSteps) + 2]] <- AggOffImpactNotAllocated
          AggOffRestList[[floor(i / ReportSteps) + 2]] <- AggOffRest
          AggOffCostList[[floor(i / ReportSteps) + 2]] <- AggOffCost
          HabHMList[[floor(i / ReportSteps) + 2]] <- HabHMNew
          HabMLList[[floor(i / ReportSteps) + 2]] <- HabMLNew
          HabVLList[[floor(i / ReportSteps) + 2]] <- HabVLNew
          HabCoreList[[floor(i / ReportSteps) + 2]] <- HabCoreNew
          HabNonCoreList[[floor(i / ReportSteps) + 2]] <- HabNonCoreNew
          KNumList[[floor(i / ReportSteps) + 2]] <- KNumNew
          AggHabHMList[[floor(i / ReportSteps) + 2]] <-AggHabHM
          AggHabMLList[[floor(i / ReportSteps) + 2]] <-AggHabML
          AggHabVLList[[floor(i / ReportSteps) + 2]] <-AggHabVL
          AggHabCoreList[[floor(i / ReportSteps) + 2]] <-AggHabCore
          AggHabNonCoreList[[floor(i / ReportSteps) + 2]] <-AggHabNonCore
          AggKNumList[[floor(i / ReportSteps) + 2]] <-AggKNum
          DwellGrowthList[[floor(i / ReportSteps) + 2]] <- DwellGrowth
          LandUseList[[floor(i / ReportSteps) + 2]] <- lucurr
          Label <- paste("Growth", i, sep = "")
          DwellGrowthLGAExistUrb <- cbind(DwellGrowthLGAExistUrb, ZonalDwellGrowth[,2])
          names(DwellGrowthLGAExistUrb)[length(names(DwellGrowthLGAExistUrb))] <- Label
        }
        break
    }
  }

  # compile dwelling growth target
  DwelGrowthTarget <- UrbanDemand$Demand
  names(DwelGrowthTarget) <- "DwelGrowthTarget"

  # record execution time
  ExecTime <- toc()

  Output <- list(LandUseList, DwellGrowthList, cbind(DwellGrowthLGAExistUrb, DwelGrowthTarget), HabHMList, HabMLList, HabVLList, HabCoreList, HabNonCoreList, KNumList, unlist(AggHabHMList), unlist(AggHabMLList), unlist(AggHabVLList), unlist(AggHabCoreList), unlist(AggHabNonCoreList), unlist(AggKNumList), OffsetImpactSitesList, OffsetSitesList, OffsetSitesIterList, OffsetImpactSitesNotAllocList, unlist(AggOffImpactList), unlist(AggOffRestList), unlist(AggOffCostList), unlist(AggOffImpactNotAllocatedList), i, ParamsSim, ExecTime$toc - ExecTime$tic)
  names(Output) <- c("LandUse", "DwellNum", "DwellGrowthLGAExistUrb", "HabHM", "HabML", "HabVL", "HabCore", "HabNonCore", "KoalaNum", "AggHabHM", "AggHabML", "AggHabVL", "AggHabCore", "AggHabNonCore", "AggKNum", "OffsetImpactSites", "OffsetSites", "OffsetSitesIter", "OffsetImpactSitesNotAlloc", "AggOffImpact", "AggOffRest", "AggOffCost", "AggOffImpactNotAllocated", "Iterations", "SimParams", "ExecuteTime")

  return(Output)
}
