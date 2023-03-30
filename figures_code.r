# CODE TO CREATE THE FIGURES AND TABLES

# clear objects in workspace
rm(list=ls())
gc()

# load pacakges
library(ggplot2)
library(cowplot)
library(tidyverse)
library(raster)
library(RColorBrewer)
library(gridExtra)
library(pastecs)
library(lemon)
library(grid)
library(readr)
library(viridis)
library(hrbrthemes)
library(devtools)
if (!require("ggthemr")) devtools::install_github('cttobin/ggthemr'); library("ggthemr")

# load functions
source("functions.r")

# set up data frames for each set of data
Grid <- as_tibble(expand.grid(LandAvail = c(1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 30, 40, 50, 60, 70, 80, 90, 100), Replicate = 1:10, DevScenario = c(100, 75, 50), OffType = c("proponent", "financialhab", "financialkoala"))) %>% mutate(NoOffCore = 0, OffCore = 0, NoOffNonCore = 0, OffNonCore = 0, NoOffTotal = 0, OffTotal = 0, NoOffK = 0, OffK = 0, OffvsNoOffCore = 0, OffvsNoOffNonCore = 0, OffvsNoOffTotal = 0, OffvsNoOffK = 0, OffCost = 0, CostEffTotal = 0, CostEffKoala = 0)

# get data for 100% consolidation scenario

Dev <- 100

# loop through replicates and land availability
for (j in 1:10) {
  NoOff <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  NoOffCore <- ((NoOff$AggHabCore[length(NoOff$AggHabCore)] / NoOff$AggHabCore[1]) - 1) * 100
  NoOffNonCore <- ((NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)] / NoOff$AggHabNonCore[1]) - 1) * 100
  NoOffTotal <- (((NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) / (NoOff$AggHabCore[1] + NoOff$AggHabNonCore[1])) - 1) * 100
  NoOffK <- ((NoOff$AggK[length(NoOff$AggK)] / NoOff$AggK[1]) - 1) * 100

  for (i in c(1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    # proponent-driven offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_proponent_percavail_", i, ".rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffK"] = OffvsNoOffK

    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffKoala"] = CostEffKoala

    # financial habitat offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialhab_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffKoala"] = CostEffKoala

    # financial koala offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialkoala_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffKoala"] = CostEffKoala
  }
}

# get data for 75% consolidation scenario

Dev <- 75

# loop through replicates and land availability
for (j in 1:10) {
  NoOff <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  NoOffCore <- ((NoOff$AggHabCore[length(NoOff$AggHabCore)] / NoOff$AggHabCore[1]) - 1) * 100
  NoOffNonCore <- ((NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)] / NoOff$AggHabNonCore[1]) - 1) * 100
  NoOffTotal <- (((NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) / (NoOff$AggHabCore[1] + NoOff$AggHabNonCore[1])) - 1) * 100
  NoOffK <- ((NoOff$AggK[length(NoOff$AggK)] / NoOff$AggK[1]) - 1) * 100

  for (i in c(1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    # proponent-driven offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_proponent_percavail_", i, ".rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffK"] = OffvsNoOffK

    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffKoala"] = CostEffKoala

    # financial habitat offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialhab_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffKoala"] = CostEffKoala

    # financial koala offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialkoala_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffKoala"] = CostEffKoala
  }
}

# get data for 50% consolidation scenario

Dev <- 50

# loop through replicates and land availability
for (j in 1:10) {
  NoOff <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  NoOffCore <- ((NoOff$AggHabCore[length(NoOff$AggHabCore)] / NoOff$AggHabCore[1]) - 1) * 100
  NoOffNonCore <- ((NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)] / NoOff$AggHabNonCore[1]) - 1) * 100
  NoOffTotal <- (((NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) / (NoOff$AggHabCore[1] + NoOff$AggHabNonCore[1])) - 1) * 100
  NoOffK <- ((NoOff$AggK[length(NoOff$AggK)] / NoOff$AggK[1]) - 1) * 100

  for (i in c(1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    # proponent-driven offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_proponent_percavail_", i, ".rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffvsNoOffK"] = OffvsNoOffK

    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "proponent", "CostEffKoala"] = CostEffKoala

    # financial habitat offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialhab_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialhab", "CostEffKoala"] = CostEffKoala

    # financial koala offsets
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffCore"] = NoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffNonCore"] = NoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffTotal"] = NoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "NoOffK"] = NoOffK
    Off <- read_rds(paste("sim_results/rep_", j, "_reg_TRUE_consol_", Dev, "_offtype_financialkoala_percavail_", i, "_npslavail_TRUE.rds", sep = ""))
    OffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / Off$AggHabCore[1]) - 1) * 100
    OffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / Off$AggHabNonCore[1]) - 1) * 100
    OffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (Off$AggHabCore[1] + Off$AggHabNonCore[1])) - 1) * 100
    OffK <- ((Off$AggK[length(Off$AggK)] / Off$AggK[1]) - 1) * 100
    OffvsNoOffCore <- ((Off$AggHabCore[length(Off$AggHabCore)] / NoOff$AggHabCore[length(NoOff$AggHabCore)]) - 1) * 100
    OffvsNoOffNonCore <- ((Off$AggHabNonCore[length(Off$AggHabNonCore)] / NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)]) - 1) * 100
    OffvsNoOffTotal <- (((Off$AggHabCore[length(Off$AggHabCore)] + Off$AggHabNonCore[length(Off$AggHabNonCore)]) / (NoOff$AggHabCore[length(NoOff$AggHabCore)] + NoOff$AggHabNonCore[length(NoOff$AggHabNonCore)])) - 1) * 100
    OffvsNoOffK <- ((Off$AggK[length(Off$AggK)] / NoOff$AggK[length(NoOff$AggK)]) - 1) * 100
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCore"] = OffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffNonCore"] = OffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffTotal"] = OffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffK"] = OffK
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffCore"] = OffvsNoOffCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffNonCore"] = OffvsNoOffNonCore
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffTotal"] = OffvsNoOffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffvsNoOffK"] = OffvsNoOffK
    OffCost <- Off$AggOffCost[length(Off$AggOffCost)] / 1000000 # to convert to millions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "OffCost"] = OffCost
    CostEffTotal <- OffTotal / (OffCost / 1000) # to convert to billions of dollars
    CostEffKoala <- OffK / (OffCost / 1000) # to convert to billions of dollars
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffTotal"] = CostEffTotal
    Grid[Grid[,"LandAvail"] == i & Grid[,"Replicate"] == j & Grid[,"DevScenario"] == Dev & Grid[, "OffType"] == "financialkoala", "CostEffKoala"] = CostEffKoala
  }
}

saveRDS(Grid, "figures/sims_grid.rds")

# load grid of results if necessary
#Grid <- read_rds("figures/sims_grid.rds")

Summary_Mean <- Grid %>% group_by(LandAvail, DevScenario, OffType) %>% summarise(NoOffCore = mean(NoOffCore, na.rm = TRUE), OffCore = mean(OffCore, na.rm = TRUE), NoOffNonCore = mean(NoOffNonCore, na.rm = TRUE), OffNonCore = mean(OffNonCore, na.rm = TRUE), NoOffTotal = mean(NoOffTotal, na.rm = TRUE), OffTotal = mean(OffTotal, na.rm = TRUE), NoOffK = mean(NoOffK, na.rm = TRUE), OffK = mean(OffK, na.rm = TRUE), OffvsNoOffCore = mean(OffvsNoOffCore, na.rm = TRUE), OffvsNoOffNonCore = mean(OffvsNoOffNonCore, na.rm = TRUE), OffvsNoOffTotal = mean(OffvsNoOffTotal, na.rm = TRUE), OffvsNoOffK = mean(OffvsNoOffK, na.rm = TRUE), OffCost = mean(OffCost, na.rm = TRUE), CostEffTotal = mean(CostEffTotal, na.rm = TRUE), CostEffKoala = mean(CostEffKoala, na.rm = TRUE))

Summary_Mean_NoOff <- Grid %>% group_by(DevScenario) %>% summarise(NoOffCore = mean(NoOffCore, na.rm = TRUE), NoOffNonCore = mean(NoOffNonCore, na.rm = TRUE), NoOffTotal = mean(NoOffTotal, na.rm = TRUE), NoOffK = mean(NoOffK, na.rm = TRUE))

# set up map figures data

# habitat loss maps

HabNonCore <- raster("input/maps/habnc.asc")
# habitat loss - no offsets - 100% consolidation
Dev <- 100
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  } else {
    Res <- Res + Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  }
}
HabLossNoOff100 <- Res / 10
writeRaster(HabLossNoOff100, "figures/maps/habloss_no_off_100.asc", format = "ascii", overwrite=TRUE)

# habitat loss - no offsets - 75% consolidation
Dev <- 75
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  } else {
    Res <- Res + Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  }
}
HabLossNoOff75 <- Res / 10
writeRaster(HabLossNoOff100, "figures/maps/habloss_no_off_75.asc", format = "ascii", overwrite=TRUE)

# habitat loss - no offsets - 50% consolidation
Dev <- 50
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  } else {
    Res <- Res + Off$HabCore[[length(Off$HabCore)]] + Off$HabNonCore[[length(Off$HabNonCore)]] - Off$HabCore[[1]] - HabNonCore
  }
}
HabLossNoOff50 <- Res / 10
writeRaster(HabLossNoOff50, "figures/maps/habloss_no_off_50.asc", format = "ascii", overwrite=TRUE)

# offset site maps

# offsets - proponent - 1% availability - 100% consolidation
Dev <- 100
j <- 1
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_proponent_percavail_", j, ".rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsProponent1Avail100 <- Res / 10
writeRaster(OffsetsProponent1Avail100, "figures/maps/offsets_proponent_1avail_100.asc", format = "ascii", overwrite=TRUE)

# offsets - proponent - 100% availability - 100% consolidation
Dev <- 100
j <- 100
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_proponent_percavail_", j, ".rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsProponent100Avail100 <- Res / 10
writeRaster(OffsetsProponent100Avail100, "figures/maps/offsets_proponent_100avail_100.asc", format = "ascii", overwrite=TRUE)

# offsets - financial habitat - 1% availability - 100% consolidation
Dev <- 100
j <- 1
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_financialhab_percavail_", j, "_npslavail_TRUE.rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsFinHab1Avail100 <- Res / 10
writeRaster(OffsetsFinHab1Avail100, "figures/maps/offsets_finhab_1avail_100.asc", format = "ascii", overwrite=TRUE)

# offsets - financial habitat - 100% availability - 100% consolidation
Dev <- 100
j <- 100
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_financialhab_percavail_", j, "_npslavail_TRUE.rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsFinHab100Avail100 <- Res / 10
writeRaster(OffsetsFinHab100Avail100, "figures/maps/offsets_finhab_100avail_100.asc", format = "ascii", overwrite=TRUE)

# offsets - financial koala - 1% availability - 100% consolidation
Dev <- 100
j <- 1
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_financialkoala_percavail_", j, "_npslavail_TRUE.rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsFinKoala1Avail100 <- Res / 10
writeRaster(OffsetsFinKoala1Avail100, "figures/maps/offsets_finkoala_1avail_100.asc", format = "ascii", overwrite=TRUE)

# offsets - financial koala - 100% availability - 100% consolidation
Dev <- 100
j <- 100
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_financialkoala_percavail_", j, "_npslavail_TRUE.rds", sep = ""))
  if (i == 1) {
    Res <- Off$OffsetSites[[length(Off$OffsetSites)]]
  } else {
    Res <- Res + Off$OffsetSites[[length(Off$OffsetSites)]]
  }
}
OffsetsFinKoala100Avail100 <- Res / 10
writeRaster(OffsetsFinKoala100Avail100, "figures/maps/offsets_finkoala_100avail_100.asc", format = "ascii", overwrite=TRUE)

# create figures

# create data for graphs
GraphData <- Summary_Mean %>% dplyr::select(DevScenario, LandAvail, OffType, OffCore, OffNonCore, OffTotal, OffK, OffCost, CostEffTotal, CostEffKoala) %>% mutate(DevScenario = as.factor(DevScenario))
GraphDataNoOff <- Summary_Mean_NoOff %>% dplyr::select(DevScenario, NoOffCore, NoOffNonCore, NoOffTotal, NoOffK) %>% mutate(DevScenario = as.factor(DevScenario))

# Figure 1 - study area map - figure created in ArcGIS -see ArcGIS Pro project "/figures/maps/maps.aprx"

# Figure 2 - overview of model - figure created in Powerpoint -see Powerpoint file "/figures/maps/figures.pptx"

# Figure 3 - habitat loss when there are no offsets
# generating bar plot of habitat loss - final figure with maps created in ArcGIS - see ArcGIS Pro project "/figures/maps/maps.aprx"

ggthemr("flat")

GraphDataNoOff %>% mutate(DevScenario = recode_factor(DevScenario, "50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")) %>% dplyr::select(-NoOffTotal) %>% pivot_longer(!DevScenario, names_to = "Indicator", values_to = "Change") %>% mutate(Indicator = as_factor(Indicator)) %>% ggplot(aes(fill = Indicator, y = Change, x = DevScenario)) + geom_bar(position = "dodge", stat = "identity") + scale_fill_discrete(labels = c("Core habitat area", "Non-core habitat area", "Koala abundance")) + ylab("Percent change") + xlab("Development scenario") + ggtitle("Habitat Area and Koala Abundance Change with no Offsets") + theme(plot.title = element_text(hjust = 0.5, size = 22), legend.position = "bottom", axis.text = element_text(size = 22), axis.title = element_text(size = 22), legend.text = element_text(size = 22), legend.title=element_blank()) + labs(fill = "Indicator:")

ggsave("figures/fig3_graph.jpg", dpi = 300, height = 10, width = 10)

# Figure 4 - offset habitat total

ggthemr("flat")

new_labels <- c("50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")

GraphData %>% ggplot(aes(x = LandAvail, y = OffTotal, group = OffType, color = OffType)) + facet_grid(. ~ DevScenario, labeller = labeller(DevScenario = new_labels)) + geom_point() + geom_line() + ylab("Percent change in total habitat area") + xlab("Percent offset site availability") + ggtitle("Habitat Area Change") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + labs(color = "Offset mechanism:") + scale_color_discrete(labels = c("Developer-led", "Agency-led (habitat)", "Agency-led (abundance)"))

ggsave("figures/fig4.jpg", dpi = 300)

# Figure 5 - offset koalas

ggthemr("flat")

new_labels <- c("50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")

GraphData %>% ggplot(aes(x = LandAvail, y = OffK, group = OffType, color = OffType)) + facet_grid(. ~ DevScenario, labeller = labeller(DevScenario = new_labels)) + geom_point() + geom_line() + ylab("Percent change in koala abundance") + xlab("Percent offset site availability") + ggtitle("Koala Abundance Change") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + labs(color = "Offset mechanism:") + scale_color_discrete(labels = c("Developer-led", "Agency-led (habitat)", "Agency-led (abundance)"))

ggsave("figures/fig5.jpg", dpi = 300)

# Figure 6 - offset cost

ggthemr("flat")

new_labels <- c("50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")

GraphData %>% ggplot(aes(x = LandAvail, y = OffCost, group = OffType, color = OffType)) + facet_grid(. ~ DevScenario, labeller = labeller(DevScenario = new_labels)) + geom_point() + geom_line() + ylab("Total offset costs ($ millions)") + xlab("Percent offset site availability") + ggtitle("Offset Costs") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + labs(color = "Offset mechanism:") + scale_color_discrete(labels = c("Developer-led", "Agency-led (habitat)", "Agency-led (abundance)"))

ggsave("figures/fig6.jpg", dpi = 300)

# Figure S1 - potential koala density - figure created in ArcGIS - see ArcGIS Pro project "/figures/maps/maps.aprx"

# Figure S2 - rules for the models of regulation and offset requirements - figure created in Powerpoint -see Powerpoint file "/figures/maps/figures.pptx"

# Figure S3 - offset habitat core

ggthemr("flat")

new_labels <- c("50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")

GraphData %>% ggplot(aes(x = LandAvail, y = OffCore, group = OffType, color = OffType)) + facet_grid(. ~ DevScenario, labeller = labeller(DevScenario = new_labels)) + geom_point() + geom_line() + ylab("Percent change in core habitat area") + xlab("Percent offset site availability") + ggtitle("Core Habitat Area Change") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + labs(color = "Offset mechanism:") + scale_color_discrete(labels = c("Developer-led", "Agency-led (habitat)", "Agency-led (abundance)"))

ggsave("figures/figs3.jpg", dpi = 300)

# Figure S4 - offset habitat non-core

ggthemr("flat")

new_labels <- c("50" = "High expansion", "75" = "Medium expansion", "100" = "Low expansion")

GraphData %>% ggplot(aes(x = LandAvail, y = OffNonCore, group = OffType, color = OffType)) + facet_grid(. ~ DevScenario, labeller = labeller(DevScenario = new_labels)) + geom_point() + geom_line() + ylab("Percent change in non-core habitat area") + xlab("Percent offset site availability") + ggtitle("Non-core Habitat Area Change") + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + labs(color = "Offset mechanism:") + scale_color_discrete(labels = c("Developer-led", "Agency-led (habitat)", "Agency-led (abundance)"))

ggsave("figures/figs4.jpg", dpi = 300)

# Figure S5 - offset locations for proponent driven offsets for 1% offsets site availability and 100% offset site availability (and the average cost and koala density)
# final figure with maps created in ArcGIS - see ArcGIS Pro project "/figures/maps/maps.aprx"

# get the data
lgasfact <- raster("input/maps/lgas.asc")
LVChangetb <- read.csv("input/tables/landval_change.csv", header = TRUE)
lu2016 <- raster("input/maps/landuse16reclsuburb4.asc")
LandVal <- raster("input/maps/landval.asc")
LandVal <- to_raster((as.data.frame(LandVal)$layer * (as.data.frame(lgasfact) %>% left_join(LVChangetb, by = c("layer" = "LGA")))$Change) + 1, lu2016)
KDen <- raster("input/maps/denmod6f.asc")
LandValdf <- as_tibble((as.data.frame(LandVal) + (20000 * 1.25)) / 1000000) # add the restoration costs
KDendf <- as_tibble(as.data.frame(KDen))
OffsetsProponent1Avail100df <- as_tibble(as.data.frame(OffsetsProponent1Avail100))
OffsetsProponent100Avail100df <- as_tibble(as.data.frame(OffsetsProponent100Avail100))
LandValOff1Availdf <- as_tibble(LandValdf * OffsetsProponent1Avail100df)
LandValOff100Availdf <- as_tibble(LandValdf * OffsetsProponent100Avail100df)
KDenOff1Availdf <- as_tibble(KDendf * OffsetsProponent1Avail100df)
KDenOff100Availdf <- as_tibble(KDendf * OffsetsProponent100Avail100df)

CostDensity <- as_tibble(expand.grid(LandAvail = c(1, 100), Indicator = c("Mean cost ($ millions)", "Mean koala density (koalas per hectare)")) %>% mutate(Value = 0))

CostDensity[CostDensity[,"LandAvail"] == 1 & CostDensity[, "Indicator"] == "Mean cost ($ millions)", "Value"] <- sum(LandValOff1Availdf$landval, na.rm = TRUE) / sum(OffsetsProponent1Avail100df$layer, na.rm = TRUE)
CostDensity[CostDensity[,"LandAvail"] == 100 & CostDensity[, "Indicator"] == "Mean cost ($ millions)", "Value"] <- sum(LandValOff100Availdf$landval, na.rm = TRUE) / sum(OffsetsProponent100Avail100df$layer, na.rm = TRUE)
CostDensity[CostDensity[,"LandAvail"] == 1 & CostDensity[, "Indicator"] == "Mean koala density (koalas per hectare)", "Value"] <- sum(KDenOff1Availdf$denmod6f, na.rm = TRUE) / sum(OffsetsProponent1Avail100df$layer, na.rm = TRUE)
CostDensity[CostDensity[,"LandAvail"] == 100 & CostDensity[, "Indicator"] == "Mean koala density (koalas per hectare)", "Value"] <- sum(KDenOff100Availdf$denmod6f, na.rm = TRUE) / sum(OffsetsProponent100Avail100df$layer, na.rm = TRUE)

CostDensity <- CostDensity %>% mutate(LandAvail = as_factor(LandAvail))

#ggthemr("flat")

p1 <- CostDensity %>% filter(Indicator == "Mean cost ($ millions)") %>% ggplot(aes(y = Value, x = LandAvail)) + geom_bar(position = "dodge", stat = "identity") + ylab("Mean offset costs in offset sites ($ millions per hectare)") + xlab("Percent offset site availability") + ggtitle("Offset Costs in Offset Sites") + theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 11))
p2 <- CostDensity %>% filter(Indicator == "Mean koala density (koalas per hectare)") %>% ggplot(aes(y = Value, x = LandAvail)) + geom_bar(position = "dodge", stat = "identity") + ylab("Mean potential koala density in offset sites (koalas per hectare)") + xlab("Percent offset site availability") + ggtitle("Koala Density in Offset Sites") + theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 11))
dev.new(width = 10, height = 15)
p3 <- grid.arrange(p1, p2, nrow = 2)

ggsave("figures/figs5_graph.jpg", p3, dpi = 300)

# Table S1 - definition of land-use classes

# Table S2 - predictor variables used in the land-use change model

# Table S3 -land-uses permitted within each planning scheme zone

# Table S4 - accuracy assessment - see Excel spreadsheet "/figures/tables4.xlsx"

# Table S5 - the simulated new dwellings versus targets for each development scenario

# dwelling growth - no offsets - 100% consolidation
Dev <- 100
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  } else {
    Res <- Res + Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  }
}
DwellGrowthNoOff100 <- as_tibble(Res / 10)
names(DwellGrowthNoOff100) <- c("DwellGrowth100", "Target100")

# dwelling growth - no offsets - 75% consolidation
Dev <- 75
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  } else {
    Res <- Res + Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  }
}
DwellGrowthNoOff75 <- as_tibble(Res / 10)
names(DwellGrowthNoOff75) <- c("DwellGrowth75", "Target75")

# dwelling growth - no offsets - 50% consolidation
Dev <- 50
for (i in 1:10) {
  Off <- read_rds(paste("sim_results/rep_", i, "_reg_TRUE_consol_", Dev, "_offtype_none.rds", sep = ""))
  if (i == 1) {
    Res <- Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  } else {
    Res <- Res + Off$DwellGrowthLGAExistUrb %>% dplyr::select(tail(names(.), 2))
  }
}
DwellGrowthNoOff50 <- as_tibble(Res / 10)
names(DwellGrowthNoOff50) <- c("DwellGrowth50", "Target50")

LGAs <- as_tibble(c("Moreton Bay", "Moreton Bay", "Noosa", "Noosa", "Redland", "Redland", "Sunshine Coast", "Sunshine Coast", "Brisbane", "Brisbane", "Gold Coast", "Gold Coast", "Ipswich", "Ipswich", "Logan", "Logan"))
names(LGAs) <- c("LGA")
ConsExp <- as_tibble(c("Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation", "Expansion", "Consolidation"))
names(ConsExp) <- c("ConsExp")
DwellvsTargets <- bind_cols(as_tibble(LGAs), as_tibble(ConsExp), DwellGrowthNoOff100, DwellGrowthNoOff75, DwellGrowthNoOff50)

write_csv(DwellvsTargets, "figures/tables5.csv")
