# Set working directory (***CHANGE TO PATH WHERE DATA ARE***)
setwd("R:/KOALA2018-A0206/model/CA-KoalaOffset")
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# get required packages
if (!require("tidyverse")) devtools::install_github("tidyverse/tidyverse"); library("tidyverse")
if (!require("raster")) install.packages("raster"); library("raster")
if (!require("nnet")) install.packages("nnet"); library("nnet")
if (!require("grDevices")) install.packages("grDevices"); library("grDevices")
if (!require("diffeR")) install.packages("diffeR"); library("diffeR")
if (!require("snowfall")) install.packages("snowfall"); library("snowfall")
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")
if (!require("vcd")) install.packages("vcd"); library("vcd")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("moveVis")) install.packages("moveVis"); library("moveVis")
if (!require("rasterVis")) install.packages("rasterVis"); library("rasterVis")
if (!require("animation")) install.packages("animation"); library("animation")
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("pastecs")) install.packages("pastecs"); library("pastecs")

# GET DWELLING GRWOTH GIFS

# SCENARIO 1 - no regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "none" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$DwellNum
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.5, main = "", col = my.palette))

# SCENARIO 2 - previous regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$DwellNum
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$DwellNum
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# GET HABITAT LOSS GIFS

# SCENARIO 2 - previous regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$HabCore
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[1]] - Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[1]] - Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$HabCore
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[1]] - Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[1]] - Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# GET OFFSET GIFS

# SCENARIO 3 - previous regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$OffsetSites
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# SCENARIO 6 - current regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$OffsetSites
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# SCENARIO 4 - previous regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$OffsetSites
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# SCENARIO 7 - current regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$OffsetSites
  List <- list()
  for (j in 1:length(Growth))
  {
    if (j == 1) {
      Stack <- Growth[[j]]
    } else {
      Stack <- addLayer(Stack, Growth[[j]])
    }
  }
  Scenario[[i]] <- Stack
}
my.palette <- brewer.pal(n = 9, name = "OrRd")
saveGIF(animate(Scenario[[1]], pause = 0.1, main = "", col = my.palette))

# GRAPHS OF HABITAT LOSS UNDER CURRENT REGULATION - NO OFFSETS
# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$HabCore

  Hab <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))

  Scenario[[i]] <- Hab
}
for (i  in 1:20)
{
  Data <- cbind(as.data.frame(Scenario[[i]]) * 100, as.data.frame(seq(2017, 2031, length.out = dim(Scenario[[i]])[1])))
  names(Data) <- c("Hab", "Time")
  if (i == 1) {
    p <- ggplot(data = Data, aes(x = Time, y = Hab)) + geom_line() + labs(x = "Year") + labs(y = "Loss of Core Habitat (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2))
  } else {
    p <- p + geom_line(data = Data, aes(x = Time, y = Hab))
  }
}

# GRAPH OF PREVIOUS VERSUS CURRENT - NO OFFSETS

# SCENARIO 2 - previous regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario2 <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$HabCore

  Hab <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  Scenario2[[i]] <- Hab
}

# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Scenario5 <- list()
for (i in 1:20) {
  Growth <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))$HabCore

  Hab <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  Scenario5[[i]] <- Hab
}

# plots
Result2 <- matrix(NA, nrow = 15, ncol = 20)
Result5 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data2 <- cbind(as.data.frame(Scenario2[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Scenario2[[i]])[1]))))
  names(Data2) <- c("Hab", "Time")
  Reg2 <- regul(x = Data2$Time, y = Data2$Hab, units = "years", frequency = 1, n = 15)
  Data5 <- cbind(as.data.frame(Scenario5[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Scenario5[[i]])[1]))))
  names(Data5) <- c("Hab", "Time")
  Reg5 <- regul(x = Data5$Time, y = Data5$Hab, units = "years", frequency = 1, n = 15)

  Result2[,i] <- as.matrix(extract(Reg2))
  Result5[,i] <- as.matrix(extract(Reg5))
}
Time <- c(seq(2017,2031,1))
PlotResult <- data.frame(Time = c(seq(2017,2031,1)), Hab2 = rowMeans(Result2), Hab5 = rowMeans(Result5))

PlotResult %>% gather(Scenario, CoreHab, Hab2, Hab5) %>% ggplot(aes(x=Time, y=CoreHab, colour=Scenario)) + geom_line(size = 2) + labs(y = "Loss of Core Habitat (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2)) + scale_color_hue(labels = c("Previous", "Current"))


# PLOT GRAPHS OF OFFSET EFFECTIVENESS FOR CURRENT AND PREVIOUS REGULATION

# SCENARIO 2 - previous regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab2 <- list()
KNum2 <- list()
Cost2 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab2[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum2[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost2[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# SCENARIO 3 - previous regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab3 <- list()
KNum3 <- list()
Cost3 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab3[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum3[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost3[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# SCENARIO 4 - previous regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "previous" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab4 <- list()
KNum4 <- list()
Cost4 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab4[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum4[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost4[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# SCENARIO 5 - current regulation and no offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "none" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab5 <- list()
KNum5 <- list()
Cost5 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab5[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum5[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost5[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# SCENARIO 6 - current regulation and lga offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "lga" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab6 <- list()
KNum6 <- list()
Cost6 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab6[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum6[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost6[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# SCENARIO 7 - current regulation and anywhere offsets
MaxIter <- 1000
RepSteps <- 1 # how often (how many iterations) to report and record outputs
Reg <- "current" # "none", or "previous", or "current" regulation ("none" means no regulation including planning schemes and offsets")
OffRule <- "anywhere" # "none", within "lga" or "anywhere" spatial rule ("none" means no offsets)
Multiplier <- 3 # the multiplier applied
RestSucc <- 0.9 # probability that restoration succeeds
Horizon <- 2031 # time horizon relative Shaping SEQ - determines the dwelling demand - 2031, 2041, or Inf
Hab7 <- list()
KNum7 <- list()
Cost7 <- list()
for (i in 1:20) {
  Data <- readRDS(paste("offset_sim_results/", "REP", i, "_reg_", Reg, "_off_", OffRule, "_mult_", Multiplier, "_rsuc_", RestSucc, "_hor_", Horizon, "_maxit_", MaxIter, "_repsteps_", RepSteps, ".rds", sep = ""))
  Growth <- Data$HabCore
  KNum <- Data$KNum
  Cost <- Data$OffCost
  Hab7[[i]] <- as.data.frame(unlist(lapply(Growth, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(Growth[[1]]), na.rm = TRUE)) / sum(as.data.frame(Growth[[1]]), na.rm = TRUE)})))
  KNum7[[i]] <-as.data.frame(unlist(lapply(KNum, FUN = function(x) {(sum(as.data.frame(x), na.rm = TRUE) - sum(as.data.frame(KNum[[1]]), na.rm = TRUE)) / sum(as.data.frame(KNum[[1]]), na.rm = TRUE)})))
  Cost7[[i]] <- as.data.frame(unlist(lapply(Cost, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)})))
}

# CORE HABITAT

# effectiveness previous regulation
Result2 <- matrix(NA, nrow = 15, ncol = 20)
Result3 <- matrix(NA, nrow = 15, ncol = 20)
Result4 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data2 <- cbind(as.data.frame(Hab2[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab2[[i]])[1]))))
  names(Data2) <- c("Hab", "Time")
  Reg2 <- regul(x = Data2$Time, y = Data2$Hab, units = "years", frequency = 1, n = 15)
  Data3 <- cbind(as.data.frame(Hab3[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab3[[i]])[1]))))
  names(Data3) <- c("Hab", "Time")
  Reg3 <- regul(x = Data3$Time, y = Data3$Hab, units = "years", frequency = 1, n = 15)
  Data4 <- cbind(as.data.frame(Hab4[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab4[[i]])[1]))))
  names(Data4) <- c("Hab", "Time")
  Reg4 <- regul(x = Data4$Time, y = Data4$Hab, units = "years", frequency = 1, n = 15)
  Result2[,i] <- as.matrix(extract(Reg2))
  Result3[,i] <- as.matrix(extract(Reg3))
  Result4[,i] <- as.matrix(extract(Reg4))

}
Time <- c(seq(2017,2031,1))
PlotResult <- data.frame(Time = c(seq(2017,2031,1)), Hab2 = rowMeans(Result2), Hab3 = rowMeans(Result3), Hab4 = rowMeans(Result4))

PlotResult %>% gather(Scenario, CoreHab, Hab2, Hab3, Hab4) %>% ggplot(aes(x=Time, y=CoreHab, colour=Scenario)) + geom_line(size = 2) + labs(y = "Loss of Core Habitat (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2)) + scale_color_hue(labels = c("No Offsets", "in LGA", "Anywhere"))

# effectiveness current regulation
Result5 <- matrix(NA, nrow = 15, ncol = 20)
Result6 <- matrix(NA, nrow = 15, ncol = 20)
Result7 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data5 <- cbind(as.data.frame(Hab5[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab5[[i]])[1]))))
  names(Data5) <- c("Hab", "Time")
  Reg5 <- regul(x = Data5$Time, y = Data5$Hab, units = "years", frequency = 1, n = 15)
  Data6 <- cbind(as.data.frame(Hab6[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab6[[i]])[1]))))
  names(Data6) <- c("Hab", "Time")
  Reg6 <- regul(x = Data6$Time, y = Data6$Hab, units = "years", frequency = 1, n = 15)
  Data7 <- cbind(as.data.frame(Hab7[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab7[[i]])[1]))))
  names(Data7) <- c("Hab", "Time")
  Reg7 <- regul(x = Data7$Time, y = Data7$Hab, units = "years", frequency = 1, n = 15)
  Result5[,i] <- as.matrix(extract(Reg5))
  Result6[,i] <- as.matrix(extract(Reg6))
  Result7[,i] <- as.matrix(extract(Reg7))

}
Time <- c(seq(2017,2031,1))
PlotResult <- data.frame(Time = c(seq(2017,2031,1)), Hab5 = rowMeans(Result5), Hab6 = rowMeans(Result6), Hab7 = rowMeans(Result7))

PlotResult %>% gather(Scenario, CoreHab, Hab5, Hab6, Hab7) %>% ggplot(aes(x=Time, y=CoreHab, colour=Scenario)) + geom_line(size = 2) + labs(y = "Loss of Core Habitat (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2)) + scale_color_hue(labels = c("No Offsets", "in LGA", "Anywhere"))

#KOALA NUMBERS

# effectiveness previous regulation
Result2 <- matrix(NA, nrow = 15, ncol = 20)
Result3 <- matrix(NA, nrow = 15, ncol = 20)
Result4 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data2 <- cbind(as.data.frame(KNum2[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab2[[i]])[1]))))
  names(Data2) <- c("Hab", "Time")
  Reg2 <- regul(x = Data2$Time, y = Data2$Hab, units = "years", frequency = 1, n = 15)
  Data3 <- cbind(as.data.frame(KNum3[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab3[[i]])[1]))))
  names(Data3) <- c("Hab", "Time")
  Reg3 <- regul(x = Data3$Time, y = Data3$Hab, units = "years", frequency = 1, n = 15)
  Data4 <- cbind(as.data.frame(KNum4[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab4[[i]])[1]))))
  names(Data4) <- c("Hab", "Time")
  Reg4 <- regul(x = Data4$Time, y = Data4$Hab, units = "years", frequency = 1, n = 15)
  Result2[,i] <- as.matrix(extract(Reg2))
  Result3[,i] <- as.matrix(extract(Reg3))
  Result4[,i] <- as.matrix(extract(Reg4))

}
Time <- c(seq(2017,2031,1))
PlotResult <- data.frame(Time = c(seq(2017,2031,1)), Hab2 = rowMeans(Result2), Hab3 = rowMeans(Result3), Hab4 = rowMeans(Result4))

PlotResult %>% gather(Scenario, CoreHab, Hab2, Hab3, Hab4) %>% ggplot(aes(x=Time, y=CoreHab, colour=Scenario)) + geom_line(size = 2) + labs(y = "Loss of Koala Abundance (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2)) + scale_color_hue(labels = c("No Offsets", "in LGA", "Anywhere"))

# effectiveness current regulation
Result5 <- matrix(NA, nrow = 15, ncol = 20)
Result6 <- matrix(NA, nrow = 15, ncol = 20)
Result7 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data5 <- cbind(as.data.frame(KNum5[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab5[[i]])[1]))))
  names(Data5) <- c("Hab", "Time")
  Reg5 <- regul(x = Data5$Time, y = Data5$Hab, units = "years", frequency = 1, n = 15)
  Data6 <- cbind(as.data.frame(KNum6[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab6[[i]])[1]))))
  names(Data6) <- c("Hab", "Time")
  Reg6 <- regul(x = Data6$Time, y = Data6$Hab, units = "years", frequency = 1, n = 15)
  Data7 <- cbind(as.data.frame(KNum7[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab7[[i]])[1]))))
  names(Data7) <- c("Hab", "Time")
  Reg7 <- regul(x = Data7$Time, y = Data7$Hab, units = "years", frequency = 1, n = 15)
  Result5[,i] <- as.matrix(extract(Reg5))
  Result6[,i] <- as.matrix(extract(Reg6))
  Result7[,i] <- as.matrix(extract(Reg7))

}
Time <- c(seq(2017,2031,1))
PlotResult <- data.frame(Time = c(seq(2017,2031,1)), Hab5 = rowMeans(Result5), Hab6 = rowMeans(Result6), Hab7 = rowMeans(Result7))

PlotResult %>% gather(Scenario, CoreHab, Hab5, Hab6, Hab7) %>% ggplot(aes(x=Time, y=CoreHab, colour=Scenario)) + geom_line(size = 2) + labs(y = "Loss of Koala Abundance (%)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) + scale_x_continuous(breaks = seq(2017, 2031, 2)) + scale_color_hue(labels = c("No Offsets", "in LGA", "Anywhere"))

# COST VERSUS BENEFIT - HABITAT
# cost current regulation
Result5 <- matrix(NA, nrow = 15, ncol = 20)
Result6 <- matrix(NA, nrow = 15, ncol = 20)
Result7 <- matrix(NA, nrow = 15, ncol = 20)
ResultC5 <- matrix(NA, nrow = 15, ncol = 20)
ResultC6 <- matrix(NA, nrow = 15, ncol = 20)
ResultC7 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data5 <- cbind(as.data.frame(Hab5[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab5[[i]])[1]))))
  names(Data5) <- c("Hab", "Time")
  Reg5 <- regul(x = Data5$Time, y = Data5$Hab, units = "years", frequency = 1, n = 15)
  Data6 <- cbind(as.data.frame(Hab6[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab6[[i]])[1]))))
  names(Data6) <- c("Hab", "Time")
  Reg6 <- regul(x = Data6$Time, y = Data6$Hab, units = "years", frequency = 1, n = 15)
  Data7 <- cbind(as.data.frame(Hab7[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(Hab7[[i]])[1]))))
  names(Data7) <- c("Hab", "Time")
  Reg7 <- regul(x = Data7$Time, y = Data7$Hab, units = "years", frequency = 1, n = 15)
  Result5[,i] <- as.matrix(extract(Reg5))
  Result6[,i] <- as.matrix(extract(Reg6))
  Result7[,i] <- as.matrix(extract(Reg7))

  DataC5 <- cbind(as.data.frame(Cost5[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost5[[i]])[1]))))
  names(DataC5) <- c("Cost", "Time")
  RegC5 <- regul(x = DataC5$Time, y = DataC5$Cost, units = "years", frequency = 1, n = 15)
  DataC6 <- cbind(as.data.frame(Cost6[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost6[[i]])[1]))))
  names(DataC6) <- c("Cost", "Time")
  RegC6 <- regul(x = DataC6$Time, y = DataC6$Cost, units = "years", frequency = 1, n = 15)
  DataC7 <- cbind(as.data.frame(Cost7[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost7[[i]])[1]))))
  names(DataC7) <- c("Cost", "Time")
  RegC7 <- regul(x = DataC7$Time, y = DataC7$Cost, units = "years", frequency = 1, n = 15)
  ResultC5[,i] <- as.matrix(extract(RegC5))
  ResultC6[,i] <- as.matrix(extract(RegC6))
  ResultC7[,i] <- as.matrix(extract(RegC7))
}
Benefit6 <- rowMeans(Result6) - rowMeans(Result5)
Benefit7 <- rowMeans(Result7) - rowMeans(Result5)
Dollars6 <- rowMeans(ResultC6)
Dollars7 <- rowMeans(ResultC7)

PlotResult <- data.frame(Doll6 = (Dollars6 / 1000000), Doll7 = (Dollars7 / 1000000), Ben6 = Benefit6, Ben7 = Benefit7)

p <- ggplot(PlotResult, aes(x=Ben6, y=Doll6, colour="in LGA")) + geom_line(size = 2) + labs(x = "Benefit (% improvement in core habitat loss)") + labs(y = "Cost ($ million)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))

p + geom_line(aes(x=Ben7, y=Doll7, colour="Anywhere"), size = 2) + scale_color_manual(values=c("red", "blue")) + scale_linetype_discrete(name = "Scenario") + scale_color_discrete(name = "Scenario")

# COST VERSUS BENEFIT - KOALAS
# cost current regulation
Result5 <- matrix(NA, nrow = 15, ncol = 20)
Result6 <- matrix(NA, nrow = 15, ncol = 20)
Result7 <- matrix(NA, nrow = 15, ncol = 20)
ResultC5 <- matrix(NA, nrow = 15, ncol = 20)
ResultC6 <- matrix(NA, nrow = 15, ncol = 20)
ResultC7 <- matrix(NA, nrow = 15, ncol = 20)
for (i in 1:20) {
  Data5 <- cbind(as.data.frame(KNum5[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(KNum5[[i]])[1]))))
  names(Data5) <- c("Hab", "Time")
  Reg5 <- regul(x = Data5$Time, y = Data5$Hab, units = "years", frequency = 1, n = 15)
  Data6 <- cbind(as.data.frame(KNum6[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(KNum6[[i]])[1]))))
  names(Data6) <- c("Hab", "Time")
  Reg6 <- regul(x = Data6$Time, y = Data6$Hab, units = "years", frequency = 1, n = 15)
  Data7 <- cbind(as.data.frame(KNum7[[i]]) * 100, as.data.frame((seq(2017, 2031, length.out = dim(KNum7[[i]])[1]))))
  names(Data7) <- c("Hab", "Time")
  Reg7 <- regul(x = Data7$Time, y = Data7$Hab, units = "years", frequency = 1, n = 15)
  Result5[,i] <- as.matrix(extract(Reg5))
  Result6[,i] <- as.matrix(extract(Reg6))
  Result7[,i] <- as.matrix(extract(Reg7))

  DataC5 <- cbind(as.data.frame(Cost5[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost5[[i]])[1]))))
  names(DataC5) <- c("Cost", "Time")
  RegC5 <- regul(x = DataC5$Time, y = DataC5$Cost, units = "years", frequency = 1, n = 15)
  DataC6 <- cbind(as.data.frame(Cost6[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost6[[i]])[1]))))
  names(DataC6) <- c("Cost", "Time")
  RegC6 <- regul(x = DataC6$Time, y = DataC6$Cost, units = "years", frequency = 1, n = 15)
  DataC7 <- cbind(as.data.frame(Cost7[[i]]), as.data.frame((seq(2017, 2031, length.out = dim(Cost7[[i]])[1]))))
  names(DataC7) <- c("Cost", "Time")
  RegC7 <- regul(x = DataC7$Time, y = DataC7$Cost, units = "years", frequency = 1, n = 15)
  ResultC5[,i] <- as.matrix(extract(RegC5))
  ResultC6[,i] <- as.matrix(extract(RegC6))
  ResultC7[,i] <- as.matrix(extract(RegC7))
}
Benefit6 <- rowMeans(Result6) - rowMeans(Result5)
Benefit7 <- rowMeans(Result7) - rowMeans(Result5)
Dollars6 <- rowMeans(ResultC6)
Dollars7 <- rowMeans(ResultC7)

PlotResult <- data.frame(Doll6 = (Dollars6 / 1000000), Doll7 = (Dollars7 / 1000000), Ben6 = Benefit6, Ben7 = Benefit7)

p <- ggplot(PlotResult, aes(x=Ben6, y=Doll6, colour="in LGA")) + geom_line(size = 2) + labs(x = "Benefit (% improvement in decline in koala abundance)") + labs(y = "Cost ($ million)") + theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))

p + geom_line(aes(x=Ben7, y=Doll7, colour="Anywhere"), size = 2) + scale_color_manual(values=c("red", "blue")) + scale_linetype_discrete(name = "Scenario") + scale_color_discrete(name = "Scenario")
