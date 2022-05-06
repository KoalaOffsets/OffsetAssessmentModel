# CODE TO FIT THE CLEARING MODEL

# clear objects in workspace
rm(list=ls())
gc()

# load packages
library(foreign)
library(data.table)
library(tidyverse)

# load data
ClearData <- read.dbf("input/clearing/forest_cleared_grid.dbf")

# convert to factors
ClearData$LU99fact <- as.factor(ClearData$LU99)
ClearData$LU16fact <- as.factor(ClearData$LU16)

# calculate number not cleared
ClearData$NOCLEAR <- ClearData$WOODY - ClearData$CLEAR

# loop through 1999 land uses, fit models and create predictions

Models <- list()
ClearPred <- as.data.frame(matrix(0, nrow = 13, ncol = 14))
names(ClearPred) <- c("lucurr", "10", "21", "22", "23", "30", "40", "51", "52", "53", "60", "71", "72", "80")
ClearPred$lucurr <- c(10, 21, 22, 23, 30, 40, 51, 52, 53, 60, 71, 72, 80)

for (i in levels(ClearData$LU99fact)) {
  NewData <- ClearData[which(ClearData$LU99fact == i), ]
  Model <- glm(cbind(CLEAR, NOCLEAR) ~ LU16fact, data = NewData, family = "binomial")
  Preds <- predict.glm(object = Model, newdata = Model$xlevels, type = "response")
  names(Preds) <- Model$xlevels$LU16fact
  ClearPred[which(levels(ClearData$LU99fact) == i), intersect(names(ClearPred), names(Preds))] <- Preds
  Models[[i]] <- Model
}

# save predictions
save(Models, file = "input/clearing/models/clearing_models.Rda")
save(ClearPred, file = "input/clearing/models/clearing_predictions.Rda")
