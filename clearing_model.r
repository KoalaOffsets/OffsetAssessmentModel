# required packages
library(foreign)
library(data.table)
library(tidyverse)

rm(list=ls())
if(!is.null(dev.list())) dev.off()

## Set working directory
#setwd("XXX")

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

# plot predictions
ggplot(Predictions, aes(x = LU16, y = PROB)) + geom_bar(stat="identity", colour = "red", fill = "red") + ylab("Estimated proportion woody vegetation cleared") + xlab("Land-use in 2016") + theme_bw() + theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1), axis.text.y = element_text(size = 10)) + scale_y_continuous(breaks=c(0, 1)) + facet_wrap(LU99 ~ ., labeller = label_value, scale = "fixed")
