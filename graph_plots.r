library(ggplot2)
library(cowplot)

DwellingGrowth <- unlist(lapply(DwellGrowthList, FUN = function(x) {sum(as.data.frame(x), na.rm = TRUE)}))
HabCoreLoss <- unlist(lapply(HabCoreList, FUN = function(x) {sum(as.data.frame((x - HabCoreList[[1]])), na.rm = TRUE)  / sum(as.data.frame(HabCoreList[[1]]), na.rm = TRUE)}))
HabNonCoreLoss <- unlist(lapply(HabNonCoreList, FUN = function(x) {sum(as.data.frame((x - HabNonCoreList[[1]])), na.rm = TRUE) / sum(as.data.frame(HabNonCoreList[[1]]), na.rm = TRUE)}))
KNumLoss <- unlist(lapply(KNumList, FUN = function(x) {sum(as.data.frame((x - KNumList[[1]])), na.rm = TRUE) / sum(as.data.frame(KNumList[[1]]), na.rm = TRUE)}))

DwellPlot <- ggplot() + geom_bar(mapping = aes(x = 1:length(DwellGrowthList), y = DwellingGrowth/1000), stat = "identity", fill = "grey") + ggtitle("New Dwellings") + labs(y="New dwellings (thousands)", x = "Simulation iterations") + theme(plot.title = element_text(hjust = 0.5))

CorePlot <- ggplot() + geom_bar(mapping = aes(x = 1:length(DwellGrowthList), y = HabCoreLoss*100), stat = "identity", fill = "red") + ggtitle("Core Habitat Change") + labs(y="Core habitat change (%)", x = "Simulation iterations") + theme(plot.title = element_text(hjust = 0.5))

NonCorePlot <- ggplot() + geom_bar(mapping = aes(x = 1:length(DwellGrowthList), y = HabNonCoreLoss*100), stat = "identity", fill = "blue") + ggtitle("Non-core Habitat Change") + labs(y="Non-core habitat change (%)", x = "Simulation iterations") + theme(plot.title = element_text(hjust = 0.5))

KNumPlot <- ggplot() + geom_bar(mapping = aes(x = 1:length(DwellGrowthList), y = KNumLoss*100), stat = "identity", fill = "blue") + ggtitle("Change in Koala Abundance") + labs(y="Abundance change (%)", x = "Simulation iterations") + theme(plot.title = element_text(hjust = 0.5))

plot_grid(DwellPlot, KNumPlot, CorePlot, NonCorePlot, ncol = 2, nrow = 2)
