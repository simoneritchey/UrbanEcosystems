###URF Poster

#load packages
library(dplyr)
library(tidyr)
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")


##Based on Dina's PNSQA PCA based on state

#load data
getwd()
setwd("/Users/simone/Documents/Documents/UT/UrbanEco/UrbanEcosystems/RSQA_allregions")
results <- read.csv("results.csv")
PNSQAresults <- results %>% filter(RSQA_STUDY == "PNSQA")
sites <- read.csv("sites.csv")
PNSQAsites <- sites %>% filter(RSQA_STUDY == "PNSQA")

#Data cleaning
results_median <- PNSQAresults %>% group_by(SITE_NO, PARM_NM) %>% dplyr::summarise(median_val = median(RESULT_VA)) %>% ungroup()
data <- pivot_wider(results_median, id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)

datatrimmed <- as.data.frame(data[,c(colSums(is.na(data)) < 20 & colSums(is.na(data)) != 0)])
rownames(datatrimmed) <- data$SITE_NO
finaldata <- datatrimmed[complete.cases(datatrimmed), ]
metadata <- PNSQAsites[match(rownames(finaldata), PNSQAsites$SITE_NO), ]

#PCA
##zero variance error
finaldata <- finaldata[, sd(col(finaldata)) != 0]
bob <- finaldata %>% summarise(across(everything(), var)) %>% pivot_longer(cols = 1:109) %>% filter(value != 0)
finaldata <- finaldata %>% select(bob$name)

?select()
prcompData <- prcomp(finaldata, center = T, scale = T)
ggbiplot::ggbiplot(prcompData, choices = c(1,2), var.axes=FALSE, groups = paste(metadata$STATE_NM), ellipse = T)
ggbiplot::ggbiplot(prcompData, choices = c(1,2), var.axes=T, groups = paste(metadata$STATE_NM), ellipse = T)

