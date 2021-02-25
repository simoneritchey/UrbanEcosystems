###SEM Tutorial
###Urban Ecosystems

#Load required packages
library(dplyr)
library(tidyr)
library(lavaan)
library(semPlot)

#Import data 
results <- read.csv("results.csv")
sites <- read.csv("sits.csv")

#Mean center, convert to short, and remove NA's as per PCA protocol.
results_median <- %>% group_by(SITE_NO, PARM_NM) %>% dplyr::summarise(meidan_val = median(RESULT_VA)) %>% ungroup()
data <- pivot_wider(results_median, id_cols = SITE_NO, names_from = PARM_NM, values_from = median_val)

colSums(is.na(data))
table(colSums(is.na(data)))
dataframeout <- data[,colSums(is.na(data)) < 7]
finaldata <- dataframeout[complete.cases(dataframeout), ]

finaldata <- as.data.frame(finaldata[,which(apply(finaldata,2,var) != 0)])
rownames(finaldata) <- finaldata[,1]
## Note we are delaying removal of the row until after merging
##finaldata <- finaldata[,-1]

## Note you can manually rename variables to make things easier
#colnames(finaldata) <- c('Calcium', "Chloride", "Magnesium", "Potassium", "SAR", "Sodium", "Sulfate")

## Okay we need to merge landuse and the biodata on so we can use it in our model
watershed <- read.csv("RSQA_Characteristics_Data_WatershedData.csv") 
finaldata$SITE_NO <- rownames(finaldata)
mergedShortData <- merge(finaldata, watershed, by = "SITE_NO")
## Reassign the SITE_No back to rownames
rownames(mergedShortData) <- mergedShortData$SITE_NO

biodata <- read.csv("biotest.csv")

## Because R likes to mess up the SITE_NO in biodata and put it in scientific notation
## We are going to override it and put a T in front of the site numbers to convince R it is not a number
mergedShortData$SITE_NO <- paste0("T", as.numeric(mergedShortData$SITE_NO))
## luckily the USGS already did this for us on biodata, but to make things easy 
## we are going to take that column and copy it to the SITE_NO column
biodata$SITE_NO <- biodata$TSITE_QW 
biodata$SITE_NO <- paste0("T", as.numeric(biodata$SITE_NO))

## Reassign the SITE_No back to rownames
rownames(mergedShortData1) <- mergedShortData1$SITE_NO
## Pull the SITE_NO back out, its not a variable but an ID so it should NOT stay a column
mergedShortData1 <- mergedShortData1[,colnames(mergedShortData1) != "SITE_NO"]

##----------------------------------
##SEM
data_cor <- cor(finaldata)

## Run the following line to see all the variables you have to work with
colnames(mergedShortData1)

## the spacing and commas in some of the chemical names will cause problems, I'm going to relabel this one
## notes: there are more efficient ways to do this for all the chemicals at once using regexr and gsub
colnames(mergedShortData1)[1] <- "Methylnaphthalene"

## Okay we need to construct the "measurement model"
## This contains the main variables with their respective latent variables (ie not normally shown in papers or otherwise)
## I suspect that the circles on the SEM in the original paper are the main variables (sometimes with only a single latent variable (which they don't show))
sem.model.measurement <- "Chemicals =~ 1*Methylnaphthalene
Chemical_bin2 =~ 1*Insecticide.P
Chemical_bin3 =~ 1*Herbicide.P
"
## These main variables or "bins" are probably not what you actually want, done just for training purposes

sem.fit.measurement <- sem(sem.model.measurement, data = mergedShortData1)
summary(sem.fit.measurement, fit.measures = TRUE)
semPaths(sem.fit.measurement, "par", edge.label.cex = 1.2, fade = FALSE)

## See how the first variable in each "bin" is set to 1
## so you are scaling everything to that first variable
## That is a result of the 1* in making the sem.model.measurement
## A good model has a CFI/TLI around .95, RMSEA approaching .05, and SRMR < .05
## Note you find this after running the summary(fit) line


## at this point you should be able to read the tutorial on SEM this was adapted from
## This should handle most of the RSQA specific information



