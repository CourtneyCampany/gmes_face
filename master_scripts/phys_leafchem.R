##gmes and anatomy 

#gmes and leaf internal anatomy
source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("master_scripts/Master_data_file_clean.csv")
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

ac <- gmes2[gmes2$co2grow == "amb",]
ec <- gmes2[gmes2$co2grow == "elev",]
low <- gmes2[gmes2$canopy == "lower",]
upp <- gmes2[gmes2$canopy == "upper",]

#stats-------------------------------
library(lme4)
library(car)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)

#Photo and Nitrogen (area based)
fit_photonitro<- lmer(Photo~Na*co2grow*canopy + (1|ring/tree),
                    data=gmes2,na.action = na.omit)
Anova(fit_photonitro, test = "F")
###nothing with NA


#Photo and gmes (area based)
fit_gmnitro<- lmer(gmes~Na*co2grow*canopy + (1|ring/tree),
                      data=gmes2,na.action = na.omit)
Anova(fit_gmnitro, test = "F")
##weak interaction with canopy p=.088

