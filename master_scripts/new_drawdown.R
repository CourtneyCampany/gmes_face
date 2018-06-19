#calculate new drawdown:

source("master_scripts/functions.R")

gmes <-read.csv("master_scripts/Tree-means_Gm-master2.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]

##calculate CC
gmes_clean$Cc<- with(gmes_clean, Ci-Photo/gmes)

##calculate drawdown
gmes_clean$drawdown2 <- with(gmes_clean, Ci-Cc)

library(doBy)
library(plotrix)

summaryBy(drawdown2 ~ co2grow + canopy,
          data=gmes_clean, FUN=c(mean, std.error), na.rm=TRUE)
