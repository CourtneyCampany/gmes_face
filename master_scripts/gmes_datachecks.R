## frequency distribution of variables:

source("master_scripts/functions.R")

gmes <-read.csv("master_scripts/Tree-means_Gm-master2.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]

#test each variable, evaluate for need to transform
#looking at distribution and shapiro.test carefully due to small samples sizes
library(car)
library(MASS)
library(rcompanion)

### 1. Likely need data transformations:----------

### I start with sqrt(), log(), inverse (1/x). Then try others

#gmes
plotNormalHistogram(gmes_clean$gmes)
qqPlot(gmes_clean$gmes)
shapiro.test(gmes_clean$gmes)

  #simple sqrt() works great
  plotNormalHistogram(sqrt(gmes_clean$gmes))
  shapiro.test(sqrt(gmes_clean$gmes))

#mesolayer.mean
plotNormalHistogram(gmes2$mesolay.mean)
qqPlot(gmes2$mesolay.mean)
shapiro.test(gmes2$mesolay.mean)

  #inverse transformation may work best
  plotNormalHistogram(1/(gmes2$mesolay.mean))
  shapiro.test(1/(gmes2$mesolay.mean))

#sumlength.par.mean
plotNormalHistogram(gmes2$sumlength.par.mean)
qqPlot(gmes2$sumlength.par.mean) #a few outliers
shapiro.test(gmes2$sumlength.par.mean)

  #sin transformation may work best
  plotNormalHistogram(sin(gmes2$sumlength.par.mean))
  shapiro.test(sin(gmes2$sumlength.par.mean)) 

#meanlength.par.mean
plotNormalHistogram(gmes2$meanlength.par.mean)
qqPlot(gmes2$meanlength.par.mean) #a few outliers
shapiro.test(gmes2$meanlength.par.mean)

  #nothing works here (but .86 could be worse)

#drawdown
plotNormalHistogram(gmes2$drawdown)
qqPlot(gmes2$drawdown) #serious outliers
boxplot(drawdown~co2grow ,data=gmes2 ) #outliers maybe in eCO2
shapiro.test(gmes2$drawdown)

  #log transform works (log or log10)
  plotNormalHistogram(log10(gmes2$drawdown))
  shapiro.test(log10(gmes2$drawdown))

### 2. Appear to be ok:-----------

#Photosynthesis 
plotNormalHistogram(gmes2$Photo)
qqPlot(gmes2$Photo)
shapiro.test(gmes2$Photo)

#Stomatal conductance
plotNormalHistogram(gmes2$gs)
qqPlot(gmes2$gs)
shapiro.test(gmes2$gs)

#Ci
plotNormalHistogram(gmes2$Ci)
qqPlot(gmes2$Ci)
shapiro.test(gmes2$Ci)

#LMA
plotNormalHistogram(gmes2$LMA)
qqPlot(gmes2$LMA)
shapiro.test(gmes2$LMA)

#Nm
plotNormalHistogram(gmes2$Nm)
qqPlot(gmes2$Nm)
shapiro.test(gmes2$Nm)

#Na
plotNormalHistogram(gmes2$Na)
qqPlot(gmes2$Na)
shapiro.test(gmes2$Na)

#leafw
hist(gmes2$leafw)
qqPlot(gmes2$leafw)
shapiro.test(gmes2$leafw)

#epiup
plotNormalHistogram(gmes2$epi_up)
qqPlot(gmes2$epi_up) ##maybe one outlier
shapiro.test(gmes2$epi_up)

#epilow
plotNormalHistogram(gmes2$epi_low)
qqPlot(gmes2$epi_low) ##maybe one outlier
shapiro.test(gmes2$epi_low)

#stomdenad
plotNormalHistogram(gmes2$stomdenad)
qqPlot(gmes2$stomdenad) ##maybe a few outliers
shapiro.test(gmes2$epi_up)

#stomdenab
plotNormalHistogram(gmes2$stomdenab)
qqPlot(gmes2$stomdenab)
shapiro.test(gmes2$stomdenab)

#stomlengthad
plotNormalHistogram(gmes2$stomlengthad)
qqPlot(gmes2$stomlengthad)
shapiro.test(gmes2$stomlengthad)

#stomlengthab
plotNormalHistogram(gmes2$stomlengthab)
qqPlot(gmes2$stomlengthab)
shapiro.test(gmes2$stomlengthab)

#meso.mean
plotNormalHistogram(gmes2$meso.mean)
qqPlot(gmes2$meso.mean)
shapiro.test(gmes2$meso.mean)

#epiupper.mean
plotNormalHistogram(gmes2$epiupper.mean)
qqPlot(gmes2$epiupper.mean) #maybe one outlier
shapiro.test(gmes2$epiupper.mean)

#epilower.mean
plotNormalHistogram(gmes2$epilower.mean)
qqPlot(gmes2$epilower.mean)
shapiro.test(gmes2$epilower.mean)

#vcmax25
plotNormalHistogram(gmes2$Vcmax25)
qqPlot(gmes2$Vcmax25)
shapiro.test(gmes2$Vcmax25)

#jmax25
plotNormalHistogram(gmes2$Jmax25)
qqPlot(gmes2$Jmax25)
shapiro.test(gmes2$Jmax25)

#Rd25
plotNormalHistogram(gmes2$Rd25)
qqPlot(gmes2$Rd25)
shapiro.test(gmes2$Rd25)


