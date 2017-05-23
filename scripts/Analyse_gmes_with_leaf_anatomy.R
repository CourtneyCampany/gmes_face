### Analysis of anatomical measurements _ EucFACE_gmes leaves _ DE

library(doBy)
library(lattice)

# read data -------------------------------------------------------------------------
anatomy<-read.csv("raw_data/face_gm_anatomy2.csv") #cross section tab

# NOTE: I sorted out name conventions for R6. R3 was a bit harder but concluded the following: There is a R3-N in anatomy 
# that does not exist or match in physiology dataset, so will be deleted. 
# R3-S is t301 south of scaffolding and R3-O is the big tree outside R3.


#anatomy<-merge(anatomy1,anatomy2)
# make tree averages within canopy position
anat1.tree<-summaryBy(leafw + epiup + epilow + meso+ sum ~ Tree +Position, data=anatomy, FUN = mean) # wrong means?
anat1.tree$Conc <- as.factor(paste(anat1.tree$Tree, anat1.tree$Position, sep = "")) # concatanating function


anatomy2<-read.csv("rawdata/face_gm_anatomy3.csv") # leaf measurements tab
anatomy2$sumlength.par<-anatomy2$length.par1+anatomy2$length.par2+anatomy2$length.par3
anatomy2$meanlength.par<-(anatomy2$length.par1+anatomy2$length.par2+anatomy2$length.par3)/3
# I think the total parenchyma thickness is meso
# but check with Rosana - checked, yes

anat2.tree<-summaryBy(leafw+ epiupper+ epilower+ mesolay+ meso+ length.par1+ sumlength.par +meanlength.par ~ Tree +Position, data=anatomy2, FUN = mean)
anat2.tree$Conc <- as.factor(paste(anat2.tree$Tree, anat2.tree$Position, sep = ""))

#Concatenate AND COMBINE the physiology and anatomy
# Combine anatomy first i.e. cross section means and leaf measurement means
anat.tree<-merge(anat1.tree, anat2.tree, all.x=TRUE, by="Conc")
# Technically you don't need to merge because the variables leafw.mean, epiup.mean, epilow.mean and meso mean are (almost) exactly the same in anat1.tree and anat2.tree 
# so we can just go with anat2.tree and be fine.
# now read in the physiology data
phys<-read.csv("rawdata/face_leaves_with_gmes.csv")

# get physiology subset for the growth conditions
phys.eCO2<-subset(phys, CO2grow == "elev" & CO2meas == "elev") 
phys.aCO2<-subset(phys, CO2grow == "amb" & CO2meas == "amb") 
phys.CO2<-rbind(phys.aCO2, phys.eCO2)

phys.CO2$Conc <- as.factor(paste(phys.CO2$Tree, phys.CO2$Canopy, sep = ""))
#Now I want to do the same thing including both growth and measurements CO2
phys$Conc <- as.factor(paste(phys$Tree, phys$Canopy, sep = ""))

# now need to combine with the averaged anatomy for CO2 growth conditions
full.data2<-merge(anat.tree, phys.CO2, all=TRUE, by="Conc")
names(full.data2)[2:7]<-c("Tree", "Position", "leafw", "epi_up", "epi_low", "meso.mean")
names(full.data2)[26]<-"gmes"

# if we want to include the measurement CO2 we can merge, but will double up the anatomy means data
full.data1<-merge(anat.tree, phys, all=TRUE, by="Conc")
names(full.data1)[2:7]<-c("Tree", "Position", "leafw", "epi_up", "epi_low", "meso.mean")
names(full.data1)[26]<-"gmes"

write.csv(full.data1,"calculated_data/Physiology.anatomy.data.csv")

# can merge LMA and leaf nitrogen concentrations (but not for all leaves)

# Making boxplots --------------------------------------------------------------------------------------------

# analyse to find the instantaneous effect of eCO2 on gmes
boxplot(Mean_gm.Eglob ~ CO2grow+Canopy+CO2meas,cex.lab=0.8, cex.axis=0.8,ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),
        xlab="",las=2, data=phys)
# this shows a higher gmes when measured at eCO2 

#dev.copy2pdf(file="Boxplot_gmes_all.pdf")
#dev.off()
# lattice plot _ effect of eCO2 on gmes
#bwplot(Mean_gm.Eglob ~ CO2grow+Canopy | CO2meas,ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),xlab="growth CO2 and position", data = phys)
#dev.copy2pdf(file="Lattplot_gmes_all.pdf")
#dev.off()

#scatterplot
#CO2.xyplot<-xyplot(Mean_gm.Eglob ~ CO2grow+Canopy | CO2meas,ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),xlab="growth CO2 and position", data = phys)
#CO2.xyplot[["main"]] <- "Measurement CO2 effects on gmes"
#CO2.xyplot
#dev.copy2pdf(file="Scatter_gmes_all.pdf")
#dev.off()

# shows an instantaneous difference in gmes based on measurement CO2 but not growth CO2
# now do the formal test for this as a linear model or nonlinear model


# now can do things combining gmes and anatomy
bwplot(meso.mean.y ~ Position | CO2grow,ylab=expression(thick[mes]~(?m)),xlab="Growth CO2 and Canopy position", data = full.data1)
# shows that leaf thickness is lower in lower canopy position for btoh ambient and elevated CO2, not much new.
bwplot(length.par1.mean ~ Position | CO2grow,ylab=expression(Paren_thick~(?m)),xlab="Growth CO2 and Canopy position", data = full.data1)
# also look at leaf thickness
bwplot(leafw.mean.y ~ Position | CO2grow,ylab=expression(Paren_thick~(?m)),xlab="Growth CO2 and Canopy position", data = full.data1)
bwplot(leafw.mean.y ~ CO2meas+Canopy | CO2grow,ylab=expression(Leaf_thick~(?m)),xlab="meas CO2 and position (growth CO2 top)", data = full.data1)
# leaf thickness is the same for ambient and elevated growth CO2 but there is a difference in canopy position.
bwplot(gmes ~ CO2meas+Canopy | CO2grow,ylab=expression(Leaf_thick~(?m)),xlab="meas CO2 and position (growth CO2 top)", data = full.data1)
# this shows 2 outliers with gmes> 1.0 so  boxplots look squashed. Gmes is higher when measured at eCO2 compared to aCO2 but no difference between growth CO2.
# There might be a difference between upper and lower canopy, not clear for gmes.

boxplot(gmes ~ CO2grow, data = full.data1, ylab = "gmes", xlab = "Co2 treatment")
boxplot(gmes ~ CO2meas, data = full.data1, ylab = "gmes", xlab = "measured Co2")
boxplot(gmes ~ Position, data = full.data1, ylab = "gmes", xlab = "Canopy position")
boxplot(meso.mean ~ Position, data = full.data1, ylab = "leaf thickness (meso)", xlab = "Canopy position")
boxplot(meso.mean ~ CO2grow, data = full.data1, ylab = "leaf thickness (meso)", xlab = "Co2 treatment")

# are any of these significantly different - 1 way ANOVA
fit1 <- lm(gmes ~ Position, data = full.data1) # not significantly different
summary(fit1)
fit2 <- lm(gmes ~ CO2grow, data = full.data1) # not significantly different
summary(fit2)
fit3 <- lm(gmes ~ CO2meas, data = full.data1) # yes significant at P = 0.0238 :-)
summary(fit3)
fit4 <- lm(meso.mean ~ Position, data = full.data1) # yes significant at P = 0.0006 
summary(fit4)
fit5 <- lm(meso.mean ~ CO2grow, data = full.data1) # not significantly different with growth CO2
summary(fit5)

# Now try a two-way ANOVA between Canopy position and CO2 treatment (the more appropriate model)
fit6 <- lm(gmes ~ Position + CO2grow, data = full.data1) # as expected, not significant for either factor
summary(fit6)
# However, in order to do these statistics properly, I have to average by tree, not sure if I really have 53 DF!
#Mean.gmes  <- summaryBy(gmes ~ Tree + CO2grow + Position, data=full.data1, FUN = mean)
#fit6b <- lm(Mean.gmes ~ Position + CO2grow, data = full.data1) # as expected, not significant for either factor
#summary(fit6b)


fit7 <- lm(gmes ~ Position + CO2grow, data = full.data2) 
summary(fit7)
anova(fit7) # now DF residuals = 25 with CO2grow just under P = 0.1
plot(fit7, which=c(3,2)) # basic diagnostic plots, points # 12 and 17 are out there, residuals ok?

fit12 <- lm(gmes ~ Position + CO2meas + Position:CO2meas, data = full.data) 
summary(fit12) 
anova(fit12) # the difference in measurement cO2 come out P = 0.028 but not CO2 position or the interaction

# some scatter plots-------------------------------------------------------------------------------------------------------

gm_range <- range(0, 0.6, na.rm=T) 
palette <- c("blue", "red")
plot(gmes ~ length.par1.mean, data=full.data2, log="", pch = 19,
     col=CO2grow, cex.lab=1.2, cex.axis=1.4, bty="l",
     ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),
     xlab=expression(Paren_thick~(?m)), ylim = gm_range)
legend(x="topright", pch=19, legend=c("aCO2","eCO2"), col=palette,
       cex=1.2, bty="n", lwd=1.5)
#abline(a= fit9$coefficients[[1]], b = fit9$coefficients[[2]], lwd = 2)
abline(fit9)
# removed one outlier of gm_Tobacco > 1.0
dev.copy2pdf(file="gmes_thickness.pdf")
dev.off()
# And is this negative relationship different in eCO2 and aCO2?
fit8 <- lm(gmes~length.par1.mean+CO2grow+length.par1.mean:CO2grow, data=full.data2)
summary(fit8) # No not different so can be fit as one line
# so to fit a line we need to do a linear regression aka one-way ANOVA
fit9 <- lm(gmes~length.par1.mean, data = full.data2) 
summary(fit9)
# good this is significant (P = 0.006) and now we need to extract these coefficients to inform the abline
# in the figure
#str(fit9)
#fit9$coefficients with intercept first then slope

plot(Mean_gm.Tobacco ~ Mean_gs, data=phys, log="", pch = 19,
     col=CO2grow, cex.lab=1.2, cex.axis=1.4, bty="l",
     ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),
     xlab=expression(Cond~(?m)))
legend(x="topright", pch=19, legend=c("aCO2","eCO2"), col=palette,
       cex=1.2, bty="n", lwd=1.5)
# should see a relationship but i think it is a scattershot

plot(gmes ~ sumlength.par.mean, data=full.data2, log="", pch = 19,
     col=CO2grow, cex.lab=1.2, cex.axis=1.4, bty="l",
     ylab=expression(g[mes]~(mol~ m^{-2}~ s^{-1})),
     xlab=expression(Tot_paren_thick~(?m)))
legend(x="topright", pch=19, legend=c("aCO2","eCO2"), col=palette,
       cex=1.2, bty="n", lwd=1.5)
abline(fit11, lwd=2)
# Relationship across CO2 treatment is significant (P = 0.025) but low R2
fit10 <- lm(gmes~sumlength.par.mean+CO2grow+sumlength.par.mean:CO2grow, data=full.data2)
summary(fit10) # interaction is not different so there is one line across CO2 treatments
fit11 <- lm(gmes~sumlength.par.mean, data = full.data2) 
summary(fit11)
# very similar fit when using full.data1 which includes meas CO2

dev.copy2pdf(file="Name_it.pdf")
dev.off()

# Need Anet in the dataset and related to gs and gmes (2 panel figure)
# Make a table with photosynthetic traits: means +SE for upper and lower, mature leaves for Anet, gs, WUE, gmes, Vcmax and Jmax
# Make a table with leaf structural/anatomical traits: means + SE for LMA, leaf N, leaf thickness, mesophyll tickness, spongy (non-existent) and pallisade mesophyll, number of pallisade layers
# should be able to determing Sm/S and chloroplast length, stomatal length for our samples?
# if we get Sm/S, then perhaps we can relate that to Anet (Anet - Sm/S)
# relationships between LMA and mesophyll thickness and Sm/S and mesophyll thickness (2 panels)
