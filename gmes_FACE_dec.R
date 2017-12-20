
# read and format data ----------------------------------------------------
setwd("C:/R-projects/EucFACE_gmes/rawdata")  ##if you are using project you do
##not need to set wd! (yeah!)

gmes <-read.csv("Master_data_file_clean.csv")
  gmes$Date <- as.Date(gmes$Date)
  gmes$treatment <- with(gmes, paste(co2grow, position, sep="-"))
  gmes$Nm <- gmes$N.perc *10           # mass-based leaf N
  gmes$Na <- gmes$LMA * gmes$Nm / 1000 # Narea in g m-2
  

#separate dataset for CO2 grow only, 
#which means Co2grow = amb and Co2meas = amb + CO2grow = elev and CO2meas = elev!
ambco2 <- subset(gmes, co2grow == "amb" & co2meas == "amb")
elevco2 <- subset(gmes, co2grow == "elev" & co2meas == "elev")
growCO2 <- rbind(ambco2,elevco2)
  growCO2$Nm <- growCO2$N.perc *10
  growCO2$Na <- growCO2$LMA * growCO2$Nm / 1000 # Narea in g m-2
  
#or split by growth CO2 (FACE treatments)
#gmes_ambg <- gmes[gmes$co2grow == "amb",]
#gmes_elevg <- gmes[gmes$co2grow == "elev",]
###i get a little confused about how to analzye instantaneous or growth co2 :/

# plot objects -------------------------------------------------------------
palette(c("black", "red"))
pchs <- c(16,17)
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
walllab <- "Cell wall thickness  (units)"
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

#try a bunch, keep the ones that may show a trend to analyze

# Physiology (growth CO2)------------------------------------
par(mar=c(5,5,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(3,1,0))

plot(Photo ~ gmes, data=growCO2, col=co2grow, pch=pchs[canopy], ylim=c(0,30), 
     xlim=c(0,.55), xlab=gmlab, ylab=photolab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)

fit_ags <- lm(Photo ~ mean_gs, data = growCO2) 
  summary(fit_ags) # significant relationship (P[1,26]=0.0002)  
  
plot(Photo ~ mean_gs, data=growCO2, col=co2grow, pch=pchs[canopy], ylim=c(0,30),
     xlim=c(0,.4), xlab=condlab, ylab=photolab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
  # abline(fit_ags, lwd =2) # not extending to axis is perferred here
  library(plotrix)
  ablineclip(fit_ags, lwd=2, lty=2, x1= min(growCO2$mean_gs), x2=max(growCO2$mean_gs), 
                    y1=min(growCO2$Photo), y2=max(growCO2$Photo))
  #may be worth it to fit for each a/e co2 if they are different
  #yes they are so let's do it

  
##gm and cc are related
fit_gmcc <- lm(gmes~mean_cc, data = growCO2) 
  summary(fit_gmcc) # significant relationship (P[1,25]<0.0001, R2 = 0.47)
  
plot(gmes~mean_cc, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab,
     ylim=c(0,.5), xlim=c(0, 400))
  ablineclip(fit_gmcc, lwd=2, lty=2, x1= min(growCO2$mean_cc), x2=max(growCO2$mean_cc))

#informative boxplots  
boxplot(gmes~treatment, data=growCO2, ylab = gmlab)
boxplot(mean_gs~treatment, data=growCO2, ylab = condlab) # 3 outliers?
boxplot(Photo~treatment, data=growCO2, ylim=c(0,25), ylab=photolab)
#if you want to plot without outliers use outline=FALSE
# there may be a couple but the gs ones dont look that crazy.  gm=.4 may be too high
# we could make a clean dataset without outliers and rerun figs/stats

boxplot(LMA~treatment, data=growCO2, ylim=c(0,300), ylab="LMA")
#possible that lma differs by canopy position? maybe with N too (stats below)? yes below


# parenchyma thickness vs gm (growth CO2)----------------------------------------

###maybe we should choose one of the following 3 (most supported in literature)?
fit_gmlength <- lm(gmes~length.par1.mean, data = growCO2) 
  summary(fit_gmlength)
# p = 0.007 (negative reltionship, R2 = 0.22), 
#looks like eCO2 could be paralel but higher intercept compared to amb
plot(gmes~length.par1.mean, data=growCO2, col=co2grow, pch=pchs[canopy], 
     ylab=gmlab,ylim=c(0,.5),xlim=c(20, 45))
#fit ablineclip here


fit_sumlength<- lm(gmes~sumlength.par.mean, data = growCO2) 
  summary(fit_sumlength) # p = 0.03 (negative reltionship, R2 = 0.14)  
  #linear model shows that CO2 is significant, so likely different slopes too, then need to fit both lines
plot(gmes~sumlength.par.mean, data=growCO2, col=co2grow, pch=pchs[canopy], 
    ylab=gmlab, xlab = "Total parenchyma thickness" ,ylim=c(0,.5), xlim=c(65, 115))
    ablineclip(fit_sumlength, lwd=2, lty=2,x1=min(growCO2$sumlength.par.mean), 
               x2=max(growCO2$sumlength.par.mean))

fit_meanlength <- lm(gmes~meanlength.par.mean, data = growCO2) 
  summary(fit_meanlength) # p = 0.03 (negative reltionship, R2 = 0.14)   
plot(gmes~meanlength.par.mean, data=growCO2, col=co2grow, pch=pchs[canopy], 
     ylab=gmlab,xlab = "Mean parenchyma thickness", ylim=c(0,.5), xlim=c(20, 40))

    
# LMA  and gm vs mesophyl and cell wall thickness (not super useful???)----------
plot(LMA~leafw.mean.y, data=growCO2, col=co2grow, pch=pchs[canopy], ylab="LMA",
     xlab = walllab ,ylim=c(100,300))

plot(LMA~sumlength.par.mean, data=growCO2, col=co2grow, pch=pchs[canopy], 
     ylab="LMA", xlab = walllab ,ylim=c(100,300))

plot(gmes~leafw.mean.y, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab, xlab = "cell wall thickness" ,ylim=c(0,0.5)) 
     legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
     #test for CO2 differences?
    
plot(gmes~meso.mean, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))


# Statistical analyses (mixed model) -------------------------------------
# Within the Oct 2014 campaign, effects for CO2 and canopy position are tested
library(doBy)
library(lattice)
library(lme4)
library(car)
library(visreg)
library(lmerTest)
library(sciplot)

## NOTE: before doing these analyses we need tree level data, 
## that is R6 outer trees need to be averaged
## load the new datafile here 
gmes3 <-read.csv("Tree-means_Gm-master.csv") # reload after adjusting names!!
# and subset again for growth CO2 only ## give the object a different name!
ambco2 <- subset(gmes3, co2grow == "amb" & co2meas == "amb")
elevco2 <- subset(gmes3, co2grow == "elev" & co2meas == "elev")
growCO2 <- rbind(ambco2,elevco2)
growCO2$Nm <- growCO2$N.perc *10
growCO2$Na <- growCO2$LMA * growCO2$Nm / 1000 # Narea in g m-2

## need to adjust the denominator degrees of freedom i.e type III F-test ------------------------------------------
mod_gmes <- lmer(gmes ~ co2grow*position + (1|ring/tree), data=growCO2, 
                 na.action = na.omit)
anova(mod_gmes, test = "F")
# P-value for main effect adjusted for correct numer & demon d.f.
#install.packages("LMERConvenienceFunctions")
library(LMERConvenienceFunctions)
1 - pf(q=anova(mod_gmes)['co2grow', 'F.value'], df1 = 1, df2 = 4)  #manually add in the correct numerator & denom d.f.
1 - pf(q=anova(mod_gmes)['position', 'F.value'], df1 = 1, df2 = 4) 
1 - pf(q=anova(mod_gmes)['co2grow:position', 'F.value'], df1 = 1, df2 = 4) 

# this gives output that looks like a standard ANOVA table
pamer.fnc(mod_gmes)  # gives the table but you still need to ensure d.f. are done correctly

# stats gmes --------------------------------------------------------------
mod_gmes <- lmer(gmes ~ co2grow*position + (1|ring/tree), data=growCO2, 
                 na.action = na.omit)
  Anova(mod_gmes, test = "F")
  VarCorr(mod_gmes)

  plot(mod_gmes) # pretty good
  qqPlot(residuals(mod_gmes)) # bit of an upwards tail on the left
  test <- leveneTest(gmes ~ co2grow * position, data = growCO2)
  summary(test) # not signficant so variance are equal
  bwplot(gmes ~ position | co2grow , data = growCO2) # all ns different

# Now we analyse gmes with the measured CO2 concentrations (short term)
mod_gmes_meas <- lmer(gmes ~ co2meas*position + (1|ring/tree), data=gmes, 
                      na.action = na.omit)
  Anova(mod_gmes_meas, test = "F")
  VarCorr(mod_gmes_meas)
  # and as seen before, there  is now a significant CO2 effect on gmes (P = 0.023) 
  #where gm in eCO2 > gm in amb
visreg(mod_gmes_meas, "co2meas", by = "position", overlay = T, ylab = gmlab)
bargraph.CI(co2meas, gmes, position, data = gmes, legend = T, ylab = gmlab)
bargraph.CI(co2meas, gmes, data = gmes, legend = T, ylab = gmlab, xlab = "measured CO2")
bargraph.CI(co2grow, gmes, data = growCO2, legend = T, ylab = gmlab, xlab = "growth CO2")

# we should be seeing the same trends for gmes using globulus parameters
#(although now species-specific data is available)
mod_gmes_glob <- lmer(mean_gm.Eglob ~ co2grow*position + (1|ring/tree), data=growCO2, 
            na.action = na.omit)
  Anova(mod_gmes_glob, test = "F")
  VarCorr(mod_gmes_glob)

mod_gmes_glob_meas <- lmer(mean_gm.Eglob ~ co2meas*position + (1|ring/tree), data=gmes, na.action = na.omit)
  Anova(mod_gmes_glob_meas, test = "F")
  # and that is the case: sign CO2 effect with CO2meas and ns with CO2grow


# stats gs ----------------------------------------------------------------
mod_gs <- lmer(mean_gs ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_gs, test = "F")
  VarCorr(mod_gs)
# no Co2 by position interaction
visreg(mod_gs, "co2grow", by= "position", overlay =T, ylab = condlab)
# so gs decreased in elevated CO2 in upper canopy (or stay the same if ns) 
# but increased in eCO2 in the lower canopy

mod_gs_meas<- lmer(mean_gs ~ co2meas*position + (1|ring/tree), data=gmes, na.action = na.omit)
Anova(mod_gs_meas, test = "F")
# there are no differces in gs using co2meas - that is good!


# stats photosynthesis ----------------------------------------------------

mod_vcmax<- lmer(Vcmax ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_vcmax, test = "F")
  VarCorr(mod_vcmax)

mod_jmax<- lmer(Jmax ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_jmax, test = "F")

mod_photo <- lmer(Photo ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_photo, test = "F")
  visreg(mod_photo, "co2grow", by= "position", overlay =T, ylab = "Photo")


# stats chemistry and lma -------------------------------------------------

mod_lma<- lmer(LMA ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_lma, test = "F")

# run when complete
mod_nitro <- lmer(N.perc ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_nitro, test = "F")
  visreg(mod_nitro, "co2grow", by= "position", overlay =T, ylab = "N.perc")
  # leaf N is reduced in eCO2 in the upper canopy only, 
  # but same values in the lower canopy between the 2 cO2 treatments


# stats leaf anatomy ------------------------------------------------------
mod_thick <- lmer(leafw.mean.y ~ co2grow*position + (1|ring/tree), data=growCO2, 
             na.action = na.omit)
  Anova(mod_thick, test = "F")
# used the leaf measurement tab from the anatomy, consistent across anatomical variables 
# values from the leaf measurements and cross section measurements are very similar
# strong position effect for leaf thickness, logical


# test for difference in mesophyll layers and the average thickness of the mesophyll
mod_mesolayer <- lmer(mesolay.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_mesolayer, test = "F")

mod_mesothick <- lmer(meso.mean.y ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_mesothick, test = "F")
  # both have strong position effects (others are ns)

# parenchyma cells: length and sum
mod_parlen <- lmer(length.par1.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(m2a4, test = "F")

mod_parsum <- lmer(sumlength.par.mean ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit)
  Anova(mod_parsum, test = "F")
  # these are not significant for any effects

# stomatal density: also ns
mod_sdup <- lmer(stomdenad ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #underside stomata
  Anova(mod_sdup, test = "F")

mod_sdunder<- lmer(stomdenab ~ co2grow*position + (1|ring/tree), data=growCO2, na.action = na.omit) #upperside stomata
  Anova(mod_sdunder, test = "F")

