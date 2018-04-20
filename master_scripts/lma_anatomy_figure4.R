#LMA relation to leaf anatomy

#photo vs gm/gs panel
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

#stats-------------------------------------
# library(lme4)
# library(car)
# library(lmerTest)
# library(LMERConvenienceFunctions)
# library(MuMIn)
# library(visreg)
# 
# #lma and leaf thickness (autocorrelated ??? ignoring fow now)
# mod_lma_leafw <- lmer(LMA~leafw.mean.y*co2grow*canopy + (1|ring/tree), data=gmes2,
#                       na.action = na.omit)
# Anova(mod_lma_leafw, test = "F") #seperate interaction with CO2 and canopy
# r.squaredGLMM(mod_lma_leafw)
# visreg(mod_lma_leafw, "leafw.mean.y", by= "co2grow", overlay =T)
# visreg(mod_lma_leafw, "leafw.mean.y", by= "canopy", overlay =T)
# 
# #lma and upper epidermis
# mod_lma_epiupp <- lmer(LMA~epiupper.mean*co2grow*canopy + (1|ring/tree),
#                        data=gmes2,na.action = na.omit)
# Anova(mod_lma_epiupp, test = "F") #.09 marginal interaction with canopy
# r.squaredGLMM(mod_lma_epiupp)
# 
# mod_lma_epilow <- lmer(LMA~epilower.mean*co2grow*canopy + (1|ring/tree),
#                        data=gmes2,na.action = na.omit)
# Anova(mod_lma_epilow, test = "F") #p=0.050 interaction with canopy
# r.squaredGLMM(mod_lma_epiupp)
# visreg(mod_lma_epilow, "epilower.mean", by= "canopy", overlay =T)
# #postive correlation in lower canopy only
# 
# ##lma and layers of epidermis
# mod_lma_mesolayer <- lmer(LMA~mesolay.mean*co2grow*canopy + (1|ring/tree),
#                           data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesolayer, test = "F") #marginal (p=0.060) with co2
# 
# ##meso thickness
# mod_lma_mesothick <- lmer(LMA~meso.mean.y*co2grow*canopy + (1|ring/tree),
#                           data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesothick, test = "F")#interatctions between mesophyll and co2 and canopy
# summary(mod_lma_mesothick)
# r.squaredGLMM(mod_lma_mesothick)
#   #important effects
#   visreg(mod_lma_mesothick, "meso.mean.y", by= "co2grow", overlay =T)
#   visreg(mod_lma_mesothick, "meso.mean.y", by= "canopy", overlay =T)
# 
#   mod_lma_mesothick2 <- lmer(LMA~meso.mean.y+ (1|ring/tree),data=ac,na.action = na.omit)
#   mod_lma_mesothick3 <- lmer(LMA~meso.mean.y+ (1|ring/tree),data=ec,na.action = na.omit)
#   Anova(mod_lma_mesothick2, test = "F") ###ac only
#   Anova(mod_lma_mesothick3, test = "F")
#   r.squaredGLMM(mod_lma_mesothick2) #r2 = .63, .73
#   #canopy (see visreg, I think postive in lower canopy
#   #but similar stats code as aboev doesnt doesnt work)
#   visreg(mod_lma_mesothick, "meso.mean.y", by= "canopy", overlay =T)
#   mod_lma_mesothick4 <- lmer(LMA~meso.mean.y * co2grow + (1|ring/tree),
#                              data=gmes2,na.action = na.omit)
#   Anova(mod_lma_mesothick4, test = "F")
#   visreg(mod_lma_mesothick4, "meso.mean.y", by= "co2grow", overlay =T)
#   
# ##uses LMERtest package... (doesnt help)
#   lsmeansLT(mod_lma_mesothick, test.effs = c("meso.mean.y", "canopy"))
#   lsmeansLT(mod_lma_mesothick, test.effs = c("meso.mean.y", "co2grow"))

#plotting----------------------------
palette(c("black", "red"))
pchs <- c(16,17)
pchs2 <- c(1,2)
lmalab <- expression(LMA~~(g~cm^-2))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)
legpch2 <- c(1,1,1,2)
library(plotrix)
library(scales)

#for ablineclip
mod_lma_mesothick_ac <- lm(LMA~meso.mean.y, data=ac)
mod_lma_mesothick_low <- lm(LMA~meso.mean.y, data=low)
mod_lma_uppderm <- lm(LMA~epilower.mean, data=low)

windows()
par(las=1, mgp=c(3,1,0), mar=c(5,5,1,1))

plot(LMA~meso.mean.y, data=gmes2,ylim=c(100, 300), xlim=c(200, 400), type='n', 
     ylab=lmalab, xlab=expression(Mesophyll~thickness~~(mu*m)))
points(LMA~meso.mean.y, data=gmes2,col=co2grow, pch=pchs2[canopy], cex=1.5)
ablineclip(mod_lma_mesothick_ac, x1=min(ac$meso.mean), x2=max(ac$meso.mean),
                    lwd=2, lty=2) #aco2 
points(LMA~meso.mean.y, data=ac, col=co2grow, pch=pchs[canopy], cex=1.5)

ablineclip(mod_lma_mesothick_low, x1=min(low$meso.mean.y), x2=max(low$meso.mean.y),
           lwd=2, lty=1) #lower
points(LMA~meso.mean.y, data=low, col=co2grow, pch=pchs[canopy], cex=1.5)

legend("topright", leglab, pch=legpch2, col=allcols,inset = 0.01, bty='n',cex=1)

dev.copy2pdf(file= "master_scripts/lma_anatomy.pdf")
dev.off()
