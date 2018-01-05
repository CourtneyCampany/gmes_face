#LMA relation to leaf anatomy

#photo vs gm/gs panel
source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("Master_data_file_clean.csv")
data2 <-read.csv("Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

ac <- gmes2[gmes2$co2grow == "amb",]
ec <- gmes2[gmes2$co2grow == "elev",]
low <- gmes2[gmes2$canopy == "lower",]
upp <- gmes2[gmes2$canopy == "upper",]

#stats-------------------------------------
library(lme4)
library(car)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)

#I havent deleted the code yet, but mesophyll thickness seems to be the only
#trait detectable with LMA, when using the full model with treatments

##lma and leaf thickness
# mod_lma_leafw <- lmer(LMA~leafw.mean.y + (1|ring/tree), data=gmes2,na.action = na.omit)
# Anova(mod_lma_leafw, test = "F") #.08632
# r.squaredGLMM(mod_lma_leafw) 
# 
# ##lma and upper epidermis
# mod_lma_epiupp <- lmer(LMA~epiupper.mean + (1|ring/tree), 
#                        data=gmes2,na.action = na.omit)
# Anova(mod_lma_epiupp, test = "F") #.055 close
# r.squaredGLMM(mod_lma_epiupp) 
# 
# mod_lma_epiupp2 <- lmer(LMA~epiupper.mean*co2grow*canopy + (1|ring/tree), 
#                         data=gmes2,na.action = na.omit)
# Anova(mod_lma_epiupp2, test = "F")
# mean(upp$epiupper.mean)
# mean(low$epiupper.mean)
# 
# ##lma and layers of epidermis
# mod_lma_mesolayer <- lmer(LMA~mesolay.mean + (1|ring/tree), 
#                        data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesolayer, test = "F")
# r.squaredGLMM(mod_lma_mesolayer) 
# 
# mod_lma_mesolayer2 <- lmer(LMA~mesolay.mean*co2grow*canopy + (1|ring/tree), 
#                           data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesolayer2, test = "F")
# 
# ##lma and thickness of epidermis
# mod_lma_mesothick <- lmer(LMA~meso.mean.y + (1|ring/tree), 
#                           data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesothick, test = "F") #.092 close
# r.squaredGLMM(mod_lma_mesothick)  #dont use the first model
# 
# mod_lma_mesothick2 <- lmer(LMA~meso.mean.y*co2grow*canopy + (1|ring/tree), 
#                           data=gmes2,na.action = na.omit)
# Anova(mod_lma_mesothick2, test = "F")
# r.squaredGLMM(mod_lma_mesothick2)
# #im interested in the interatction between mesophyll and co2 and canopy
# mesoupp <- mean(upp$meso.mean)
# mesolow <- mean(low$meso.mean)
# (mesoupp-mesolow)/mesoupp #mesopyll thickness higher in upper canopy
# 
# mesoac <- mean(ac$meso.mean)
# mesoec <- mean(ec$meso.mean)
# (mesoec-mesoac)/mesoec #mesopyll thickness higher in eco2

#plotting----------------------------
palette(c("black", "red"))
pchs <- c(16,17)
lmalab <- expression(LMA~~(g~cm^-2))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

#for ablineclip
mod_lma_mesothick3 <- lm(LMA~meso.mean.y, data=ac)
mod_lma_mesothick5 <- lm(LMA~meso.mean.y, data=low)

library(scales)

#LMA and mesophyll thickness
windows(7,7)
par(oma=c(5,5,1,1), cex.axis=1)

par(mar=c(0,0,0,0),xpd=TRUE )
plot(LMA~meso.mean.y, data=gmes2,ylim=c(100, 280), xlim=c(200, 400), type='n')
plotrix::ablineclip(mod_lma_mesothick3, x1=min(ac$meso.mean), x2=max(ac$meso.mean),
                    lwd=2, lty=1)
plotrix::ablineclip(mod_lma_mesothick5, x1=min(low$meso.mean), x2=max(low$meso.mean),
                    lwd=2, lty=2)
points(LMA~meso.mean.y, data=gmes2,col=co2grow, pch=pchs[canopy], cex=1.5)
mtext(side=1, at=300, line=3,text=expression(Mesophyll~thickness~~(mu*m)),
      xpd=TRUE, las=1, cex=1)
mtext(side=2, at=190, line=3,text=lmalab,xpd=TRUE, las=3, cex=1)
legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)

dev.copy2pdf(file= "output/lma_anatomy.pdf")
dev.off()
