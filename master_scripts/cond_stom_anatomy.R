###stomatal conductance and stomatal anatomy

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes2 <- gmes_format_func2(data2)

##subsets for ablineclips
ac <- gmes2[gmes2$co2grow == "amb",]
ec <- gmes2[gmes2$co2grow == "elev",]
low <- gmes2[gmes2$canopy == "lower",]
upp <- gmes2[gmes2$canopy == "upper",]

# library(sciplot)
# library(doBy)
# 
# stolen_means <- summaryBy(stomlengthad + stomlengthab ~ co2grow + position,
#                           data=gmes2, FUN=c(mean, se))

#stats----------------------------
# library(lme4)
# library(car)
# library(lmerTest)
# library(LMERConvenienceFunctions)
# library(MuMIn)
# library(visreg)
# 
# #gs and stomatal density (two sides of leaf)
# mod_cond_stomad <- lmer(mean_gs ~ stomdenad * co2grow * canopy + (1|ring/tree), data=gmes2,
#                         na.action = na.omit)
# Anova(mod_cond_stomad, test = "F") ###weak interaction
# r.squaredGLMM(mod_cond_stomad)
# 
# mod_cond_stomab <- lmer(mean_gs ~ stomdenab * co2grow * canopy + (1|ring/tree), data=gmes2,
#                         na.action = na.omit)
# Anova(mod_cond_stomab, test = "F") ###strong interaction with canopy and co2
# summary(mod_cond_stomab)
# r.squaredGLMM(mod_cond_stomab)
# visreg(mod_cond_stomab, "stomdenab", by= "co2grow", overlay =T)
# visreg(mod_cond_stomab, "stomdenab", by= "canopy", overlay =T)
# 
# fit_dens_ac <- lmer(mean_gs ~ stomdenab + (1|ring/tree),data=ac,na.action = na.omit)
# fit_dens_ec <- lmer(mean_gs ~ stomdenab + (1|ring/tree),data=ec,na.action = na.omit)
# summary(fit_dens_ac) ###postive correlation in ambient co2 (but only in upper canopy--see visreg)
# summary(fit_dens_ec)
# 

#gs and stomatal length 
# mod_cond_length.ad <- lmer(mean_gs ~ stomlengthad * co2grow * canopy + (1|ring/tree), data=gmes2,
#                         na.action = na.omit)
# Anova(mod_cond_length.ad, test = "F") #nothing with lengthad
# r.squaredGLMM(mod_cond_length.ad)
# summary(mod_cond_length.ad)
# 
# mod_cond_length.ab <- lmer(mean_gs ~ stomlengthab * co2grow * canopy + (1|ring/tree), data=gmes2,
#                            na.action = na.omit)
# Anova(mod_cond_length.ab, test = "F") #interaction with length and co2
# r.squaredGLMM(mod_cond_length.ab)
# summary(mod_cond_length.ab)
# 
# fit_length_ac <- lmer(mean_gs ~ stomlengthab + (1|ring/tree),data=ac,na.action = na.omit)
# fit_length_ec <- lmer(mean_gs ~ stomlengthab + (1|ring/tree),data=ec,na.action = na.omit)
# summary(fit_length_ac)
# summary(fit_length_ec)

#gs and spi
# mod_cond_spi.ad <- lmer(mean_gs ~ spi.ad  * co2grow * canopy + (1|ring/tree), data=gmes2,
#                         na.action = na.omit)
# Anova(mod_cond_spi.ad, test = "F") #nothing with spi ad
# r.squaredGLMM(mod_cond_spi.ad)

# mod_cond_spi.ab <- lmer(mean_gs ~ spi.ab  * co2grow * canopy + (1|ring/tree), data=gmes2,
#                         na.action = na.omit)
# Anova(mod_cond_spi.ab, test = "F") #p = 0.056 with spiab, co2 and canopy interaction
# r.squaredGLMM(mod_cond_spi.ab)
# summary(mod_cond_spi.ab)
# ###only the 3-way interaction is significant....im a little clueless how to interpret....
# visreg(mod_cond_spi.ab, "spi.ab", by= "co2grow", overlay =T)
# visreg(mod_cond_spi.ab, "spi.ab", by= "canopy", overlay =T)
# ##I think  that it is same as stomatal length but a little messier...
# library(effects)
# e <- allEffects(mod_cond_spi.ab)
# plot(e) #doesnt help much because I still dont know which relationships are significant

#plotting-------------------------
palette(c("black", "red"))
pchs <- c(16,17)
pchs2 <- c(1,2)
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
denslab <- expression(Stomatal~density~on~abaxial~surface~~(mm^2))
sllab <- expression(Stomatal~length~on~abaxial~surface~~(mu*m))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)
legpch2 <- c(1,1,1,2)

library(scales)
library(plotrix)

#simple models for regression lines
fit_length_ac2 <- lm(mean_gs ~ stomlengthab , data=ac,na.action = na.omit)
#special subset for regression line
acupp <- gmes2[gmes2$co2grow=="amb" & gmes2$canopy == "upper",]
fit_dens_acupp <- lm(mean_gs ~ stomdenab , data=acupp,na.action = na.omit)

windows(10,6)
par(mfrow=c(1,2), las=1, mgp=c(3,1,0), oma=c(5,5,1,1))

par(mar=c(0,0,0,0),xpd=TRUE )
plot(mean_gs ~ stomlengthab, data=gmes2, ylim=c(0,.5), xlim=c(20, 50), type='n',ylab="",xlab="")
points(mean_gs ~ stomlengthab, data=gmes2, col=co2grow, pch=pchs2[canopy], cex=1.25)
points(mean_gs ~ stomlengthab, data=ac, col=co2grow, pch=pchs[canopy], cex=1.25)
ablineclip(fit_length_ac2, x1=min(ac$stomlengthab), x2=max(ac$stomlengthab), lty=2, lwd=2)
mtext(side=2, at=.275, line=3,text=condlab, xpd=TRUE, las=3, cex=1.25)
mtext(side=1, at=35, line=3,text=sllab, xpd=TRUE, las=1, cex=1.25)
text('A', x=20, y=.5, cex=1.25)

par(mar=c(0,0,0,0), xpd=TRUE)
plot(mean_gs ~ stomdenab, data=gmes2,  ylim=c(0, .5), xlim=c(75, 250), type='n',ylab="", xlab="")
ablineclip(fit_dens_acupp, x1=min(acupp$stomdenab), x2=max(acupp$stomdenab), lty=2, lwd=2)
points(mean_gs ~ stomdenab, data=gmes2, col=co2grow, pch=pchs2[canopy], cex=1.25)
points(mean_gs ~ stomdenab, data=acupp, col=co2grow, pch=pchs[canopy], cex=1.25)
text('B', x=75, y=.5, cex=1.25)
mtext(side=1, at=162.5, line=3,text=denslab, xpd=TRUE, las=1, cex=1.25)
legend("topright", leglab, pch=legpch2, col=allcols,inset = 0.01, bty='n',cex=1)

dev.copy2pdf(file= "master_scripts/cond_anatomy.pdf")
dev.off()
