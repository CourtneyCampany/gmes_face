###stomatal conductance and density

########need to add in length and spi########## update stats as with others

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("Master_data_file_clean.csv")
data2 <-read.csv("Tree-means_Gm-master2.csv") 

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

##subsets for ablineclips
ac <- gmes2[gmes2$co2grow == "amb",]
ec <- gmes2[gmes2$co2grow == "elev",]
low <- gmes2[gmes2$canopy == "lower",]
upp <- gmes2[gmes2$canopy == "upper",]

#stats----------------------------
library(lme4)
library(car)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)

##gs and stomatal density
# mod_cond_stomad <- lmer(mean_gs ~ stomdenad + (1|ring/tree), data=gmes2, 
#                         na.action = na.omit)
# Anova(mod_cond_stomad, test = "F")
# fixef(mod_cond_stomad)
# summary(mod_cond_stomad)
# r.squaredGLMM(mod_cond_stomad)
# 
# mod_cond_stomab <- lmer(mean_gs ~ stomdenab + (1|ring/tree), data=gmes2, 
#                         na.action = na.omit)
# Anova(mod_cond_stomab, test = "F")
# fixef(mod_cond_stomab)
# summary(mod_cond_stomab)
# r.squaredGLMM(mod_cond_stomab)
# 
# ##Stomatal conductance was weakly correlated with stomatal density on the abaxial side
# ##for all leaves
# 
# mod_cond_stomab2 <- lmer(mean_gs ~ stomdenab + (1|ring/tree), data=ac, 
#                         na.action = na.omit)
# Anova(mod_cond_stomab2, test = "F") 
# 
# mod_cond_stomab3 <- lmer(mean_gs ~ stomdenab + (1|ring/tree), data=ec, 
#                         na.action = na.omit)
# Anova(mod_cond_stomab3, test = "F")
##only for aco2, does this matter?


#plotting-------------------------
palette(c("black", "red"))
pchs <- c(16,17)
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
denslab <- expression(Stomatal~Density~on~abaxial~surface~~(mm^2))
mod <- lm(mean_gs ~ stomdenab, data=gmes2)
library(scales)
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

windows(7,7)
par(mar=c(5,5,1,1), las=1)
plot(mean_gs ~ stomdenab, data=gmes2,  ylim=c(0, .5), xlim=c(75, 250), type='n',
     ylab=condlab, xlab=denslab)
predline(mod, col="grey20",lwd=2, lty=2)
points(mean_gs ~ stomdenab, data=gmes2, col=co2grow, pch=pchs[canopy], cex=1.25)
legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)

dev.copy2pdf(file= "output/cond_sd.pdf")
dev.off()
