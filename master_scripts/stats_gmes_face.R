### new stats table
source("master_scripts/functions.R")

gmes <-read.csv("master_scripts/Tree-means_Gm-master2.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]

#stats-------------------------------
library(lme4)
library(car)
library(lmerTest)
library(LMERConvenienceFunctions)
library(MuMIn)
library(lattice)

##1. Example for regrsssions:------------------------------------------

# gmes vs mesothickness
mod_gmes_meso <- lmer(gmes~meanlength.par.mean*co2grow*canopy + (1|ring/tree),
                 data=gmes_clean,na.action = na.omit)

  #model testing
  plot(mod_gmes_meso) 
  qqPlot(residuals(mod_gmes_meso)) # pretty good

Anova(mod_gmes_meso, test = "F")
r.squaredGLMM(mod_gmes_meso)
#overall gm is correlated with mesolength


### 2. Example for anatomical variables------------------------------------------

mod_leafthick <- lmer(leafw ~ co2grow*position + (1|ring/tree), data=gmes2, 
                 na.action = na.omit)

  #model testing
  plot(mod_leafthick) 
  qqPlot(residuals(mod_leafthick)) # pretty good
  Ltest <- leveneTest(leafw ~ co2grow * position, data = gmes2)
    summary(Ltest) # not signficant so variance are equal
  bwplot(leafw ~ position | co2grow , data = gmes2) 
  
#model output:    
Anova(mod_mesolay, test = "F")
r.squaredGLMM(mod_mesolay)
VarCorr(mod_mesolay)



