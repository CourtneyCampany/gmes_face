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

##-----------------------------------------------------------------------------
## This is not quite what I wanted - I really want to see simple linear regression equations, R2 and p-value
levels(gmes_clean$co2grow)
palette(c("blue","red")) # with blue for ambient and red for elevated CO2
# in this case, the canopy position often helps extend the relationship axes but for completeness we will have to test
# significant models for interaction with canopy position, like we have done with CO2.
# if I have the overview right, then there are six relationships signficant out of the 18 tested!


plot(gmes~meanlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model1 <- lm(gmes~meanlength.par.mean, data = gmes_clean)
coef(model1) #this is what I want, but now with R2 and p-value so we need really the summary of the model
summary(model1)
confint(model1)
polygon(c(0.3218, 1.1528),col = "grey75", border = FALSE) # does not work, need predictions of the model?
abline(model1)
# so the intercept = 0.74 and slope = -0.018 (negative relationship), weak (R2 = 0.21) but significant (p = 0.014) relationship
# given the significan relationship, we investigate whether treatments are different (test in slope differences)
lmfit1 <- lm(gmes~meanlength.par.mean  * co2grow, data = gmes_clean)
summary(lmfit1)
# in this case the slope difference is P= 0.0637 so we may opt for separate treatment lines depending on our story (it's marginal!)
# now check for canopy position interaction
lmfit1c <- lm(gmes~meanlength.par.mean  * canopy, data = gmes_clean)
summary(lmfit1c) # no slope difference for canopy


plot(gmes~sumlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model1b <- lm(gmes~sumlength.par.mean, data = gmes_clean)
summary(model1b) # significant p = 0.014, not sure what the difference in between meanlenght and sumlength.par
# they are exactly the same except for the numbers on the x-axis

#Relationship between gm and leaf thickness
plot(gmes~leafw, data = gmes_clean, pch=19, col = co2grow)
model2 <- lm(gmes~leafw, data = gmes_clean)
abline(model2)
summary(model2)
#non-significant relationships p = 0.55, same outcome iwth leafw.mean.y

# relationshisp between gm and mean mesophyll thickness
plot(gmes~meso.mean, data = gmes_clean, pch=19, col = co2grow)
model3 <- lm(gmes~meso.mean, data = gmes_clean)
abline(model3)
summary(model3) #non-significant relationships p = 0.27

plot(gmes~sum.mean, data = gmes_clean, pch=19, col = co2grow)
model3b <- lm(gmes~sum.mean, data = gmes_clean)
abline(model3b)
summary(model3b) #non-significant relationships p = 0.40

plot(gmes~mesolay.mean, data = gmes_clean, pch=19, col = co2grow)
model3c <- lm(gmes~mesolay.mean, data = gmes_clean)
abline(model3c)
summary(model3c) #non-significant relationships p = 0.81

# relationship between gm and gs
plot(gmes~gs, data = gmes_clean, pch=19, col = co2grow)
model4 <- lm(gmes~gs, data = gmes_clean)
abline(model4)
summary(model4) #non-significant relationships p = 0.88

# relationship between gm and Ci-Cc (drawdown)
plot(gmes~drawdown, data = gmes_clean, pch=19, col = co2grow)
model5 <- lm(gmes~drawdown, data = gmes_clean)
abline(model5)
summary(model5) # not linear so need a different fit or we transform the vars

plot(gmes~mean_cc, data = gmes_clean, pch=19, col = co2grow)
model5b <- lm(gmes~mean_cc, data = gmes_clean)
abline(model5b)
summary(model5b) # very significant relationship, as expected, R2 = 0.42, p = 0.0003
#Test for slope differences
lmfit5b <- lm(gmes~mean_cc * co2grow, data = gmes_clean)
summary(lmfit5b) #and that shows no differences, so the plot is fit across CO2 treatments
lmfit5c <- lm(gmes~mean_cc * canopy, data = gmes_clean)
summary(lmfit5c) # and no slope differences between canopy positions

#------------------------------------------------------------------------
# LMA and gm and LMA and leaf thickness (leafw)
plot(gmes~LMA, data = gmes_clean, pch=19, col = co2grow)
model6 <- lm(gmes~LMA, data = gmes_clean)
abline(model6)
summary(model6) #non-significant relationships p = 0.75

plot(LMA~leafw, data=gmes_clean, pch=19, col = co2grow)
model7 <- lm(LMA~leafw, data = gmes_clean)
abline(model7)
summary(model7) #significant relationships p = 0.04, very weak with R2 = 0.14
# test for slope differences
lmfit7b <- lm(LMA~leafw * co2grow, data = gmes_clean)
summary(lmfit7b) #and that both slope and intercept differences (P = 0.014)
# how do I draw 2 lines in there for each treatment?
library(visreg)
visreg(lmfit7b, "leafw", by="co2grow", overlay=TRUE)
# so the weak relationship is driven by ambient CO2 while elevated CO2 does not change LMA with CO2 treatment (this is a bit strange)
lmfit7c <- lm(LMA~leafw * canopy, data = gmes_clean)
summary(lmfit7c) # and no slope differences between canopy positions



plot(LMA~sumlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
model14<- lm(LMA~sumlength.par.mean, data = gmes_clean)
abline(model14)
summary(model14) #non-significant relationships p = 0.64

# photosynthesis with gm and gs relationships
plot(Photo~gs, data=gmes_clean, pch=19, col = co2grow)
model8 <- lm(Photo~gs, data = gmes_clean)
abline(model8)
summary(model8)
# again a test if slopes are different with co2 treatment
lmfit2 <- lm(Photo~gs * co2grow, data = gmes_clean)
summary(lmfit2) #shows no different slope and a marginal difference in intercept (p=0.078), will ignore
lmfit2c <- lm(Photo~gs * canopy, data = gmes_clean)
summary(lmfit2c) #no differences in canopy

plot(Photo~mean_gm.Eglob, data=gmes_clean, pch=19, col = co2grow)
model9 <- lm(Photo~mean_gm.Eglob, data = gmes_clean)
abline(model9)
summary(model9)  #non-significant relationships p = 0.24
#BUT when fit with mean_gm.Eglob, then the relationship is significant!! p = 0.030 and R2 =0.16
lmfit4 <- lm(Photo~mean_gm.Eglob*co2grow, data = gmes_clean)
summary(lmfit4) # and no sign difference so relationship across treatments is fine.
lmfit4c <- lm(Photo~mean_gm.Eglob*canopy, data = gmes_clean)
summary(lmfit4c) # and no sign difference so relationship across treatments is fine.


c# gm-spi.ad
plot(gmes ~ spi.ad, data=gmes_clean, pch=19, col = co2grow)
model10 <- lm(gmes ~ spi.ad, data = gmes_clean)
abline(model10)
summary(model10) # not significant

plot(gs ~ spi.ad, data=gmes_clean, pch=19, col = co2grow)
model10b <- lm(gs ~ spi.ad, data = gmes_clean)
abline(model10b)
summary(model10b) # not significant, even when lower point of t523 is removed.


#----------------------------------------
# relationships with Vcmax25
# and nitrogen, Na
# and gm
# and LMA
plot(Vcmax25 ~ Na, data=gmes_clean, pch=19, col = co2grow)
model11 <- lm(Vcmax25 ~ Na, data = gmes_clean)
abline(model11)
summary(model11) #not significant p = 0.76

plot(gmes~Vcmax25, data=gmes_clean, pch=19, col = co2grow)
model12 <- lm(gmes~Vcmax25, data = gmes_clean)
abline(model12)
summary(model12) #not significant p = 0.44

plot(Vcmax25 ~ LMA, data=gmes_clean, pch=19, col = co2grow)
model13 <- lm(Vcmax25 ~ LMA, data = gmes_clean)
abline(model13)
summary(model13) #not significant p = 0.60

# we can redo these relationships including the co2measurement points as well. It usually does not make a big difference, see table in my notebook
# would be nice to add confidence intervals if possible, with polygon


