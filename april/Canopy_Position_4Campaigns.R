
### Analysis of anatomical measurements _ EucFACE_gmes leaves _ DE
# Clean up workspace
rm(list = ls())
library(doBy)
library(lattice)
library(lme4)
library(car)
library(visreg)
library(lmerTest)
library(sciplot)

# Set working directory
# setwd("//ad.uws.edu.au/dfshare/HomesHWK$/30021697/My Documents/Documents_Crous/EucFACE/Kristine-TDL/TDL_Oct2014/Campaigns_CanopyDiff")
#read in datafile
across <-read.csv("april/Tree_aver_age_position_EucFACE.csv") #data for four campaigns where we have upper and lower canopy results

str(across)

# not sure if we need this...as I already have tree data
# need to remove NAs before doing this
# mean.data <- summaryBy(LMA + LeafN + LeafC + LeafP + N.mg.g + N_area ~ Campaign + Treatment + Ring + Tree + Age + Cohort + LeafAge + LeafAgeNew + LeafAge100 + Year, data = df2, FUN = function(x) { c(m = mean(x), se = sd(x)/sqrt(length(x)))} )
# names(mean.data)[names(mean.data) %in% c("LMA.m","LMA.se", "LeafN.m","LeafN.se", "LeafC.m", "LeafC.se", "LeafP.m",  "LeafP.se", "N.mg.g.m",  "N.mg.g.se", "N_area.m", "N_area.se")] <- c("LMA", "SE", "LeafN", "SE","LeafC", "SE", "LeafP", "SE", "LeafN [mg/g]", "SE","N_area", "SE")
# rounding up: mean.data<- as.data.frame(lapply(mean.data,function(x)if(is.double(x))round(x,6)else x))
# levels(as.factor(mean.data$Campaign))

## Levene's test - homogenity of variance across groups #####
# It tests the null hypothesis that the population variances are equal - insignificant = not equal*</span></font>

test <- leveneTest(Photo ~ C.treat * Campaign, data = across)
summary(test)

# diagnostic plots: add after each model if you like ##############
  
plot(mixed)
par(mfrow = c(2, 2))
qqPlot(residuals(mixed))
visreg(mixed, "C.treat", by = "Campaign")
visreg(mixed, "Campaign", by = "C.treat", overlay = T, ylab = "Photo")
bwplot(Photo ~ C.treat | Campaign, data = Photo.all2)
bwplot(Photo ~ Campaign | C.treat , data = Photo.all2)
bargraph.CI(Campaign, Photo, C.treat, data = Photo.all2, legend = T, ylab = "Photo")


### Mixed model analyses ######
# need to add canopy position
# do we use Ring or tree nested within ring as the random factor?
# need to think about year (fixed or random)
# and what to do with leaf age (vs. campaign)
m1b <- lmer(Photo ~ C.treat*Position + (1|Ring), data=across, na.action = na.omit)
Anova(m1b, test = "F")
# and it worked! significant cO2 treatment and very sign canopy position effect.

m1b2 <- lmer(Photo ~ C.treat*Position* Campaign + (1|Ring), data=across, na.action = na.omit)
Anova(m1b2, test = "F")
VarCorr(m1b2)
plot(m1b2)
# so campaing is different and canopy position by campaign interaction. This could be a leaf age effect...


m1b3 <- lmer(Photo ~ C.treat * Position * Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1b3, test = "F")
# so it is not leaf age because no signficant age effect in this dataset, so could campaign effect be a year effect?

m1b4 <- lmer(Photo ~ C.treat*Position* Campaign * as.factor(Year) + (1|Ring), data=across, na.action = na.omit)
Anova(m1b4, test = "F")
# does not work with year... but it is probably fine to look across years here (only 3 years and 4 campaigns)
# What if year is a random variable, first need to convert year as factor
across$Year <- as.factor(across$Year)
m1b4 <- lmer(Photo ~ C.treat*Position* Campaign  + (1|Ring/Tree) + (1|Year), data=across, na.action = na.omit)
Anova(m1b4, test = "F")
# I basically get the same result (with warnings) so year does not matter!

m1f2 <- lmer(Photo ~ C.treat*Position* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f2, test = "F")
# doing tree nested within ring seems to make the outcome stronger (i.e. more significant) except for CO2 treatm
# summary(m1f2)
# slight CO2 treatment (P = 0.07), strong position and Campaign, with Position:Campaign interaction
visreg(m1f2, "Campaign", by= "Position", overlay = T, ylab = "Photo")

m1f2b <- lmer(Photo ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f2b, test = "F")
# sign C.treat at P = 0.05 and strong Position, no interactions

plot(m1f2b) 
qqPlot(residuals(m1f2b))
# looks good
test <- leveneTest(Photo ~ C.treat * Campaign, data = across)
summary(test) # significant

visreg(m1f2b, "Age", by = "Position", overlay = T, ylab = "Photo")
visreg(m1f2, "Campaign", by = "Position", overlay = T, ylab = "Photo") # Feb 2016 higher gs in lower canopy?
visreg(m1f2, "Campaign", by = "C.treat", overlay = T, ylab = "Photo") # perhaps sign for Oct2014 only
bwplot(Photo ~ Campaign | Position, data=across)
bargraph.CI(Age, Photo, Position, data = across, legend = T, ylab = "Photo") 
bargraph.CI(PosAge, Photo, C.treat, data = across, legend = T, ylab = "Photo")

boxplot(Photo~C.treat, data=across)
boxplot(Photo~Position, data=across)
boxplot(Photo~Campaign+Position, data=across, ylab = "Photo")
boxplot(Photo~Age+Position, data=across, ylab = "Photo")


#----------------------------------------------------------------------------------------------------
##################
# Variable: Vcmax
##################

## MIXED MODEL ####
###################

m1f3 <- lmer(Vcmax ~ C.treat*Position* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f3, test = "F")
VarCorr(m1f3)
# Can we replace Campaing with leaf age given that 2 campaigns are similar with 2 others?
m1f3b <- lmer(Vcmax ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f3b, test = "F")
# so both show very sign Position and Campaign difference, but m1f3 shows Position:Campaign interaction whereas 
# m1f3b shows (perhaps) a C.treat:Age interaction
# Comparing models
anova(m1f3,m1f3b) # the model with campaign seems to be  better, lower AIC score (992 versus 1003)
library(pbkrtest) # other ways to compare models
KRmodcomp(m1f3,m1f3b)
AIC(m1f3,m1f3b)

# So what does this Position by Campaign interaction mean? Create a new variable:
across$PosCamp <- paste(across$Position, across$Campaign, sep='-')
across$PosCamp <- as.factor(across$PosCamp)
m1f3c <- lmer(Vcmax ~ C.treat*PosCamp + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f3c, test = "F")
visreg(m1f3c, "PosCamp", by = "C.treat", overlay = T, ylab = "Vcmax") 

across$PosAge <- paste(across$Position, across$Age, sep = '-')
across$PosAge <- as.factor(across$PosAge)
m1f3d <- lmer(Vcmax ~ C.treat*PosAge* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f3d, test = "F")
visreg(m1f3d, "PosAge", by = "C.treat", overlay = T, ylab = "Vcmax")
# there may be a CO2 effect in upper new only? t-test
upnew <- subset(across, PosAge == "upper-new")
t.test(Vcmax~C.treat, data=upnew, var.equal=TRUE) # no not different

visreg(m1f3d, "Campaign", by = "PosAge", overlay = T, ylab = "Vcmax")


# Testing the assumptions ######
################################

plot(m1f3) 
qqPlot(residuals(m1f3))
# looks good
test <- leveneTest(Vcmax ~ C.treat * Campaign, data = across)
summary(test) # when significant, then there are no equal variances
## so qqplot shows normality, residuals are fine but variances are not equal ==> is there an outlier?

# Another way of doing it:
# E2 <- resid(m1f3, type="pearson", scaled=T)
# F2 <- fitted(m1f3)
# plot(E2 ~ F2) #, xlab="Fitted values", ylab=mylab)
# abline(h=0, lty=2, lwd=2, col="grey")
# qqnorm(E2)

#Visualising the data #####
###########################

# visreg(m1f3, "C.treat", by = "Campaign")
visreg(m1f3, "Campaign", by = "C.treat", overlay = T, ylab = "Vcmax")
visreg(m1f3, "Position", by = "C.treat", overlay = T, ylab = "Vcmax")
# bwplot(Vcmax ~ C.treat | Campaign, data = across)
bwplot(Vcmax ~ Campaign | C.treat , data = across)
bwplot(Vcmax ~ Campaign | C.treat:Position, data=across)
bwplot(Vcmax ~ Position | Age:C.treat, data=across)
# so from the various combinations we learn that:
      ## Age can be a replacement for combining the two OLD leaf campaigns (Oct and May 2014) and the two NEW leaf campaigns (Feb 2015, 2016)
      ## there is likely no difference in elevated CO2 (as Anova showed)
      ## there could be a canopy position effect, but the variaton in the NEW campaigns is much larger compared to the OLD leaf campaigns
bwplot(Vcmax ~ Campaign | Position, data=across)
# Position is highly significant in the model but there are overlaping whiskers in the boxplot with means only somewhat higher
bargraph.CI(Campaign, Vcmax, C.treat, data = across, legend = T, ylab = "Vcmax") # no CO2 treatment
bargraph.CI(Campaign, Vcmax, Position, data = across, legend = T, ylab = "Vcmax")# Position only in Feb2015 and perhaps in Oct2014
bargraph.CI(Age, Vcmax, Position, data = across, legend = T, ylab = "Vcmax")
# no interaction Vcmax is higher in new compared to old leaves and always higher in upper canopy than lower canopy regardless of leaf age

# well I think I settled on what mixed model to use but I  need posthoc test to interpret exactly what campaign means.
# perhaps we can use AIC to compare models where smaller value is the better model

# POSTHOC tests #############
#############################

install.packages("multcomp")
library(multcomp)
# Tukey is only usefull without the interactions, so let's look at some multiple comparisons for Campaign 
# to understand what is going on with the main factors
mVcmax <- lmer(Vcmax ~ C.treat + Position + Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(mVcmax, linfct=mcp(Campaign = "Tukey")))
# ok so from the posthoc test, Feb 2016 and 2015 are not different, and Oct 2014 is not different from May 2014
# so that would again fall out with Age, and suggests that campaign can be combined despite a higher AIC number
# it would make interpretation easier :-)
summary(glht(mVcmax, linfct=mcp(C.treat = "Tukey"))) # P=0.0431 in posthoc?
summary(glht(mVcmax, linfct=mcp(Position = "Tukey")))# very significant!!

m2Vcmax <- lmer(Vcmax ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(m2Vcmax, linfct=mcp(Position = "Tukey")))# very significant!!
summary(glht(m2Vcmax, linfct=mcp(Age = "Tukey"))) # very significant, age here is across campaigns
summary(glht(m2Vcmax, linfct=mcp(C.treat = "Tukey")))# P = 0.0497, still under the line (sign), although this does not come out in the overal model


#-----------------------------------------------------------------------------------------------------
# Variable Jmax
################

# Same approach as above but more protracted...
# Mixed model 
m1f4 <- lmer(Jmax ~ C.treat*Position* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f4, test = "F")
m1f4b <- lmer(Jmax ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f4b, test = "F")
# postion by campaign interaction, when Campaign is replaced with Age we get a Position by Age interaction
# We are not really interested in Campaign here...
# posthoc test whether the summer campaigns (new leaves) and non-summer campaigns (old leaves) can be merged
mJmax <- lmer(Jmax ~ C.treat + Position + Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(mJmax, linfct=mcp(Campaign = "Tukey")))
# here we learn that Feb campaings can be merged, but somehow the Oct2014 and May 2014 are different from one another


## Visualising data
m1f4c <- lmer(Jmax ~ C.treat*Age* Position + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f4c, test = "F")
visreg(m1f4c, "Age", by = "Position", overlay = T, ylab = "Jmax")
visreg(m1f4, "Campaign", by = "Position", overlay = T, ylab = "Jmax")
visreg(m1f4, "Campaign", by = "C.treat", overlay = T, ylab = "Jmax") # same as Vcmax
bwplot(Jmax ~ Campaign | Position, data=across)
bargraph.CI(Campaign, Jmax, C.treat, data = across, legend = T, ylab = "Jmax") # no CO2 treatment
bargraph.CI(Campaign, Jmax, Position, data = across, legend = T, ylab = "Jmax")# Position in all campaigs but certainly Feb campaigns
bargraph.CI(Age, Jmax, Position, data = across, legend = T, ylab = "Jmax") 
# Position:Age interaction explained by reduction in Jmax in upper campaign from new to old but not in the lower.

## Testing assumptions
plot(m1f4) 
qqPlot(residuals(m1f4))
# looks good
test <- leveneTest(Jmax ~ C.treat * Campaign, data = across)
summary(test) # not significant here

## further posthoc tests
summary(glht(mJmax, linfct=mcp(C.treat = "Tukey"))) # ns
summary(glht(mJmax, linfct=mcp(Position = "Tukey")))# very significant!!

m2Jmax <- lmer(Jmax ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)
summary(glht(m2Jmax, linfct=mcp(Position = "Tukey")))# very significant!!
summary(glht(m2Jmax, linfct=mcp(Age = "Tukey"))) # very significant, age here is across campaigns
summary(glht(m2Jmax, linfct=mcp(C.treat = "Tukey")))# ns

#-------------------------------------------------------------------------------------------------
# LMA ################
######################
m1f5 <- lmer(LMA ~ C.treat*Position* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f5, test = "F")

## mhhh, a C.treat:Position interaction (P = 0.038), but no CO2 effect
visreg(m1f5, "C.treat", by = "Position", overlay = T, ylab = "LMA")
# So LMA only changes in the upper canopy between CO2 treatments while there is no CO2 effect on the lower canopy

m1f5b <- lmer(LMA ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f5b, test = "F")
# Using Age in the model gives fairly different effects with Position:Age interaction (and a slight C.treat:Age interaction)
visreg(m1f5b, "C.treat", by = "Age", overlay = T, ylab = "LMA")
# LMA only increased in eCO2 in new leaves, but no CO2 effect in old leaves where LMA's between C.treat are the same
# So perhaps only a CO2 effect in upper new leaves
bwplot(LMA ~ C.treat | Age:Position, data=across)
bwplot(LMA ~ C.treat | PosAge, data=across)

m1f5c <- lmer(LMA ~ C.treat*PosAge* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f5c, test = "F")
visreg(m1f5c, "Campaign", by = "PosAge", overlay = T, ylab = "LMA") # misleading
visreg(m1f5b, "Age", by = "Position", overlay = T, ylab = "LMA")
visreg(m1f5, "Campaign", by = "Position", overlay = T, ylab = "LMA") # better, LMA lower> LMA upper in Feb 2015?
visreg(m1f5, "Campaign", by = "C.treat", overlay = T, ylab = "LMA") # perhaps sign for Feb2016 only
bwplot(LMA ~ Campaign | Position, data=across)
bargraph.CI(Age, LMA, Position, data = across, legend = T, ylab = "LMA") 
bargraph.CI(PosAge, LMA, C.treat, data = across, legend = T, ylab = "LMA") 

## Testing assumptions
plot(m1f5) 
qqPlot(residuals(m1f5))
# looks good
test <- leveneTest(LMA ~ C.treat * Campaign, data = across)
summary(test) # not significant here

# some posthoc tests - drop interactions from the model
phLMA1 <- lmer(LMA ~ C.treat + Position + Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
phLMA2 <- lmer(LMA ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)

summary(glht(phLMA1, linfct=mcp(Campaign = "Tukey"))) # summer and non-summer campaigns are not different, but Feb campaigns are different from Oct2014
summary(glht(phLMA1, linfct=mcp(Position = "Tukey"))) # very significant!!
summary(glht(phLMA1, linfct=mcp(C.treat = "Tukey")))  # ns

summary(glht(phLMA2, linfct=mcp(Age = "Tukey")))      # P = 0.02
summary(glht(phLMA2, linfct=mcp(Position = "Tukey"))) # very significant!!
summary(glht(phLMA2, linfct=mcp(C.treat = "Tukey")))  # ns

#------------------------------------------------------------------------------------------------
# gs ##########
###############

m1f6 <- lmer(gs ~ C.treat*Position* Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f6, test = "F")
# small Position effect, large Campaign effect, some interactions C.treat:Campaign and Position:Campaign
bwplot(gs ~ Position, data=across)
visreg(m1f6,"C.treat", by = "Campaign", overlay = T, ylab = "gs")
visreg(m1f6,"Campaign", by = "C.treat", overlay = T, ylab = "gs")
visreg(m1f6,"Campaign", by = "Position", overlay = T, ylab = "gs")


m1f6b <- lmer(gs ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f6b, test = "F")
# nothing is different except for a CO2 by Age interaction
visreg(m1f6b,"C.treat", by = "Age", overlay = T, ylab = "gs")
# old has significantly reduced gs in eCO2 whereas new leaves do not differ in gs between cO2 treatments

## Testing assumptions
plot(m1f6) 
qqPlot(residuals(m1f6))
# looks great
test <- leveneTest(gs ~ C.treat * Campaign, data = across)
summary(test) #  significant 

## More visualising
visreg(m1f6b, "Age", by = "Position", overlay = T, ylab = "gs")
visreg(m1f6, "Campaign", by = "Position", overlay = T, ylab = "gs") # Feb 2016 higher gs in lower canopy?
visreg(m1f6, "Campaign", by = "C.treat", overlay = T, ylab = "gs") # perhaps sign for Oct2014 only
bwplot(gs ~ Campaign | Position, data=across)
bargraph.CI(Age, gs, Position, data = across, legend = T, ylab = "gs") 
bargraph.CI(PosAge, gs, C.treat, data = across, legend = T, ylab = "gs") # CO2 effect in old leaves both in upper and lower, yes see above!

# some posthoc tests - drop interactions from the model
phgs1 <- lmer(gs ~ C.treat + Position + Campaign + (1|Ring/Tree), data=across, na.action = na.omit)
phgs2 <- lmer(gs ~ C.treat + Position + Age + (1|Ring/Tree), data=across, na.action = na.omit)

summary(glht(phgs1, linfct=mcp(Campaign = "Tukey"))) # OCT and MAY 2014 are VERY different, with Feb campaigns also being different with non-summer but not from each other
summary(glht(phgs1, linfct=mcp(Position = "Tukey"))) # P = 0.055
summary(glht(phgs1, linfct=mcp(C.treat = "Tukey")))  # sign P = 0.03

summary(glht(phgs2, linfct=mcp(Age = "Tukey")))      # ns
summary(glht(phgs2, linfct=mcp(Position = "Tukey"))) # ns
summary(glht(phgs2, linfct=mcp(C.treat = "Tukey")))  # sign. 0.032


#------------------------------------------------------------------------------------------------
# Leaf N ##########
###############
m1f7b <- lmer(Leaf.N ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f7b, test = "F")

visreg(m1f7b,"C.treat", by = "Age", overlay = T, ylab = "LeafN")
#------------------------------------------------------------------------------------------------
# Openness in canopy ##########
###############
m1f8b <- lmer(log.Open. ~ C.treat*Position* Age + (1|Ring/Tree), data=across, na.action = na.omit)
Anova(m1f8b, test = "F")
