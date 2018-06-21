### new stats table
source("master_scripts/functions.R")

gmes <-read.csv("master_scripts/Tree-means_Gm-master2.csv")
gmes2 <- gmes_format_func2(gmes)

##if analyzing gmes, drop tree 422
gmes_clean <- gmes2[gmes2$tree != 422,]

##remko created a series of functions to make nice confidence intervals 
##using simple linear regressions: addpoly, ablinepeice and predline

##those functions are already added to the functions.R script.

##I use them constantly, and can easily add them to final plots
##I probably left them out because with treatments they can make plots super busy
##better to decide at the end and I can generate them

library(scales) #for color transparency
library(plotrix) #for ablineclip

##simple models
model1 <- lm(gmes~meanlength.par.mean, data = gmes_clean)
model1aco2 <- lm(gmes~meanlength.par.mean, data = gmes_clean[gmes_clean$co2grow == "amb",])
model1eco2 <- lm(gmes~meanlength.par.mean, data = gmes_clean[gmes_clean$co2grow == "elev",])


##1. predline example (unfortunatley not a lot of easy customization)
plot(gmes~meanlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
predline(model1, col="grey20",lwd=2, lty=2)

plot(gmes~meanlength.par.mean, data = gmes_clean, type='n')
predline(model1aco2, col="black",lwd=2, lty=2)
predline(model1eco2, col="red",lwd=2, lty=2)
points(gmes~meanlength.par.mean, data = gmes_clean, pch=19, col = co2grow)


##2.  alternatively you can predict from the model (lots of code but works great)

#length par values for one treatment
par_dat_amb <- gmes_clean[gmes_clean$co2grow == "amb", "meanlength.par.mean"]

#make a sequence across all the data
length_seq_amb <- seq(min(par_dat_amb), max(par_dat_amb), length=101)

#predict from model
gm_pred_amb <- predict(model1aco2, newdata=data.frame(meanlength.par.mean=length_seq_amb), 
                       se.fit=TRUE)

#conf interval from model fit
par_upr_amb <- gm_pred_amb$fit + (2*gm_pred_amb$se.fit)
par_lwr_amb <- gm_pred_amb$fit - (2*gm_pred_amb$se.fit)


plot(gmes~meanlength.par.mean, data = gmes_clean, type='n')
points(par_upr_amb~length_seq_amb, col="black",lwd=2, lty=2, type='l')
points(par_lwr_amb~length_seq_amb, col="black",lwd=2, lty=2, type='l')
ablineclip(model1aco2, x1=min(par_dat_amb), x2=max(par_dat_amb), lwd=2)
points(gmes~meanlength.par.mean, data = gmes_clean, pch=19, col = co2grow)
