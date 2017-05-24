
# read and format data ----------------------------------------------------

gmes <-read.csv("master_data_file_clean.csv")
  gmes$Date <- as.Date(gmes$Date)
  gmes$treatment <- with(gmes, paste(co2grow, position, sep="-"))

#anatomy is doubled up so split dfr by CO2 measurement level (instantaneous effect) 
gmes_amb <- gmes[gmes$co2meas == "amb",]
gmes_elev <- gmes[gmes$co2meas == "elev",]

#or split by growth CO2 (FACE treatments)
gmes_amb2 <- gmes[gmes$co2grow == "amb",]
gmes_elev2 <- gmes[gmes$co2grow == "elev",]


# basic plots -------------------------------------------------------------
palette(c("black", "red"))
pchs <- c(16,17)
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

#try a bunch, keep the ones that may should a trend to analyze

#Physiology (growth CO2)
par(mar=c(5,5,1,1), cex=1.25, las=1, cex.axis=.8, cex.lab=1, mgp=c(3,1,0))

plot(Photo ~ gmes, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylim=c(0,30), xlim=c(0,.5), xlab=gmlab, ylab=photolab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)

plot(Photo ~ mean_gs, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylim=c(0,30), xlim=c(0,.4), xlab=condlab, ylab=photolab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)

plot(gmes ~ mean_gs, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylim=c(0,.5), xlim=c(0,.4),xlab=condlab, ylab=gmlab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
  
##make sure gm and cc are related
plot(gmes~mean_cc, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))

#make some informative boxplots  
boxplot(gmes~treatment, data=gmes_amb)
boxplot(mean_gs~treatment, data=gmes_amb)
boxplot(Photo~treatment, data=gmes_amb, ylim=c(0,25), ylab=photolab)

#Anatomy (growth CO2)
  
plot(gmes~mesolay.mean, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))
plot(gmes~length.par1.mean, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))

# plot(gmes~leafw, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))
# plot(gmes~mesolay.mean, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))
# plot(gmes~epi_up, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))
# plot(gmes~epi_low, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))
# plot(gmes~meso.mean, data=gmes_amb, col=co2grow, pch=pchs[canopy], ylab=gmlab,ylim=c(0,.5))






