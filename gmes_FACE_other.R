###these plots are extracted from the gmes_FACE.R script
###they dont seem to participate in the story, so we have moved them out of the way


# read and format data ----------------------------------------------------

gmes <-read.csv("master_data_file_clean.csv")
  gmes$Date <- as.Date(gmes$Date)
  gmes$treatment <- with(gmes, paste(co2grow, position, sep="-"))
  gmes$Nm <- gmes$N.perc *10           # mass-based leaf N
  gmes$Na <- gmes$LMA * gmes$Nm / 1000 # Narea in g m-2

#subsets
ambco2 <- subset(gmes, co2grow == "amb" & co2meas == "amb")
elevco2 <- subset(gmes, co2grow == "elev" & co2meas == "elev")
growCO2 <- rbind(ambco2,elevco2)  # including Jmax and Vcmax!


# plot objects -------------------------------------------------------------
  palette(c("black", "red"))
  pchs <- c(16,17)
  gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
  condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
  photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
  leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
  allcols <- c("black", "red", "black", "black")
  legpch <- c(16,16,16,17)
  

# plotting ----------------------------------------------------------------

plot(gmes ~ mean_gs, data=growCO2, col=co2grow, pch=pchs[canopy], ylim=c(0,.5), xlim=c(0,.4),xlab=condlab, ylab=gmlab)
  legend("topleft", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
#no apparent coordination bewtween gm and gs
  
  
#tleaf is same and no apparent differnces in biochemical parameters for photosynthesis
  boxplot(Tleaf~treatment, data=growCO2, ylim=c(0,33), ylab="Tleaf")
  boxplot(Vcmax~treatment, data=growCO2, ylim=c(0,150), ylab="Vcmax")
  boxplot(Vcmax25~treatment, data=growCO2, ylim=c(0,120), ylab="Vcmax25")

#uninspiring chemistry and anatomy-----
  boxplot(leafw.mean.y~treatment, data=growCO2, ylim=c(0,450), ylab="leaf thickness (leafw)")
  boxplot(meso.mean.y~treatment, data=growCO2, ylim=c(0,450), ylab="mesomean.y")
  boxplot(stomdenab~treatment, data=growCO2, ylim=c(0,300), ylab="underside stom density")
  boxplot(stomdenad~treatment, data=growCO2, ylim=c(0,300), ylab="adax stom density")
  boxplot(N.perc~treatment, data=growCO2, ylim=c(0,2.5), ylab="leaf N")
  
  plot(gmes~mesolay.mean, data=growCO2, col=co2grow, pch=pchs[canopy], 
       ylab=gmlab,ylim=c(0,.5), xlim=c(6,10))
  
#gmes vs internal leaf anatomy -----
  plot(gmes~mesolay.mean, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab,
       ylim=c(0,.5))
  plot(gmes~epiupper.mean, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab,
       ylim=c(0,.5))
  plot(gmes~epilower.mean, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=gmlab,
       ylim=c(0,.5))
  

#Photo - N, Vcmax -N, LMA-N, anatomical relationships--------------
  plot(Vcmax~Na, data=growCO2, col=co2grow, pch=pchs[canopy], ylab="Vcmax", 
       xlab = "leaf Narea" ,ylim=c(0,120)) 
  legend("bottomright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
  
  plot(Jmax~Na, data=growCO2, col=co2grow, pch=pchs[canopy], ylab="Jmax", 
       xlab = "leaf Narea" ,ylim=c(0,200)) 
  legend("bottomright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=.8)
  
  plot(Photo~Na, data=growCO2, col=co2grow, pch=pchs[canopy], ylab=photolab, xlab = "leaf Narea" ,ylim=c(0,28)) 
  # No relationship with nitrogen with Vcmax, Jmax and Photo, likely the range is not large enough?
  # Obviously using cc can also convert to cc-based Vcmax verus ci-based Vcmax (or apparent Vcmax)
  
  