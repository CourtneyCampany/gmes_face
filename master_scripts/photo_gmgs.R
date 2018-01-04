#photo vs gm/gs panel
source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("Master_data_file_clean.csv")
#test <-read.csv("Tree-means_Gm-master.csv") 
gmes <- gmes_format_func(data)

#linear models for best fit lines----------------------
fit_ags <- lm(Photo ~ mean_gs, data = growCO2) 
summary(fit_ags) # significant relationship (P[1,26]=0.0002)  

fit_agm <- lm(Photo ~ mean_gm, data = growCO2) 
summary(fit_agm) # significant relationship (P[1,26]=0.0002)  


#plotpobjects--------------------------------------
palette(c("black", "red"))
pchs <- c(16,17)
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

#2panel plots----------------------------------------
windows(10,6)
par(mfrow=c(1,2), las=1, mgp=c(3,1,0), oma=c(6,6,1,1))

par(mar=c(0,0,0,0), xpd=TRUE)
plot(Photo ~ gmes, data=gmes, type='n', ylim=c(0,30), xlim=c(0,.55))
legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)
# predline(lma_pnue_mod, col="grey20",lwd=2, lty=2)
points(Photo ~ gmes, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=2, at=15, line=3,text=photolab, xpd=TRUE, las=3, cex=1.25)
mtext(side=1, at=.28, line=3,text=gmlab, xpd=TRUE, las=1, cex=1.25)
text('A', x=0, y=30, cex=1.25)

par(mar=c(0,0,0,0),xpd=TRUE )
plot(Photo ~ mean_gs, data=gmes,ylim=c(0,30), xlim=c(0, .55),
     yaxt='n',type='n')
axis(2, labels=FALSE)
# predline(lma_ppue_mod, col="grey20",lwd=2, lty=2)
points(Photo ~ mean_gs, data=gmes, col=co2grow, pch=pchs[canopy], cex=1.25)
mtext(side=1, at=.2, line=3,text=condlab, xpd=TRUE, las=1, cex=1.25)
text('B', x=0, y=30, cex=1.25)

dev.copy2pdf(file= "output/photo_gmgs.pdf")
dev.off()

###results needs to asses if different between canopies or co2 treatments, then can add correct
###model fit lines (if ns for either, then use one model line)