#boxplots of gm gs An with canopy and eco2

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("master_scripts/Master_data_file_clean.csv")
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

#plot objects-----------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
cols <- c("black", "black", "red", "red")

aclc<- expression(atop(Ambient~CO[2]), Low~canopy)
acuc<- expression(atop(Ambient~CO[2]), High~canopy)
eclc<- expression(atop(Elevated~CO[2]), Low~canopy)
ecuc<- expression(atop(Elevated~CO[2]), High~canopy)

#plotting---------------------------------------
# windows(12,6)
par(mfrow=c(1,3), mar=c(5,5,1,1), cex.lab=1.25)

boxplot(gmes~treatment, gmes2, ylab=gmlab,names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=.55, "A", cex=1.25)

boxplot(mean_gs~treatment, gmes2, ylab=condlab,names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=.55, "B", cex=1.25)

boxplot(Photo~treatment, gmes2, ylab=photolab,names=FALSE, ylim=c(0,25), 
        outline=FALSE, border=cols)
mtext(aclc, side=1,at=1, line=2.5, cex=.7)
mtext(acuc, side=1,at=2, line=2.5, cex=.7)
mtext(eclc, side=1,at=3, line=2.5, cex=.7)
mtext(ecuc, side=1,at=4, line=2.5, cex=.7)
text(x=.55, y=25, "C", cex=1.25)

# dev.copy2pdf(file= "master_scripts/phys_box.pdf")
# dev.off()
