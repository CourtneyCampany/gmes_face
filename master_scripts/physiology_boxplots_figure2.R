##physiology boxplots

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("master_scripts/Master_data_file_clean.csv")
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

not422 <- gmes2[gmes2$tree != "422",]
not408 <- gmes2[gmes2$tree != "408",]

#plot objects-----------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
condlab <- expression(italic(g)[s]~~(mol~m^-2~s^-1))
cols <- c("black", "black", "red", "red")
cilab <- expression(italic(C)[i]~~(ppm))
cclab <- expression(italic(C)[c]~~(ppm))
cols <- c("black", "black", "red", "red")

lc<- "Low"
uc<- "High"
ac<- expression(Ambient~CO[2])
ec<- expression(Elevated~CO[2])

canopy <- c(lc, uc, lc, uc)
co2lab <- c(ac, ec)

#plotting---------------------------------------
windows()

par(mfrow=c(2,2), oma=c(5,5,1,5),mar=c(0,0,0,0), cex.lab=1.25)
#gs
boxplot(mean_gs~treatment, gmes2, ylab="",names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols, xaxt='n')
text(x=.55, y=.55, "A", cex=1.25)
mtext(condlab, side=2, line=3)

#gm
boxplot(gmes~treatment, gmes2, ylab="",names=FALSE, ylim=c(0,.55), 
        outline=FALSE, border=cols, xaxt='n', yaxt='n')
text(x=.55, y=.55, "B", cex=1.25)
mtexti(gmlab, 4, outer=TRUE, cex=1.25, off=.6)
axis(4, labels=TRUE)

#ci
boxplot(mean_ci~treatment, gmes2, ylab="",names=FALSE, ylim=c(0,435), 
        outline=FALSE, border=cols)
mtext(canopy, side=1,at=1:4, line=1, cex=.9)
mtext(co2lab, side=1, at=c(1.5, 3.5),line=3, cex=.9)
text(x=.55, y=430, "C", cex=1.25)
mtext(cilab, side=2, line=3)

#cc
boxplot(mean_cc~treatment, gmes2, ylab="",names=FALSE, ylim=c(0,435), 
        outline=FALSE, border=cols, yaxt='n')
text(x=.55, y=430, "D", cex=1.25)
axis(4, labels=TRUE)
mtexti(cclab, 4, outer=TRUE, cex=1.25,off=.6)
mtext(canopy, side=1,at=1:4, line=1, cex=.9)
mtext(co2lab, side=1, at=c(1.5, 3.5),line=3, cex=.9)

dev.copy2pdf(file= "master_scripts/phys_box.pdf")
dev.off()
