#drawdown and gm vs LMA

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("master_scripts/Master_data_file_clean.csv")
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

#plot objects-----------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))

cols <- c("black", "black", "red", "red")
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
pchs <- c(16,17)
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

mod_thick_gm <- lm(gmes~sumlength.par.mean, data=gmes2)

#plotting---------------------------------------
windows()
par(mar=c(5,6,1,1))
plot(gmes~sumlength.par.mean, data=gmes2,  ylab=gmlab, pch=c(16, 17)[canopy],
     col=c("black", "red")[co2grow])
legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)

###nothing here


windows()
par(mar=c(5,6,1,1))
plot(LMA~sumlength.par.mean, data=gmes2 , pch=c(16, 17)[canopy],
     ylim=c(100, 250), xlim=c(50, 150),
     col=c("black", "red")[co2grow])
# legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)