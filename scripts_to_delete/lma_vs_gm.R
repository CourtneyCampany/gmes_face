#drawdown and gm vs LMA

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("master_scripts/Master_data_file_clean.csv")
data2 <-read.csv("master_scripts/Tree-means_Gm-master2.csv")

gmes <- gmes_format_func(data)
gmes2 <- gmes_format_func2(data2)

#plot objects-----------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
drawdownlab <- expression(italic(C)[i]-italic(C)[c]~~(mu*mol~mol^-1))
lmalab <- expression(LMA~~(g~cm^-2))
cols <- c("black", "black", "red", "red")

lc<- expression(Low~canopy)
uc<- expression(High~canopy)
ac<- expression(Ambient~CO[2])
ec<- expression(Elevated~CO[2])

canopy <- c(lc, uc, lc, uc)
co2lab <- c(ac, ec)

mod_lma_draw <- lm(drawdown~LMA, data=gmes2)
mod_lma_gm <- lm(gmes~LMA, data=gmes2)

#plotting---------------------------------------
# windows(12,6)
par(mar=c(5,6,1,1))
plot(drawdown~LMA, data=gmes2, ylab=drawdownlab, xlab=lmalab, pch=c(16, 17)[canopy],
     col=c("black", "red")[co2grow], xlim=c(100, 300))
###nothing here

par(mar=c(5,6,1,1))
plot(gmes~LMA, data=gmes2, ylab=drawdownlab, xlab=lmalab, pch=c(16, 17)[canopy],
     col=c("black", "red")[co2grow])
###nothing here