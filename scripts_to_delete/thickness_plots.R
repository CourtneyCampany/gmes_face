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

mod_thick_gm <- lm(gmes~sumlength.par.mean, data=gmes2)

#plotting---------------------------------------
# windows(12,6)

par(mar=c(5,6,1,1))
plot(gmes~sumlength.par.mean, data=gmes2,  ylab=gmlab, pch=c(16, 17)[canopy],
     col=c("black", "red")[co2grow])
###nothing here