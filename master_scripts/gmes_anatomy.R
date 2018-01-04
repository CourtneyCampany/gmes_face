##gmes and anatomy 

#gmes and leaf internal anatomy
source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("Master_data_file_clean.csv")
#test <-read.csv("Tree-means_Gm-master.csv") 
gmes <- gmes_format_func(data)
##subsets for ablineclips
ac <- gmes[gmes$co2grow == "amb",]
ec <- gmes[gmes$co2grow == "elev",]

#plot objects-------------------------------
gmlab <-expression(italic(g)[m]~~(mol~m^-2~s^-1))
tpc_lab <- expression(Parenchyma~cell~thickness~~(mu*m))
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)
palette(c("black", "red"))
pchs <- c(16,17)
leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")

# plotting----------------------------------------

#####gm vs average parencyhma cell length
fit_gmpara1 <- lm(gmes~meanlength.par.mean, data = ac) 
summary(fit_gmpara1)
fit_gmpara2 <- lm(gmes~meanlength.par.mean, data = ec) 
summary(fit_gmpara2)

summary(fit_meanlength) # p = 0.03 (negative reltionship, R2 = 0.14)   
plot(gmes~meanlength.par.mean, data=gmes, col=co2grow, pch=pchs[canopy], 
     ylab=gmlab,xlab = tpc_lab, ylim=c(0,.5), xlim=c(20, 40))

plotrix::ablineclip(fit_gmpara1, lwd=2, lty=2, col="black",
                    x1= min(ac$meanlength.par.mean), x2=max(ac$meanlength.par.mean))
plotrix::ablineclip(fit_gmpara2, lwd=2, lty=2, col="red",
                    x1= min(ec$meanlength.par.mean), x2=max(ec$meanlength.par.mean))


###need to reconcile variables

###maybe we should choose one of the following 3 (most supported in literature)?
fit_gmlength <- lm(gmes~length.par1.mean, data = gmes) 
summary(fit_gmlength)
# p = 0.007 (negative reltionship, R2 = 0.22), 
#looks like eCO2 could be paralel but higher intercept compared to amb
plot(gmes~length.par1.mean, data=gmes, col=co2grow, pch=pchs[canopy], 
     ylab=gmlab,ylim=c(0,.5),xlim=c(20, 45))
#fit ablineclip here