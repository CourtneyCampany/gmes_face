###stomatal conductance and density

source("master_scripts/functions.R")

# read and format data ----------------------------------------------------
data <-read.csv("Master_data_file_clean.csv")
#test <-read.csv("Tree-means_Gm-master.csv") 
gmes <- gmes_format_func(data)
##subsets for ablineclips
ac <- gmes[gmes$co2grow == "amb",]
ec <- gmes[gmes$co2grow == "elev",]

palette(c("black", "red"))
pchs <- c(16,17)

plot(mean_gs ~ stomdenad, data=gmes, col=co2grow, pch=pchs[canopy])
plot(mean_gs ~ stomdenab, data=gmes, col=co2grow, pch=pchs[canopy])


fit_gsdens1 <- lm(mean_gs~stomdenad, data = gmes) 
summary(fit_gsdens1)
fit_gsdens2 <- lm(mean_gs~stomdenab, data = gmes) 
summary(fit_gsdens2)


fit_gsdens3 <- lm(mean_gs~stomdenab, data = ac) 
summary(fit_gsdens3)
fit_gsdens4 <- lm(mean_gs~stomdenab, data = ec) 
summary(fit_gsdens4)