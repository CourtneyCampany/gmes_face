##relationships with vcmax_cc
vcmax <- read.csv("april/vcmax_cc.csv")


##need to match with gm values 
gm <- read.csv("april/Tree-means_Gm-master2.csv")
  gm_new <- gm[complete.cases(gm$Vcmax),c(1:40, 53:54)]


gm_vcmax <- merge(vcmax, gm_new, by="id")

leglab <- c(expression(aCO[2]), expression(eCO[2]), "lower canopy", "upper canopy")
pchs <- c(16,17)
allcols <- c("black", "red", "black", "black")
legpch <- c(16,16,16,17)

windows()
plot(Vcmax25 ~ gmes, data=gm_vcmax, col=co2grow, pch=pchs[canopy])
legend("topright", leglab, pch=legpch, col=allcols,inset = 0.01, bty='n',cex=1)

plot(Vcmax ~ gmes, data=gm_vcmax, col=co2grow, pch=19)

plot(drawdown ~ Vcmax25, data=gm_vcmax, col=co2grow, pch=19)
