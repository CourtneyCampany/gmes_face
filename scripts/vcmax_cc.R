##relationships with vcmax_cc

vcmax <- read.csv("april/vcmax_cc.csv")


##need to match with gm values 

gm <- read.csv("april/Tree-means_Gm-master2.csv")

gm_new <- gm[complete.cases(gm$Vcmax),c(1:40, 53:54)]


test <- merge(vcmax, gm_new)
