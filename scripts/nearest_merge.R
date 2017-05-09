# Nearest merge

# One variable
df <- data.frame(latitude=seq(30.25, 50.25, by=0.5))
df$MAT <- runif(nrow(df),20,25)

plotloc <- data.frame(latitude = runif(10, 31,49))

diffun <- function(lat1, lat2)abs(lat1 - lat2)
ii <- sapply(1:nrow(plotloc), function(i)which.min(diffun(plotloc$latitude[i], df$latitude)))
plotloc$MAT <- df$MAT[ii]

# Two variables
df2 <- expand.grid(latitude=seq(40.25, 50.25, by=0.5), longitude=c(110, 115, 120))
df2$MAT <- 1:nrow(df2)

diffun2 <- function(lat1, lat2, lon1, lon2)abs(lat1-lat2) + abs(lon1-lon2)

plotloc2 <- data.frame(latitude = runif(10, 41,49), longitude=runif(10, 110,120))

ii <- sapply(1:nrow(plotloc2), 
             function(i)which.min(diffun2(plotloc2$latitude[i], 
                                          df2$lat, plotloc2$longitude[i], df2$lon)))
plotloc2$MAT <- df2$MAT[ii]