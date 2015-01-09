source("functions/tdl_licor_functions.R")

#### load packages and data---------------------------------------------------------------------------------------------
library(data.table)

licor_master <- read.csv("licor_gm_master.csv")
tdl_oct22_2121 <- read.csv("tdl_oct22_2122.csv")


####format tdl data (will be csv covering a day and a ref/sample line with samples within)------------------------------
tdl_oct22_2121 <- tdlformat_func(tdl_oct22_2121)
####calculatre xsi/Delta with times
xsi_oct22_2121 <- xsicalc_func(tdl_oct22_2121)


####format master licor file, will have two dataframes one with all data, and one with times for matching---------------
licor_gmes <- licorformat_func(licor_master)
licor_times <- timerange_func(licor_gmes)


####once files are formatted subset the tdl files to range of licor datetime for any sample id-------------------------

#make a sample specific tdl and licor dfr
upper_116_tdl <- xsi_oct22_2121[xsi_oct22_2121$timeavg > as.POSIXct('2014-10-22 11:35:22') & 
                           xsi_oct22_2121$timeavg < as.POSIXct('2014-10-22 11:50:11 '), ]
  upper_116_tdl$id <- "116_upper_amb"

upper_116_licor <- licor_gmes[licor_gmes$id == "116-upper-amb",]
  
####time match upper116

timematch <- function(datetime1, datetime2) 

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

  
  
  


licor_gm2 <- subset(licor_gm, datetime >= min.dt & datetime <= max.dt)


