source("functions/tdl_licor_functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

####read data---------------------------------------------------------------------------------------------
licor_master <- read.csv("licor_master2.csv")
tdl_oct22_2122 <- read.csv("tdl_oct22_2122.csv")
tdl_oct22_1920 <- read.csv("tdl_oct22_1920.csv")

####format master licor file, will have two dataframes one with all data, and one with times for matching---------------
licor_gmes <- chooseidfunc(licor_master, c("Ring" , "Tree",  "Canopy",	"CO2"))
licor_gmes <- licorformat_func(licor_gmes)

###now run time range function if get samples id and time range
licor_times <- timerange_func(licor_gmes)


####format tdl data (will be csv covering a day and a ref/sample line with samples within)------------------------------
tdl_oct22_2122 <- tdlformat_func(tdl_oct22_2122)
tdl_oct22_1920 <- tdlformat_func(tdl_oct22_1920)

####calculatre xsi/Delta with times by licor id
xsi_oct22_f2 <- xsicalc_func(tdl_oct22_2122)
xsi_oct22_f4 <- xsicalc_func(tdl_oct22_1920)


##gm ready dataframe function
gm_oct22_f2 <- gmes_func(xsi_oct22_f2, licor_gmes, licor_times, whichlicor="f2")
gm_oct22_f4 <- gmes_func(xsi_oct22_f4, licor_gmes, licor_times, whichlicor="f4")

