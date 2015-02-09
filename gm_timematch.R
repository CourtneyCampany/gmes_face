source("functions/tdl_licor_functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

####read licor data and run licor formatting functions------------------------------------------------------------------
licor_master <- read.csv("licor_master2.csv")

####format master licor file, will have two dataframes one with all data, and one with times for matching
licor_gmes <- chooseidfunc(licor_master, c("Ring" , "Tree",  "Canopy",  "CO2"))
licor_gmes <- licorformat_func(licor_gmes)

###now run time range function if get samples id and time range
licor_times <- timerange_func(licor_gmes)



####read all tdl files and run tdl formating and xsi functions on each list of tdl files---------------------------------

# tdl_oct22_2122 <- read.csv("tdl_files/tdl_oct22_2122.csv")
# tdl_oct22_2122 <- tdlformat_func(tdl_oct22_2122)
# xsi_oct22_f2 <- xsicalc_func(tdl_oct22_2122)

names<- list.files(path="tdl_files/",pattern="csv",full.names=TRUE)
names2 <- gsub("tdl_files/", "", names)
names2 <- gsub(".csv", "", names2)

tdl_files <- llply(list.files(path="tdl_files/",pattern="csv",full.names=TRUE),function(filename) {
  dat=read.csv(filename)
  dat$filename=filename
  return(dat)
})


####format tdl data (will be csv covering a day and a ref/sample line with samples within)
tdl_formatted <- llply(tdl_files, tdlformat_func)

####calculatre xsi/Delta with times by licor id
xsi_face <- llply(tdl_formatted, function(x)  xsicalc_func(x))
xsi_dfr <- llply(xsi_face, function(x) data.frame(x))


# test2 <- xsi_dfr[2]
# test <- data.frame(xsi_dfr[2])

xsi_dfr2 <- setNames(xsi_dfr, names2)
list2env(lapply(xsi_dfr2, as.data.frame), .GlobalEnv)


##gm ready dataframe function
licor_values <- unique(licor_gmes$licor)

#oct22
gm_oct22_f2 <- gmesdata_func(tdl_oct22_1920, licor_gmes, licor_times, whichlicor="f2")
gm_oct22_f4 <- gmesdata_func(tdl_oct22_1920, licor_gmes, licor_times, whichlicor="f4")

#oct24
oct24_1920 <- gmesdata_func(tdl_oct24_1920, licor_gmes, licor_times, whichlicor="f2")
oct24_1920 <- gmesdata_func(tdl_oct24_1920, licor_gmes, licor_times, whichlicor="f4")


##calculate gmes
gm2_oct22_f2 <- gmcalc_func(gm_oct22_f2 )

##
gm_agg <- summaryBy(gm ~id, data=gm2_oct22_f2, FUN=c(mean, se))


