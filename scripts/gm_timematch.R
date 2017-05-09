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


####read all tdl files and run tdl formating and xsi functions on each list element-------------------------------------
  names<- list.files(path="tdl_files/",pattern="csv",full.names=TRUE)

  tdl_files <- llply(list.files(path="tdl_files/",pattern="csv",full.names=TRUE),function(filename) {
  dat=read.csv(filename)
  })

  ####format tdl data (will be csv covering a day and a ref/sample line with samples within)
  tdl_formatted <- llply(tdl_files, tdlformat_func)

  ####calculatre xsi/Delta with times by licor id
  xsi_face <- llply(tdl_formatted, function(x)  xsicalc_func(x))
  xsi_dfr <- llply(xsi_face, function(x) data.frame(x))

  ####name each list by filename and export to global environment as dfr
  names2 <- gsub("tdl_files/", "", names)
  names2 <- gsub(".csv", "", names2)
  
  xsi_dfr2 <- setNames(xsi_dfr, names2)
  list2env(lapply(xsi_dfr2, as.data.frame), .GlobalEnv)


##gm ready dataframe function-------------------------------------------------------------------------------
#licor_values <- unique(licor_gmes$licor)

#oct22
oct22_1920 <- gmesdata_func(tdl_oct22_1920, licor_gmes, licor_times, whichlicor="f4")
oct22_2122 <- gmesdata_func(tdl_oct22_2122, licor_gmes, licor_times, whichlicor="f2")

#oct23
oct23_1920 <- gmesdata_func(tdl_oct23_1920, licor_gmes, licor_times, whichlicor="f4")
oct23_2122 <- gmesdata_func(tdl_oct23_2122, licor_gmes, licor_times, whichlicor="f2")

#oct27
oct27_1920 <- gmesdata_func(tdl_oct27_1920, licor_gmes, licor_times, whichlicor="f4")
oct27_2122 <- gmesdata_func(tdl_oct27_2122, licor_gmes, licor_times, whichlicor="f2")

#oct28
oct28_1920 <- gmesdata_func(tdl_oct28_1920, licor_gmes, licor_times, whichlicor="f4")
oct28_2122 <- gmesdata_func(tdl_oct28_2122, licor_gmes, licor_times, whichlicor="f2")

#oct29
oct29_1920 <- gmesdata_func(tdl_oct29_1920, licor_gmes, licor_times, whichlicor="f4")
oct29_2122 <- gmesdata_func(tdl_oct29_2122, licor_gmes, licor_times, whichlicor="f2")

#oct30
oct30_1920 <- gmesdata_func(tdl_oct30_1920, licor_gmes, licor_times, whichlicor="f4")
oct30_2122 <- gmesdata_func(tdl_oct30_2122, licor_gmes, licor_times, whichlicor="f6")

##calculate gmes---------------------------------------------------------------------------------------------
gm_oct22_f4 <- gmcalc_func(oct22_1920 )
gm_oct22_f2 <- gmcalc_func(oct22_2122 )
gm_oct23_f4 <- gmcalc_func(oct23_1920 )
gm_oct23_f2 <- gmcalc_func(oct23_2122 )
gm_oct27_f4 <- gmcalc_func(oct27_1920 )
gm_oct27_f2 <- gmcalc_func(oct27_2122 )
gm_oct28_f4 <- gmcalc_func(oct28_1920 )
gm_oct28_f2 <- gmcalc_func(oct28_2122 )
gm_oct29_f4 <- gmcalc_func(oct29_1920 )
gm_oct29_f2 <- gmcalc_func(oct29_2122 )
gm_oct30_f4 <- gmcalc_func(oct30_1920 )
gm_oct30_f6 <- gmcalc_func(oct30_2122 )

##rbind these these by pattern in global environment??????
##then summarise by id



