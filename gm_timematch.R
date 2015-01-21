source("functions/tdl_licor_functions.R")

###required packakges
library(data.table)
library(plyr)
library(doBy)

####read data---------------------------------------------------------------------------------------------
licor_master <- read.csv("licor_face_master.csv")
tdl_oct22_2122 <- read.csv("tdl_oct22_2122.csv")
tdl_oct22_1920 <- read.csv("tdl_oct22_1920.csv")

####format master licor file, will have two dataframes one with all data, and one with times for matching---------------
licor_gmes <- licorformat_func(licor_master)

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

#####Testing------------------------------

licor_dfr2 <- licor_gmes[licor_gmes$licor == "f2",]
licor_dfr2 <- droplevels(licor_dfr2)

xsi_oct22_f2$id <- "no_sample"

for(i in 1:nrow(licor_times)){
  
  j <- which(xsi_oct22_f2$timeavg >= licor_times$min[i] & xsi_oct22_f2$timeavg <= licor_times$max[i])
  xsi_oct22_f2$id[j] <- as.character(licor_times$id[i])
}

xsi_samples <- xsi_oct22_f2[xsi_oct22_f2$id != "no_sample",]


ii<- sapply(1:nrow(xsi_samples), function(i)which.min(timematch(xsi_samples$timeavg[i], licor_dfr2$datetime)))
xsi_samples$datetime_match <- licor_dfr2$datetime[ii]

licorrows <-5

thisrow <- ii


delr <- (licorrows-1)/2

licor_ids <- lapply(1:length(thisrow),
                    function(k) {
                      id_split <- licor_dfr2[(thisrow[k]-delr):(thisrow[k]+delr),]
                    })

###mean of each list, convert to dfr, cbind with xsi&DELTA from xsi_dfr (nrow must be equal)
licor_ids_datefix <- llply(licor_ids, function(x) c(x$datetime <- as.numeric(x$datetime), return(x)))
licor_ids_agg <- llply(licor_ids_datefix,function(x) summaryBy(.~id+Date+Empty_id, data=x, FUN=mean, keep.names=TRUE))
licor_agg_dtfix <- llply(licor_ids_agg, function(x) c(x$datetime <- as.POSIXct(x$datetime,
                                                  origin= '1970-01-01', tz="UTC"), return(x)))

licor_agg<- rbind.fill(licor_agg_dtfix)

###nrow needs to be equal (STOP)
licor_xsi <- cbind(licor_agg, xsi_samples[,c("xsi", "DELTA")])


####gmes_func---------------------------------------------------------------------------------------------------------
#for the moment i removed licor times

gmes_func <- function(xsi_dfr, licor_dfr, times_dfr, licorrows=5,whichlicor="f2" ){
  
  ###subset licor_dfr by licor used
  licor_dfr2 <- licor_dfr[licor_dfr$licor == whichlicor,]
  licor_dfr2 <- droplevels(licor_dfr2)
  
  ###use times_dfr to add "sample id" to xsi dfr based datetime interval
  xsi_dfr$id <- "no_sample"
  
  for(i in 1:nrow(times_dfr)){
    
    j <- which(xsi_dfr$timeavg >= times_dfr$min[i] & xsi_dfr$timeavg <= times_dfr$max[i])
    xsi_dfr$id[j] <- as.character(times_dfr$id[i])
  }
  
  xsi_samples <- xsi_dfr[xsi_dfr$id != "no_sample",]
  if(any(xsi_samples$xsi >20)) warning ("You have XSI values over the acceptable limit")
  
  ###add closest matched licor time "datetime_match" to xsi_dfr with timematch func (min diff)
  ii<- sapply(1:nrow(xsi_samples), function(i)which.min(timematch(xsi_samples$timeavg[i], licor_dfr2$datetime)))
  xsi_samples$datetime_match <- licor_dfr2$datetime[ii]
  
  ###subsets of licor_dfr2 based on time match + 2 logs +- (nrow=5 in each list)
  #   licor_ids <- lapply(1:nrow(xsi_samples),
  #                       function(k) {
  #                         id_split <- licor_dfr2[licor_dfr2$datetime <= (xsi_samples$datetime_match[k]+(2.4*logtime)) &
  #                                                licor_dfr2$datetime >= (xsi_samples$datetime_match[k]-(2.4*logtime)), ]
  #                       })
  thisrow <- ii 
  delr <- (licorrows-1)/2
  licordfr[(thisrow-delr):(thisrow+delr),]
  
  licor_ids <- lapply(1:nrow(xsi_samples),
                      function(k) {
                        id_split <- licor_dfr2[(thisrow[k]-delr:(thisrow[k]+delr),]
                      })
  
  
  
  ###mean of each list, convert to dfr, cbind with xsi&DELTA from xsi_dfr (nrow must be equal)
  licor_ids_datefix <- llply(licor_ids, function(x) c(x$datetime <- as.numeric(x$datetime), return(x)))
  licor_ids_agg <- llply(licor_ids_datefix,function(x) summaryBy(.~id+Date+Empty_id, data=x, FUN=mean, keep.names=TRUE))
  licor_agg_dtfix <- llply(licor_ids_agg, function(x) c(x$datetime <- as.POSIXct(x$datetime,
                                                                                 origin= '1970-01-01', tz="UTC"), return(x)))
  
  licor_agg<- rbind.fill(licor_agg_dtfix)
  ###nrow needs to be equal (STOP)
  licor_xsi <- cbind(licor_agg, xsi_samples[,c("xsi", "DELTA")])
}





