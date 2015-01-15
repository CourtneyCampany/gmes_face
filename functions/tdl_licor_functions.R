#tdl formatting function----------------------------------------------------------------------------------

tdlformat_func <- function(x){
  x$calendar <- strptime(x$TIMESTAMP, format = "%d/%m/%Y")
  x$calendar <- as.character(x$calendar)
  x$clock <- as.character(x$TIME)
  x$Date <- paste(x$calendar, x$clock, sep=" ")
  x$datetime <- strptime(x$Date,  format = "%Y-%m-%d  %H:%M:%S")
  
  dfr <- x[,c("SiteOutput", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", "datetime", "licor")]
  
  return(dfr)
}


####licor formating function------------------------------------------------------------------------------------------
licorformat_func <- function(x){
  x$calendar <- strptime(x$Date, format = "%d/%m/%Y")
  x$calendar <- as.character(x$Date)
  x$clock <- as.character(x$HHMMSS)
  x$Date2 <- paste(x$calendar, x$clock, sep=" ")
  x$datetime<- strptime(x$Date2, format = "%d/%m/%Y  %H:%M:%S")
  
  dfr <- x[,c(1:62,66)]
  dfr$id <- paste(dfr$Ring,dfr$Tree, dfr$Canopy, dfr$CO2, sep="-")
  dfr$id <- as.factor(dfr$id)
  dfr$O2 <- 21
  #sort the dfr to put o2 in the right place
  dfr <- dfr[,c(1:9, 65, 10:64)]
  return(dfr)
}


####function to return unique max and min dates for each tree id----------------------------------------------------

timerange_func <- function(x, dfr){
  
  x_sp <- split(x, x$id)
  dfr<- lapply(x_sp, function(x){
      max.dt <- max(x$datetime)
      min.dt <- min(x$datetime)
      id<- unique(x$id)
      newdfr <- data.frame(min=min.dt, max=max.dt,id=id)

    })
  #below needs package 'data.table'
  times <- rbindlist(dfr)
  times2 <- as.data.frame(times)
}

###xsi calulcation for csv with tdl data (needs input of sample id still)
xsicalc_func <- function(x){
  xsi_dfr <- x[x$SiteOutput != c(3,4),]
  #give unique to ref and sample lines using odd/even tdl line #s
  xsi_dfr$line_id <- ifelse(xsi_dfr$SiteOutput %% 2 == 1,"a", "b")
  xsi_dfr$CO2_total <- (xsi_dfr$CorrConcA_Avg+xsi_dfr$CorrConcB_Avg)/(1-0.00474)
  #seperate ref and sample lines for calculations
  xsi_a <- xsi_dfr[xsi_dfr$line_id=="a",]
  colnames(xsi_a)[(names(xsi_a) == "Corrdel13C_Avg")] <- "del13_ref"
  colnames(xsi_a)[(names(xsi_a) == "CO2_total")] <- "CO2_total_ref"

  xsi_b <- xsi_dfr[xsi_dfr$line_id=="b",]
  colnames(xsi_b)[(names(xsi_b) == "Corrdel13C_Avg")] <- "del13_samp"
  colnames(xsi_b)[(names(xsi_b) == "CO2_total")] <- "CO2_total_samp"
  
  #new dfr with xsi, deltadiff, DELTA, and timestamp for matching
  deltadiff<- xsi_b$del13_samp - xsi_a$del13_ref
  xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_ref - xsi_b$CO2_total_samp)
  xsi_calc <-data.frame(cbind(deltadiff, xsi))
  xsi_calc$DELTA <- (1000 * xsi_calc$xsi * xsi_calc$deltadiff)/(1000+xsi_b$del13_samp-(xsi_calc$xsi*xsi_calc$deltadiff))
  
  #times back to this dataframe..which ones?
  xsi_calc$timeavg <- xsi_a$datetime-((xsi_a$datetime - xsi_b$datetime)/2)
  xsi_calc$licor <- xsi_dfr$licor[1]
  
  return(xsi_calc)
}

###time match function to find minimum difference between two sets of datetimes-------------------------------------
timematch <- function(time1, time2)abs(time1-time2)


####gmes_func---------------------------------------------------------------------------------------------------------
gmes_func <- function(xsi_dfr, licor_dfr, times_dfr, logtime=10,whichlicor="f2" ){
  
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
  
  ###add closest matched licor time "datetime_match" to xsi_dfr with timematch func (min diff)
  ii<- sapply(1:nrow(xsi_samples), function(i)which.min(timematch(xsi_samples$timeavg[i], licor_dfr2$datetime)))
  xsi_samples$datetime_match <- licor_dfr2$datetime[ii]
  
  ###subsets of licor_dfr2 based on time match + 2 logs +- (nrow=5 in each list)
  licor_ids <- lapply(1:nrow(xsi_samples),
                      function(k) {
                        id_split <- licor_dfr2[licor_dfr2$datetime <= (xsi_samples$datetime_match[k]+(2.4*logtime)) &
                                                 licor_dfr2$datetime >= (xsi_samples$datetime_match[k]-(2.4*logtime)), ]
                      })
  
  ###mean of each list, convert to dfr, cbind with xsi&DELTA from xsi_dfr (nrow must be equal)
  licor_ids_datefix <- llply(licor_ids, function(x) c(x$datetime <- as.numeric(x$datetime), return(x)))
  licor_ids_agg <- llply(licor_ids_datefix,function(x) summaryBy(.~id+Date+Empty_id, data=x, FUN=mean, keep.names=TRUE))
  licor_agg_dtfix <- llply(licor_ids_agg, function(x) c(x$datetime <- as.POSIXct(x$datetime,
                                                                                 origin= '1970-01-01', tz=""), return(x)))
  
  licor_agg<- rbind.fill(licor_agg_dtfix)
  ###nrow needs to be equal (STOP)
  licor_xsi <- cbind(licor_agg, xsi_samples[,c("xsi", "DELTA")])
}

