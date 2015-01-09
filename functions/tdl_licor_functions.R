#tdl formatting function----------------------------------------------------------------------------------

tdlformat_func <- function(x){
  x$calendar <- strptime(x$TIMESTAMP, format = "%d/%m/%Y")
  x$calendar <- as.character(x$calendar)
  x$clock <- as.character(x$TIME)
  x$Date <- paste(x$calendar, x$clock, sep=" ")
  x$datetime <- strptime(x$Date,  format = "%Y-%m-%d  %H:%M:%S")
  
  dfr <- x[,c("SiteOutput", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", "datetime")]
  
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
  dfr$id <- paste(dfr$Tree, dfr$Canopy, dfr$CO2, sep="-")
  dfr$id <- as.factor(dfr$id)
  dfr$O2 <- 21
  #sort the dfr to put o2 in the right place
  dfr <- dfr[,c(1:9, 65, 10:64)]
  return(dfr)
}


####function to return unique max and min dates for each tree id----------------------------------------------------

timerange_func <- function(x, dfr){
  
  x_sp <- split(x, x$id)
  dfr<- lapply(x_sp, function(x, dfr){
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
  xsi_dfr$line_id <- ifelse(xsi_dfr$SiteOutput %% 2 == 1,"a", "b")
  xsi_dfr$CO2_total <- (xsi_dfr$CorrConcA_Avg+xsi_dfr$CorrConcB_Avg)/(1-0.00474)
  #seperate ref and sample lines for calculations
  xsi_a <- xsi_dfr[xsi_dfr$line_id=="a",]
  names(xsi_a)[4] <- "del13_ref" 
  names(xsi_a)[7] <- "CO2_total_ref"
  xsi_b <- xsi_dfr[xsi_dfr$line_id=="b",]
  names(xsi_b)[4] <- "del13_samp"
  names(xsi_b)[7] <- "CO2_total_samp"
  #new dfr with xsi, deltadiff, DELTA, and timestamp for matching
  deltadiff<- xsi_b$del13_samp - xsi_a$del13_ref
  xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_ref - xsi_b$CO2_total_samp)
  
  xsi_calc <-data.frame(cbind(deltadiff, xsi))
  xsi_calc$DELTA <- (1000 * xsi_calc$xsi * xsi_calc$deltadiff)/(1000+xsi_b$del13_samp-(xsi_calc$xsi*xsi_calc$deltadiff))
  #times back to this dataframe..which ones?
  xsi_calc$timeavg <- xsi_a$datetime-((xsi_a$datetime - xsi_b$datetime)/2)
  
  return(xsi_calc)
}
