tdl_oct22_2121 <- read.csv("tdl_oct22_2122.csv")


tdlformat_func <- function(x){
  x$calendar <- strptime(x$TIMESTAMP, format = "%d/%m/%Y")
  x$calendar <- as.character(x$calendar)
  x$clock <- as.character(x$TIME)
  x$Date <- paste(x$calendar, x$clock, sep=" ")
  x$datetime <- strptime(x$Date,  format = "%Y-%m-%d  %H:%M:%S")
  
  dfr <- x[,c("SiteOutput", "CorrConcA_Avg","CorrConcB_Avg",  "Corrdel13C_Avg", "datetime")]
  
  return(dfr)
}

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


test0 <- tdlformat_func(tdl_oct22_2121)
test1 <- xsicalc_func(test0)





test <- test0[test0$SiteOutput != c(3,4),]
test$id <- ifelse(test$SiteOutput %% 2 == 1,"a", "b")
test$CO2_total <- (test$CorrConcA_Avg + test$CorrConcB_Avg)/(1-0.00474)

xsi_a <- test[test$id=="a",]
  names(xsi_a)[4] <- "del13_ref" 
  names(xsi_a)[7] <- "CO2_total_ref"
xsi_b <- test[test$id=="b",]
  names(xsi_b)[4] <- "del13_samp"
  names(xsi_b)[7] <- "CO2_total_samp"

deltadiff<- xsi_b$del13_samp - xsi_a$del13_ref
xsi <- xsi_b$CO2_total_samp/(xsi_a$CO2_total_ref - xsi_b$CO2_total_samp)

xsi_dfr <-data.frame(cbind(deltadiff, xsi))

xsi_dfr$DELTA <- (1000 * xsi_dfr$xsi * xsi_dfr$deltadiff)/(1000+xsi_b$del13_samp-(xsi_dfr$xsi*xsi_dfr$deltadiff))
#times back to this dataframe..which ones?
xsi_dfr$timeavg <- xsi_a$datetime-((xsi_a$datetime - xsi_b$datetime)/2)




