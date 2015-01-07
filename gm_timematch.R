####read tdl and get a working date-time-----------------------------------------------------------------------------------
tdl <- read.csv("tdl_oct22_2122.csv")

#formatting function

tdlformat <- function(x){
  x$calendar <- strptime(x$TIMESTAMP, format = "%d/%m/%Y")
  x$calendar <- as.character(x$calendar)
  x$clock <- as.character(x$TIME)
  x$Date <- paste(x$calendar, x$clock, sep=" ")
  x$datetime <- strptime(x$Date,  format = "%Y-%m-%d  %H:%M:%S")

  dfr <- x[,c("SiteOutput", "CorrConcA_Avg","CorrConcB_Avg",	"Corrdel13C_Avg", "DateTime")]

  return(dfr)
}

xsi <- tdlformat(tdl)

####read in licor and get working date-time---------------------------------------------------------------------------------
licor <- read.csv("licor_gm_master.csv")

licor$calendar <- strptime(licor$Date, format = "%d/%m/%Y")
licor$calendar <- as.character(licor$Date)
licor$clock <- as.character(licor$HHMMSS)
licor$Date2 <- paste(licor$calendar, licor$clock, sep=" ")
licor$datetime <- strptime(licor$Date2,  format = "%d/%m/%Y  %H:%M:%S")


licorformat <- function(x){
  x$calendar <- strptime(x$Date, format = "%d/%m/%Y")
  x$calendar <- as.character(x$Date)
  x$clock <- as.character(x$HHMMSS)
  x$Date2 <- paste(x$calendar, x$clock, sep=" ")
  x$datetime<- strptime(x$Date2, format = "%d/%m/%Y  %H:%M:%S")
  
  dfr <- x[,c(1:62,67)]
  
  return(dfr)
}

licor_gm <- licorformat(licor)

