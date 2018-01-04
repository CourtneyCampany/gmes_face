#functions

gmes_format_func <- function(x){
  x$Date <- as.Date(x$Date, format ="%d/%m/%Y", tz="UTC")
  x$treatment <- with(x, paste(co2grow, position, sep="-"))
  x$Nm <- x$N.perc *10           # mass-based leaf N
  x$Na <- x$LMA * x$Nm / 1000 # Narea in g m-2
  #dataset with growth CO2
  ambco2 <- subset(x, co2grow == "amb" & co2meas == "amb")
  elevco2 <- subset(x, co2grow == "elev" & co2meas == "elev")
  growCO2 <- rbind(ambco2,elevco2)
  growCO2$Nm <- growCO2$N.perc *10
  growCO2$Na <- growCO2$LMA * growCO2$Nm / 1000 # Narea in g m-2
  return(growCO2)
}