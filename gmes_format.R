
gmes <-read.csv("master_data_file_clean.csv")
  gmes$Date <- as.Date(gmes$Date)

  
amb <- gmes[gmes$co2meas=="amb",]

palette(c("black", "red"))
pchs <- c(16,17)

plot(gmes~mesolay.mean, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~leafw, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~mesolay.mean, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~epi_up, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~epi_low, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~meso.mean, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~length.par1.mean, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~mean_gs, data=amb, col=co2grow, pch=pchs[amb$canopy])

plot(gmes~mean_cc, data=amb, col=co2grow, pch=pchs[amb$canopy])

