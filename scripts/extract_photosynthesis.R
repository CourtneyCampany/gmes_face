### extract Anet from licor master

library(doBy)
library(lattice)

# read data -------------------------------------------------------------------------
photo<-read.csv("raw_data/licor_master2.csv")
photo$Date <- as.Date(photo$Date, format = "%d/%m/%Y")

photo_spot <- summaryBy(Photo ~ Tree + Canopy + CO2 + Ring+ Date, FUN=mean, data=photo, keep.names = TRUE)
