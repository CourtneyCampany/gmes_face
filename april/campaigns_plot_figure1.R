##gas exchange campaigns------

ge <- read.csv("april/face_photo_means.csv")
  ge <- ge[complete.cases(ge),]
  ge <- droplevels(ge)
  ge$Date <- paste(ge$Campaign, "01", sep="_")
  ge$Date <- as.Date(ge$Date, format="%b_%Y_%d")

aco2 <- ge[ge$C.treat == "+C",]
eco2 <- ge[ge$C.treat == "0C",]
  
aco2_upp <- ge[ge$C.treat == "+C" & ge$Position == "upper",]
aco2_low <- ge[ge$C.treat == "+C" & ge$Position == "lower",]
eco2_upp <- ge[ge$C.treat == "0C" & ge$Position == "upper",]
eco2_low <- ge[ge$C.treat == "0C" & ge$Position == "lower",]


#plot bits------
campainlab <- gsub("_", " ", unique(ge$Campaign))
canopycols <- c("forestgreen", "royalblue")
pchs=c(1,16)
photolab <- expression(italic(A)[n]~~(mu*mol~m^-2~s^-1))
lmalab <- expression(LMA~~(g~cm^-2))
jlab <- expression(atop(Apparent~J[max],~~(mu*mol~m^-2~s^-1)))
vlab <- expression(atop(Apparent~V[cmax],~~(mu*mol~m^-2~s^-1)))
daterange <- c(as.Date("2014-04-01"), as.Date("2016-04-01"))
datelab <- as.Date("2016-04-01")
pchs2 <- c(1,1,16,16)

#plotting---------
windows()
par(oma=c(5,7,3,1), mfrow=c(4,2), mar=c(0,0,0,0), pch=16, lwd=1.5)

#photosynthesis-aco2
plot(photo ~ Date, data=aco2_low, type="b", pch=pchs2,lty=3,
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', cex=1.25, xlim=daterange)
points(photo ~ Date, data=aco2_upp,  cex=1.25, type="b",pch=pchs2)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, photo, Date, photo+se.photo, angle=90, lwd=1,
                    length=.03))
with(aco2, arrows(Date, photo, Date, photo-se.photo, angle=90, lwd=1,
                  length=.03))
mtext(photolab, side=2, line=4, cex=.8)
legend("bottomleft", legend=c("Mature Leaves", "New Leaves", "Upper Canopy", "Lower Canopy"),
       bty='n', col="black", inset=.01, pch=c(1,16,NA,NA), lty=c(NA, NA, 1,3))
mtext(expression(Ambient~CO[2]), side=3, line=1, cex=.8)
text(datelab, 24, "A", cex=1.2)

#photosynthesis-eco2
plot(photo ~ Date, data=eco2_low,  type="b", pch=pchs2,lty=3,
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', yaxt='n', cex=1.25, xlim=daterange)
points(photo ~ Date, data=eco2_upp, cex=1.25, type="b",pch=pchs2)
axis(2, labels=FALSE, tcl=.5)
with(eco2, arrows(Date, photo, Date, photo+se.photo, angle=90, lwd=1,
                  length=.03))
with(eco2, arrows(Date, photo, Date, photo-se.photo, angle=90, lwd=1,
                  length=.03))
mtext(expression(Elevated~CO[2]), side=3, line=1, cex=.8)
text(datelab, 24, "B", cex=1.2)

#vcmax-aco2
plot(Vcmax ~ Date, data=aco2_low, xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.25, type="b", pch=pchs2,lty=3)
points(Vcmax ~ Date, data=aco2_upp,  type="b",pch=pchs2, cex=1.25)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, lwd=1,
                  length=.03))
with(aco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, lwd=1,
                  length=.03))
mtext(vlab, side=2, line=3,cex=.8)
text(datelab, 150, "C", cex=1.2)

#vcmax-eco2
plot(Vcmax ~ Date, data=eco2_low, xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.25,yaxt='n', type="b", pch=pchs2,lty=3)
points(Vcmax ~ Date, data=eco2_upp, type="b",pch=pchs2, cex=1.25)
axis(2, labels=FALSE, tcl=.5)
with(eco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, lwd=1,
                  length=.03))
with(eco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, lwd=1,
                  length=.03))
text(datelab, 150, "D", cex=1.2)

#jmax-aco2
plot(Jmax ~ Date, data=aco2_low,  xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.25, type="b", pch=pchs2,lty=3)
points(Jmax ~ Date, data=aco2_upp, type="b",pch=pchs2, cex=1.25)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, lwd=1,
                  length=.03))
with(aco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, lwd=1,
                  length=.03 ))
mtext(jlab, side=2, line=3,cex=.8)
text(datelab, 200, "E", cex=1.2)

#jmax-eco2
plot(Jmax ~ Date, data=eco2_low, xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.25,yaxt='n', type="b", pch=pchs2,lty=3)
points(Jmax ~ Date, data=eco2_upp, type="b",pch=pchs2, cex=1.25)
axis(2, labels=FALSE, tcl=.5)
with(eco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, lwd=1,
                  length=.03))
with(eco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, lwd=1,
                  length=.03))
text(datelab, 200, "F", cex=1.2)

#lma-aco2
plot(LMA ~ Date, data=aco2_low,  xlim=daterange,
     xlab="",ylab="", ylim=c(0, 225), xaxt='n', cex=1.25,type="b", pch=pchs2,lty=3)
points(LMA ~ Date, data=aco2_upp,  type="b",pch=pchs2, cex=1.25)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 45, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
with(aco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, lwd=1,
                  length=.03))
with(aco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, lwd=1,
                  length=.03))
mtext(lmalab, side=2, line=4, cex=.8)
text(datelab, 210, "G",cex=1.2)

#lma-eco2
plot(LMA ~ Date, data=eco2_low, xlim=daterange, xlab="",ylab="", ylim=c(0, 225), 
     xaxt='n', cex=1.25,yaxt='n', type="b", pch=pchs2,lty=3)
points(LMA ~ Date, data=eco2_upp, type="b",pch=pchs2, cex=1.25)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 45, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
axis(2, labels=FALSE, tcl=.5)
with(eco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, lwd=1,
                  length=.03))
with(eco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, lwd=1,
                  length=.03))
text(datelab, 210, "H",cex=1.2)

dev.copy2pdf(file= "output/campaigns.pdf")
dev.off()


