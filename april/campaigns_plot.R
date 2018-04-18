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


#plotting with color------
windows()
par(oma=c(5,7,1,1), mfrow=c(4,2), mar=c(0,0,0,0), pch=16, lwd=1.5)

#photosynthesis-aco2
plot(photo ~ Date, data=aco2_low, col=canopycols[1], type="o",
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', cex=1.5, xlim=daterange)
points(photo ~ Date, data=aco2_upp, col=canopycols[2], cex=1.5, type="o")
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, photo, Date, photo+se.photo, angle=90, 
                    length=.03, col=canopycols[Position]))
with(aco2, arrows(Date, photo, Date, photo-se.photo, angle=90, 
                  length=.03, col=canopycols[Position]))
mtext(photolab, side=2, line=4, cex=.8)
legend("bottomleft", legend=c("Upper Canopy", "Lower Canopy"), pch=16,bty='n',
       col=c(canopycols[2], canopycols[1]), inset=.01)

#photosynthesis-eco2
plot(photo ~ Date, data=eco2_low, col=canopycols[1], type="o",
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', yaxt='n', cex=1.5, xlim=daterange)
points(photo ~ Date, data=eco2_upp, col=canopycols[2],cex=1.5, type="o")
axis(2, labels=FALSE)
with(eco2, arrows(Date, photo, Date, photo+se.photo, angle=90, 
                  length=.03, col=canopycols[Position]))
with(eco2, arrows(Date, photo, Date, photo-se.photo, angle=90, 
                  length=.03, col=canopycols[Position]))

#vcmax-aco2
plot(Vcmax ~ Date, data=aco2_low, col=canopycols[1],xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.5, type='o')
points(Vcmax ~ Date, data=aco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, 
                  length=.03, col=canopycols[Position]))
with(aco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, 
                  length=.03, col=canopycols[Position]))
mtext(vlab, side=2, line=3,cex=.8)

#vcmax-eco2
plot(Vcmax ~ Date, data=eco2_low, col=canopycols[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.5,yaxt='n', type='o')
points(Vcmax ~ Date, data=eco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(2, labels=FALSE)
with(eco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, 
                  length=.03, col=canopycols[Position]))
with(eco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, 
                  length=.03, col=canopycols[Position]))

#jmax-aco2
plot(Jmax ~ Date, data=aco2_low, col=canopycols[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.5, type='o')
points(Jmax ~ Date, data=aco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, 
                  length=.03, col=canopycols[Position]))
with(aco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, 
                  length=.03, col=canopycols[Position]))
mtext(jlab, side=2, line=3,cex=.8)

#jmax-eco2
plot(Jmax ~ Date, data=eco2_low, col=canopycols[1],xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.5,yaxt='n', type='o')
points(Jmax ~ Date, data=eco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(2, labels=FALSE)
with(eco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, 
                  length=.03, col=canopycols[Position]))
with(eco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, 
                  length=.03, col=canopycols[Position]))

#lma-aco2
plot(LMA ~ Date, data=aco2_low, col=canopycols[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 225), xaxt='n', cex=1.5, type='o')
points(LMA ~ Date, data=aco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 40, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
with(aco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, 
                  length=.03, col=canopycols[Position]))
with(aco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, 
                  length=.03, col=canopycols[Position]))
mtext(lmalab, side=2, line=4, cex=.8)

#lma-eco2
plot(LMA ~ Date, data=eco2_low, col=canopycols[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 225), xaxt='n', cex=1.5,yaxt='n', type='o')
points(LMA ~ Date, data=eco2_upp, col=canopycols[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 40, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
axis(2, labels=FALSE)
with(eco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, 
                  length=.03, col=canopycols[Position]))
with(eco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, 
                  length=.03, col=canopycols[Position]))

dev.copy2pdf(file= "output/campaigns.pdf")
dev.off()


#plotting with no color------
windows()
par(oma=c(5,7,1,1), mfrow=c(4,2), mar=c(0,0,0,0), lwd=1)

#photosynthesis-aco2
plot(photo ~ Date, data=aco2_low, pch=pchs[1], type="o",
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', cex=1.5, xlim=daterange)
points(photo ~ Date, data=aco2_upp, pch=pchs[2], cex=1.5, type="o")
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, photo, Date, photo+se.photo, angle=90, 
                  length=.03))
with(aco2, arrows(Date, photo, Date, photo-se.photo, angle=90, 
                  length=.03))
mtext(photolab, side=2, line=4, cex=.8)
legend("bottomleft", legend=c("Upper Canopy", "Lower Canopy"),
       pch=c(pchs[2],pchs[1]),bty='n', inset=.01, pt.cex=1.5)

#photosynthesis-eco2
plot(photo ~ Date, data=eco2_low, pch=pchs[1], type="o",
     xlab="",ylab="", ylim=c(0, 25), xaxt='n', yaxt='n', cex=1.5, xlim=daterange)
points(photo ~ Date, data=eco2_upp, pch=pchs[2],cex=1.5, type="o")
axis(2, labels=FALSE)
with(eco2, arrows(Date, photo, Date, photo+se.photo, angle=90, 
                  length=.03))
with(eco2, arrows(Date, photo, Date, photo-se.photo, angle=90, 
                  length=.03))

#vcmax-aco2
plot(Vcmax ~ Date, data=aco2_low, pch=pchs[1],xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.5, type='o')
points(Vcmax ~ Date, data=aco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, 
                  length=.03))
with(aco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, 
                  length=.03))
mtext(vlab, side=2, line=3,cex=.8)

#vcmax-eco2
plot(Vcmax ~ Date, data=eco2_low, pch=pchs[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 160), xaxt='n', cex=1.5,yaxt='n', type='o')
points(Vcmax ~ Date, data=eco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(2, labels=FALSE)
with(eco2, arrows(Date, Vcmax, Date, Vcmax+se.Vcmax, angle=90, 
                  length=.03))
with(eco2, arrows(Date, Vcmax, Date, Vcmax-se.Vcmax, angle=90, 
                  length=.03))

#jmax-aco2
plot(Jmax ~ Date, data=aco2_low, pch=pchs[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.5, type='o')
points(Jmax ~ Date, data=aco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
with(aco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, 
                  length=.03))
with(aco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, 
                  length=.03))
mtext(jlab, side=2, line=3,cex=.8)

#jmax-eco2
plot(Jmax ~ Date, data=eco2_low, pch=pchs[1],xlim=daterange,
     xlab="",ylab="", ylim=c(0, 210), xaxt='n', cex=1.5,yaxt='n', type='o')
points(Jmax ~ Date, data=eco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(2, labels=FALSE)
with(eco2, arrows(Date, Jmax, Date, Jmax+se.Jmax, angle=90, 
                  length=.03))
with(eco2, arrows(Date, Jmax, Date, Jmax-se.Jmax, angle=90, 
                  length=.03))

#lma-aco2
plot(LMA ~ Date, data=aco2_low, pch=pchs[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 225), xaxt='n', cex=1.5, type='o')
points(LMA ~ Date, data=aco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 40, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
with(aco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, 
                  length=.03))
with(aco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, 
                  length=.03))
mtext(lmalab, side=2, line=4, cex=.8)

#lma-eco2
plot(LMA ~ Date, data=eco2_low, pch=pchs[1], xlim=daterange,
     xlab="",ylab="", ylim=c(0, 225), xaxt='n', cex=1.5,yaxt='n', type='o')
points(LMA ~ Date, data=eco2_upp, pch=pchs[2], type='o', cex=1.5)
axis(1, labels=FALSE, at=unique(aco2$Date))
text(unique(aco2$Date), par("usr")[3] - 40, srt = 45,
     labels = campainlab, xpd = NA, cex=1.2)
axis(2, labels=FALSE)
with(eco2, arrows(Date, LMA, Date, LMA+se.LMA, angle=90, 
                  length=.03))
with(eco2, arrows(Date, LMA, Date, LMA-se.LMA, angle=90, 
                  length=.03))

dev.copy2pdf(file= "output/campaigns2.pdf")
dev.off()