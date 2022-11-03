git <- "https://github.com/Ratey-AtUWA/ridgefield-2022/raw/main/"
rf_boundary <- read.csv(paste0(git,"rf_boundary.csv"))
farmtrack <- read.csv(paste0(git,"farmtrack.csv"))
minor <- read.csv(paste0(git,"minorStrm.csv"))
nimb <- read.csv(paste0(git,"NimbedillingBrk.csv"))
rf_annot <- list(farmtrack = farmtrack, minor = minor,
                 nimb = nimb, rf_boundary = rf_boundary)

prevpal <- palette()
palette("default");palette(c(palette(),"gray92","white","transparent"))

with(rf_annot$nimb,lines(Easting, Northing,
                         lty=1,lwd=2, col="skyblue3", ljoin="round"))
text(500140, 6402800, labels="Nimbedilling Brook",
     col="skyblue2", srt=-25, font=3, cex=1)
with(rf_annot$minor, lines(Easting, Northing,
                          lty=1, lwd=1, col="skyblue3", ljoin="round"))
text(500158, 6405200, labels="Minor stream",
     col="skyblue2", srt=17, font=3, cex=0.9)
with(rf_annot$farmtrack, lines(Easting, Northing,
                          lty="12", lwd=1, col="khaki", ljoin="round"))
text(497275, 6405650, labels="Farm track",
     col="khaki3", srt=346, font=3, cex=0.9)
points(499243,6403432, pch=22, cex=2, col=9, bg=8, lwd=2, ljoin="mitre")
text(499243,6403432, labels="Farm buildings", cex=1, font=3, col=9, pos=4)
text(494900,6406400, labels="Boyagin\nNature\nReserve", cex=1.5, font=4, col="#668866")
text(496100,6407120, labels="Boyagin Road", cex=0.9, font=3, col=9, pos=4)
text(496665,6405830, labels="Walwalling Road", cex=0.9, font=3, col=9, srt=47)
text(499560,6402650, labels="Page\nRoad", cex=0.9, font=3, col=9, srt=90)
with(rf_annot$rf_boundary, polygon(Easting, Northing,
                            lty=c("12"), lwd=2, border="#FFFFFFB0"))

palette(prevpal); rm(prevpal)
