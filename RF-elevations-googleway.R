library(googleway)

lats0 <- seq(-32.515, -32.465, len=50) # gives latitudes of w-e transects
lons0 <- seq(116.94, 117.01, len=50) # gives longitudes of n-s transects
#
e1 <- rep(NA,2500)
lat1 <- rep(NA,2500)
lon1 <- rep(NA,2500)
# i in 1:NROW(lons0)
for(i in 1:NROW(lons0)) {
  for(j in 1:NROW(lats0)) {
    pt0 <- as.data.frame(cbind(lats0[j],lons0[i]))
    colnames(pt0)[1:2] <- c("lat","lon")
    #       use own api key
    elev.TEMP <- as.data.frame(google_elevation(df_locations = pt0,
                                                location_type="individual",
                                                key = GoogleElevKey,
                                                simplify = TRUE))
    e1[((NROW(lats0)*(i-1))+j)] <- unlist(elev.TEMP[1], use.names=F)
    lat1[((NROW(lats0)*(i-1))+j)]<- unlist(elev.TEMP[2], use.names=F)[1]
    lon1[((NROW(lats0)*(i-1))+j)]<- unlist(elev.TEMP[2], use.names=F)[2]
    cat("i = ",i,"; j = ",j, "rownum = ",((NROW(lats0)*(i-1))+j),"\n")
  }
}
elev.TEMP <- data.frame(lon1[1:(i*j)],lat1[1:(i*j)],e1[1:(i*j)])
colnames(elev.TEMP) <- c("Longitude","Latitude","Elevation")
print(elev.TEMP, digits=9)
print(summary(elev.TEMP[,3]), digits=9)
# rm(list=c("lats0", "lons0","e1","lat1","lon1","i","j"))
#
# add utm coordinates to the data frame and save to a csv file
library(sf)
TEMP.LL <- st_as_sf(x = elev.TEMP,
                            coords = c("Longitude", "Latitude"), crs = LongLat)
summary(st_coordinates(TEMP.LL))
TEMP.utm <- st_transform(TEMP.LL, crs = UTM50S)
print(summary(st_coordinates(TEMP.utm)), digits=8)
#
elev.TEMP <- cbind(round(st_coordinates(TEMP.utm),1),elev.TEMP)
colnames(elev.TEMP)[1:2] <- c("Easting","Northing")
write.csv(elev.TEMP, file="elev_RF.csv", row.names=FALSE)

elev_rf_google <- read.csv("elev_RF.csv")
par(oma=c(3,3,1,1), mar=c(4,4,1.5,1.5), mgp=c(1.4,0.3,0),
    lend=2, ljoin=1, tcl=0.3)
plot(rftiles);axis(1);axis(2)
with(rf_boundary, lines(Easting, Northing, col="#00000040", lwd=3,type="l"))
contour(x = seq(min(elev_rf_google[,1]), max(elev_rf_google[,1]), l = 50),
      y = seq(min(elev_rf_google[,2]), max(elev_rf_google[,2]), l = 50),
      z = matrix(elev_rf_google$Elevation,nrow=50, byrow = T),
      col="gold", labcex = 0.8, nlevels=20, add = TRUE)
image(x = seq(min(elev_rf_google[,1]), max(elev_rf_google[,1]), l = 50),
      y = seq(min(elev_rf_google[,2]), max(elev_rf_google[,2]), l = 50),
      z = matrix(elev_rf_google$Elevation,nrow=50, byrow = T),
      col=viridis::viridis(64, alpha = 0.33), add = TRUE)
#
# end code
