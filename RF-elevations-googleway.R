library(googleway)

lats0 <- seq(-32.5161, -32.4613, len=50) # gives latitudes of w-e transects
lons0 <- seq(116.945, 117.0085, len=50) # gives longitudes of n-s transects
# 
e1 <- rep(NA,2500)
lat1 <- rep(NA,2500)
lon1 <- rep(NA,2500)
# i in 1:NROW(lons0)
for(i in 1:NROW(lons0)) {
  for(j in 1:NROW(lats0)) {
    pt0 <- as.data.frame(cbind(lats0[j],lons0[i]))
    colnames(pt0)[1:2] <- c("lat","lon")
    #       use new api key for andrewwrate@gmail.com 20190123
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
TEMP.LL<-extent <- st_as_sf(x = elev.TEMP, 
                            coords = c("Longitude", "Latitude"), crs = LongLat)
summary(st_coordinates(TEMP.LL))
TEMP.utm <- st_transform(TEMP.LL, crs = UTM50S)
print(summary(st_coordinates(TEMP.utm)), digits=8)
#
elev.TEMP <- cbind(round(st_coordinates(TEMP.utm),1),elev.TEMP)
colnames(elev.TEMP)[1:2] <- c("Easting","Northing")
write.csv(elev.TEMP, file="elev_RF.csv", row.names=FALSE)

with(elev.TEMP, plot(Easting, Northing, asp=1))
image(elev.TEMP[,1],elev.TEMP[,2],elev.TEMP[,3])

#
# end code