library(stars)
library(sf)

rf_DEM_stars <- read_stars("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
par(mar=c(0,0,0,0), oma=c(4,4,1,1))
plot(rf_DEM_stars);axis(1);axis(2)
str(rf_DEM_stars)
names(rf_DEM_stars)
names(rf_DEM_stars) <- "Ridgefield_DEM"
str(rf_DEM_stars)
st_dimensions(rf_DEM_stars)
##   from  to  offset delta              refsys point values x/y
## x    1 425  495053    13 GDA94 / MGA zone 50 FALSE   NULL [x]
## y    1 360 6407739   -13 GDA94 / MGA zone 50 FALSE   NULL [y]
rf_DEM_sf <- st_as_sf(rf_DEM_stars)
# plot(rf_DEM_sf) # VERY SLOW!!!
rf_DEMcontour <-
  st_contour(rf_DEM_sf,  # doesn't work yet
             contour_lines = TRUE, breaks=seq(280,380,10))

par(mar=c(4,4,1,1), oma=c(0,0,0,0), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2)
image(seq(495060,500572,len=425),
      seq(6403066,6407732,length.out=360),
      rf_elevs_mat[,seq(NCOL(rf_elevs_mat),1,-1)],
      asp=1, col = terrain.colors(24)[1:18],
      xlab="Easting (UTM Zone 50, m)", ylab="Northing (UTM Zone 50, m)")
contour(seq(495060,500572,len=425),
      seq(6403066,6407732,length.out=360),
      rf_elevs_mat[,seq(NCOL(rf_elevs_mat),1,-1)],
      asp=1, levels = seq(290,400,5), add=T)
lines(rf_boundary[,1:2])

require(fields)
quilt.plot(x=st_coordinates(rf_DEM_stars)[,1],
           y=st_coordinates(rf_DEM_stars)[,2],
           z=rf_elevs_vect, asp=1,
           col=terrain.colors(36))
