library(stars)
library(sf)
library(viridis)
library(ggplot2)

rf_DEM_stars <- read_stars("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
par(mar=c(0,0,0,0), oma=c(4,4,1,1))
plot(rf_DEM_stars);axis(1);axis(2)
names(rf_DEM_stars) <- "RF_elevation"
str(rf_DEM_stars)
summary(st_coordinates(rf_DEM_stars))

# might be useful to store the metadata for geoTIFF/stars object
(rf_DEM_dims <- as.data.frame(rbind(unlist(rf_DEM_dims[1,]),
                                    unlist(rf_DEM_dims[2,]))))
rf_DEM_dims$from <- as.numeric(rf_DEM_dims$from)
rf_DEM_dims$to <- as.numeric(rf_DEM_dims$to)
rf_DEM_dims$offset <- as.numeric(rf_DEM_dims$offset)
rf_DEM_dims$delta <- as.numeric(rf_DEM_dims$delta)
##   from  to  offset delta              refsys point values x/y
## x    1 425  495053    13 GDA94 / MGA zone 50 FALSE   NULL [x]
## y    1 360 6407739   -13 GDA94 / MGA zone 50 FALSE   NULL [y]

par(mar=c(4,4,1,1), oma=c(0,0,0,0), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2)
image(seq(495060,500572,len=425),
      seq(6403066,6407732,length.out=360),
      rf_elevs_mat[,seq(NCOL(rf_elevs_mat),1,-1)],
      asp=1, col = cividis(18), # cividis seems best colour palette
      xlab="Easting (UTM Zone 50, m)", ylab="Northing (UTM Zone 50, m)")
contour(seq(495060,500572,len=425),
        seq(6403066,6407732,length.out=360),
        rf_elevs_mat[,seq(NCOL(rf_elevs_mat),1,-1)],
        asp=1, levels = seq(335,400,5), add=T)
contour(seq(495060,500572,len=425),
        seq(6403066,6407732,length.out=360),
        rf_elevs_mat[,seq(NCOL(rf_elevs_mat),1,-1)],
        col="grey67", asp=1, levels = seq(290,330,5), add=T)
lines(rf_boundary[,1:2])

require(fields)
quilt.plot(x=st_coordinates(rf_DEM_stars)[,1],
           y=st_coordinates(rf_DEM_stars)[,2],
           z=rf_elevs_vect, asp=1,
           col=terrain.colors(36))

# trying ggplot ####
ggplot() +
  geom_stars(data=rf_DEM_stars, na.rm=TRUE) +
  geom_sf(data = rf_hydrology, col="steelblue2") +
  coord_sf() +
  scale_fill_viridis(option="cividis", na.value = "#00000000") +
  theme_bw()
