# create matrix of test elevations in Excel using saddle quadratic function &
# import into R data frame testElev, check with plot:
image(seq(1,10), seq(1,10), as.matrix(testElev[,2:11]))
contour(seq(1,10), seq(1,10), as.matrix(testElev[,2:11]),add=T,
        labcex=2, levels=seq(0,100,2))

# this section from
# https://datacarpentry.org/r-raster-vector-geospatial/01-raster-structure/
library(raster) # slope parameters from raster::terrain() function
library(rgdal)
library(ggplot2)

GDALinfo("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
RF_raster <- raster("Ridgefield_DEM_gda94_utm50_epsg28350.tif")
RF_rast2df <- as.data.frame(RF_raster, xy=TRUE)
colnames(RF_rast2df)[3] <- "RF_raster"

ggplot() +
  geom_raster(data = RF_rast2df , aes(x = x, y = y, fill = RF_raster)) +
  scale_fill_viridis_c(na.value = "#00000000") +
  theme_bw()

# for terrain() function see
# https://arc2r.github.io/book/Surface.html
