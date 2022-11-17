# Load the usual package list ####
  # (not all used probably)
library(sp)
library(lctools)
library(gstat)
library(sf)
library(maptiles)
library(prettymapr)
library(fields)
library(viridis)
library(ncf)

# list the kml files ####
list.files("./kml_files/")
# [1] "Dep_01.kml"              "Dep_02.kml"              "Dep_03.kml"
# [4] "Ero_01.kml"              "Ero_02.kml"              "Ero_03.kml"
# [7] "Ero_04.kml"              "Ero_05.kml"              "Ero_06.kml"
# [10] "NimBrk.kml"              "Nimbrk2.kml"             "Res_01.kml"
# [13] "Res_02.kml"              "Res_03.kml"              "Res_04.kml"
# [16] "Res_05.kml"              "Ridgefield UWA Farm.kml" "Stream01.kml"
# [19] "Stream01a.kml"           "Stream01b.kml"           "Stream01d.kml"
# [22] "Stream01e.kml"

# use kmlconvert() function to make data frames from kml ####
  # (in './FUNCTION kml conversion to LongLat and UTM.R')
LF_Dep01 <- kmlconvert("./kml_files/Dep_01.kml")
LF_Dep02 <- kmlconvert("./kml_files/Dep_02.kml")
LF_Dep03 <- kmlconvert("./kml_files/Dep_03.kml")

LF_Ero01 <- kmlconvert("./kml_files/Ero_01_new.kml")
LF_Ero02 <- kmlconvert("./kml_files/Ero_02.kml")
LF_Ero03 <- kmlconvert("./kml_files/Ero_03.kml")
LF_Ero04 <- kmlconvert("./kml_files/Ero_04.kml")
LF_Ero05 <- kmlconvert("./kml_files/Ero_05.kml")
LF_Ero06 <- kmlconvert("./kml_files/Ero_06.kml")

LF_Res01 <- kmlconvert("./kml_files/Res_01.kml")
LF_Res02 <- kmlconvert("./kml_files/Res_02.kml")
LF_Res03 <- kmlconvert("./kml_files/Res_03.kml")
LF_Res04 <- kmlconvert("./kml_files/Res_04.kml")
LF_Res05 <- kmlconvert("./kml_files/Res_05.kml")

# make sf POLYGON objects ####

LF_Dep01_sf <- st_polygon(list(as.matrix(LF_Dep01[,1:2])))
LF_Dep02_sf <- st_polygon(list(as.matrix(LF_Dep02[,1:2])))
LF_Dep03_sf <- st_polygon(list(as.matrix(LF_Dep03[,1:2])))

LF_Ero01_sf <- st_polygon(list(as.matrix(LF_Ero01[,1:2]),
                               as.matrix(LF_Res02[,1:2]),
                               as.matrix(LF_Res04[,1:2]))) # 2 holes in LF_Ero01
LF_Ero02_sf <- st_polygon(list(as.matrix(LF_Ero02[,1:2])))
LF_Ero03_sf <- st_polygon(list(as.matrix(LF_Ero03[,1:2])))
LF_Ero04_sf <- st_polygon(list(as.matrix(LF_Ero04[,1:2])))
LF_Ero05_sf <- st_polygon(list(as.matrix(LF_Ero05[,1:2])))
LF_Ero06_sf <- st_polygon(list(as.matrix(LF_Ero06[,1:2])))

LF_Res01_sf <- st_polygon(list(as.matrix(LF_Res01[,1:2])))
LF_Res02_sf <- st_polygon(list(as.matrix(LF_Res02[,1:2])))
LF_Res03_sf <- st_polygon(list(as.matrix(LF_Res03[,1:2])))
LF_Res04_sf <- st_polygon(list(as.matrix(LF_Res04[,1:2])))
LF_Res05_sf <- st_polygon(list(as.matrix(LF_Res05[,1:2])))

# use st_snap() to align polygon vertices and sides ####
  # most can be aligned with LF_Ero01, except:
  # Ero02 should be snapped to Dep02;
  # Ero03-Ero06 should be snapped to Dep03.
  #
  # LF_Dep01_sf, LF_Dep02_sf, LF_Dep03_sf
  # LF_Ero01_sf, LF_Ero02_sf, LF_Ero03_sf, LF_Ero04_sf, LF_Ero05_sf, LF_Ero06_sf
  # LF_Res01_sf, LF_Res02_sf, LF_Res03_sf, LF_Res04_sf, LF_Res05_sf

LF_Dep01_sf <- st_snap(LF_Dep01_sf, LF_Ero01_sf, tolerance = 10)
LF_Dep02_sf <- st_snap(LF_Dep02_sf, LF_Ero01_sf, tolerance = 10)
LF_Dep03_sf <- st_snap(LF_Dep03_sf, LF_Ero01_sf, tolerance = 10)
LF_Res01_sf <- st_snap(LF_Res01_sf, LF_Ero01_sf, tolerance = 20)
# LF_Res02_sf <- st_snap(LF_Res02_sf, LF_Ero01_sf, tolerance = 10) hole
LF_Res03_sf <- st_snap(LF_Res03_sf, LF_Ero01_sf, tolerance = 40)
# LF_Res04_sf <- st_snap(LF_Res04_sf, LF_Ero01_sf, tolerance = 10) hole
LF_Res05_sf <- st_snap(LF_Res05_sf, LF_Ero01_sf, tolerance = 10)
LF_Ero02_sf <- st_snap(LF_Ero02_sf, LF_Dep02_sf, tolerance = 10)
LF_Ero03_sf <- st_snap(LF_Ero03_sf, LF_Dep03_sf, tolerance = 10)
LF_Ero04_sf <- st_snap(LF_Ero04_sf, LF_Dep03_sf, tolerance = 10)
LF_Ero05_sf <- st_snap(LF_Ero05_sf, LF_Dep03_sf, tolerance = 10)
LF_Ero06_sf <- st_snap(LF_Ero06_sf, LF_Dep03_sf, tolerance = 10)

par(oma=c(4,4,1,1),mar=c(0,0,0,0))
plot(LF_Ero01_sf, xlim = c(495000,501000), ylim = c(6403000,6408000),
     lwd = 2, border = 7, col="moccasin")
axis(1);axis(2)
plot(LF_Dep01_sf, add=T, border=4, lwd = 2, col = "lightblue1")
plot(LF_Dep02_sf, add=T, border=4, lwd = 2, col = "lightblue1")
plot(LF_Dep03_sf, add=T, border=4, lwd = 2, col = "lightblue1")
plot(LF_Ero02_sf, add=T, border=7, lwd = 2, col = "moccasin")
plot(LF_Ero03_sf, add=T, border=7, lwd = 2, col = "moccasin")
plot(LF_Ero04_sf, add=T, border=7, lwd = 2, col = "moccasin")
plot(LF_Ero05_sf, add=T, border=7, lwd = 2, col = "moccasin")
plot(LF_Ero06_sf, add=T, border=7, lwd = 2, col = "moccasin")
plot(LF_Res01_sf, add=T, border=3, lwd = 2, col = "honeydew")
plot(LF_Res02_sf, add=T, border=3, lwd = 2, col = "honeydew")
plot(LF_Res03_sf, add=T, border=3, lwd = 2, col = "honeydew")
plot(LF_Res04_sf, add=T, border=3, lwd = 2, col = "honeydew")
plot(LF_Res05_sf, add=T, border=3, lwd = 2, col = "honeydew")

# export the tidied-up data ####
# make multipolygon sf object
RF_LF3_sf <- st_multipolygon(list(LF_Dep01_sf,LF_Dep02_sf,LF_Dep03_sf,
                                  LF_Ero01_sf,LF_Ero02_sf,LF_Ero03_sf,
                                  LF_Ero04_sf,LF_Ero05_sf,LF_Ero06_sf,
                                  LF_Res01_sf,LF_Res02_sf,LF_Res03_sf,
                                  LF_Res04_sf,LF_Res05_sf))
# convert sf to sfc and add CRS
RF_LF3_sfc <- st_sfc(RF_LF3_sf)
st_crs(RF_LF3_sfc) <- UTM50S
# transform to GDA94
Ridgefield_3LF <- st_transform(RF_LF3_sfc, crs = st_crs(28350))
# write to GIS shapefile!
st_write(Ridgefield_3LF,"Ridgefield_3LF.shp")
