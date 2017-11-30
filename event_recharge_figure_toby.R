library(OpenStreetMap)
library(rgdal)
library(raster)

amyraster<-raster("/Users/ayoder/Desktop/finalevent3.tif", ncols=1000,nrows=1000)
bounds<-read.csv("databounds.csv")
wells<-read.csv("MW_label.csv")
#turnboundsinto spatialpoints
WGScoor<-bounds
str(bounds)
lat<-WGScoor$y
long<-WGScoor$x
coordinates(WGScoor)=~x+y
proj4string(WGScoor)<- CRS("+proj=longlat +datum=WGS84")
raster::shapefile(WGScoor, "/Users/ayoder/Desktop/MyShapefile.shp")
amyshape<-readOGR("/Users/ayoder/Desktop/MyShapefile.shp")

amyshape
amycut<-mask(amyraster,amyshape)
plot(amycut)

map <- openmap (c(38.31263, -121.3913), c(38.29174, -121.3686),type='esri-topo')

#provides the map boundary using upper-left and lower-right corners
library(leaflet)
map_longlat <- openproj(map)
crs(map_longlat)
crs(amycut)<-crs(map_longlat)
plot(map_longlat)
points(y=wells$lat,wells$long)
text(y=wells$lat,wells$long, labels=wells$name, pos=1, cex=0.75)
plot(amycut, add=TRUE, alpha=0.5, col=rainbow(100, start = 0, end = .8))

# calculate recharge
amycut_ft <- amycut * 3.28084
ac_per_cell <- 1136/10000
sum_amycut_ft <- cellStats(amycut_ft, 'sum')
rech_acft <- sum_amycut_ft * ac_per_cell
amy_noNA <- cellStats(amycut, 'sum', na.rm = TRUE)
ncells <- ncell(amy_noNA)
l <- rech_acft * .07
h <- rech_acft * .25
mean_rech <- mean(c(h,l))
mean_rech-l
