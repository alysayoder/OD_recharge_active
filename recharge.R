# setwd()
setwd("C:/Users/ayoder/BoxSync/CH")


# load packages
packagelist = list("ggplot2", "colorspace", "reshape2", "tidyverse", "knitr", "sp", 
                   "rgdal", "raster", "gstat", "dismo", "spMC", "formatR", "dplyr", "magrittr")
lapply(packagelist, library, character.only = TRUE)

low <- read.csv("R_input_low_1617.csv")
glimpse(low)

# plot surface water levels
low %>% as.data.frame %>% 
  ggplot(aes(long, lat)) + geom_point(aes(size=levellow), color="blue", alpha=3/4) + 
  ggtitle("Lowest Surface Water Level (m)") + coord_equal() + theme_bw()

# convert to spatial points data frame
class(low)
str(low)
coordinates(low) <- ~ long + lat
class(low)
str(low)


bbox(low) # bounding box
coordinates(low) %>% glimpse
proj4string(low)
identical(coordinates(low), low@coords)

# krige low----

# variogram
lzn.vgm <- variogram(log(low$levellow*-1)~1, low) # calculate sample variogram values
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(psill = 10, "Sph", range = NA, kappa = 0.5)) # fit model
plot(lzn.vgm, lzn.fit)

# load spatial domain to interpolate over
grid <- raster(low, ncols=100, nrows=100)
g<-as(grid, 'SpatialGrid')

# krige
k <- gstat(formula = low$levellow~1, locations = low, model = lzn.fit)
kriged <- predict(k,g)
plot(kriged)
plot(low, add = T)

#krige high----

# variogram 
h.vgm <- variogram(log(high$levelhigh)~1, high) 
h.fit <- fit.variogram(h.vgm, model=vgm(psill = .1, "Lin", range = NA))
plot(h.vgm, h.fit)

# krige
kh <- gstat(formula = high$levelhigh~1, locations = high, model = h.fit)
krigedh <- predict(kh, g)
plot(krigedh)
plot(high, add = T)

# krige difference----
rash <- raster(krigedh)
rasl <- raster(kriged)
krige1617 <- rash-rasl
plot(krige1617, col = rainbow(100), main = expression("2016-17 Wet-Season Cumulative "*Delta*" Head(m) w/Kriging"))
plot(high, add = T)

# krige recharge estimate----
krige1617feet <- krige1617*3.28084
sum_krige_ft <- cellStats(krige1617feet, 'sum')
krig_rech1617_acft <- sum_krige_ft*ac_per_cell


# idw low ----
gs<-gstat(formula = low$levellow~1, locations = low, nmax=8, set=list(idp=2))
idw<-interpolate(grid, gs)
plot(idw, main = 'Lowest SWL (m)')
plot(low, add=T)

# idw high----
high <- read.csv('R_input_high_1617.csv')
high %>% as.data.frame %>% 
  ggplot(aes(long, lat)) + geom_point(aes(size=levelhigh), color="blue", alpha=3/4) + 
  ggtitle("Highest Surface Water Level (m)") + coord_equal() + theme_bw()
coordinates(high) <- ~ long + lat
hi <- gstat(formula = high$levelhigh~1, locations = high, nmax=8, set=list(idp=2))
idw_h <- interpolate(grid, hi)
plot(idw_h, main = 'Highest SWL(m)')
plot(high, add=T)

# inverse distance weighting difference----
idw1617 <- idw_h - idw
plot(idw1617, main = expression("2016-17 Wet-Season Cumulative "*Delta*" Head(m)"), col = rainbow(100))
plot(high, add=T)

# idw recharge estimate----
idw1617feet <- idw1617*3.28084
ac_per_cell <- 1136/10000
sum_idw_ft <- cellStats(idw1617feet, 'sum')
rech1617_acft <- sum_idw_ft*ac_per_cell

# publish to PDF----
pdf(file = "Krig_1617_hmap.pdf", paper = "special", width = 11, height = 8.5, onefile = TRUE)

plot(idw, main = 'Lowest SWL (m)')
plot(low, add=T)
plot(idw_h, main = 'Highest SWL(m)')
plot(high, add=T)


dev.off()
