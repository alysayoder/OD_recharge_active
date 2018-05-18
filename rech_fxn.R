# Function that takes inputs of groundwater levels at the start and end of a recharge event and returns
# volume of groundwater recharged throughout the event. 

# import water levels at the beginning and end of the event
# input files' columns must be in this order: lat, long, level
# the 'level' column must be in meters of groundwater elevation relative to mean sea level

# get data
adata <- read.csv('allsets.csv', stringsAsFactors = FALSE)
adata <- adata[-c(1,3,15,17:31)]
# format dataframes w/lat long 
label <- read.csv("MW_label_allsets.csv", header = F) # get lat long data, already in order of allsets.csv wells
label[3] <- NULL
# grab max and mins from each water year. 
max2014 <- as.data.frame(t(subset(adata, Date == "2014-05-10 12:00:00")))
min2014 <- as.data.frame(t(subset(adata, Date == "2014-02-09 12:00:00")))

#2015 formatting
max2015 <- as.data.frame(t(subset(adata,Date =="2015-04-12 12:00:00")))

rownames(max2015) <- NULL

colnames(max2015) <- NULL
max2015 <- max2015[-1,]
max2015
hi2015 <- data.frame(c(label,max2015))
colnames(hi2015) <- NULL
rownames(hi2015) <- NULL
colnames(hi2015) <- c("lat", "long", "level")
hi2015 <- read.csv("hi2015.csv")

min2015 <- as.data.frame(t(subset(adata,Date =="2014-12-04 12:00:00")))
rownames(min2015) <- NULL
colnames(min2015) <- NULL
min2015 <- min2015[-1,]
lo2015 <- data.frame(c(label,min2015))
colnames(lo2015) <- NULL
rownames(lo2015) <- NULL
colnames(lo2015) <- c("lat", "long", "level")
lo2015 <- read.csv("lo2015.csv")


# 2016 formatting
max2016 <- as.data.frame(t(subset(adata,Date =="2016-03-18 12:00:00")))
rownames(max2016) <- NULL
colnames(max2016) <- NULL
max2016 <- max2016[-1,]
hi2016 <- data.frame(c(label,max2016))
colnames(hi2016) <- NULL
rownames(hi2016) <- NULL
colnames(hi2016) <- c("lat", "long", "level")

min2016 <- as.data.frame(t(subset(adata,Date=="2016-12-10 12:00:00")))
rownames(min2016) <- NULL
colnames(min2016) <- NULL
min2016 <- min2016[-1,]
lo2016 <- data.frame(c(label,min2016))
colnames(lo2016) <- NULL
rownames(lo2016) <- NULL
colnames(lo2016) <- c("lat", "long", "level")

# below is working code for formatting a recharge estimate input file
min2014 <- as.data.frame(t(subset(adata, Date == "2014-02-09 12:00:00")))
rownames(min2014) <- NULL
colnames(min2014) <- NULL
min2014 <- min2014[-1,]
lo2014 <- data.frame(c(label,min2014))
colnames(lo2014) <- NULL
rownames(lo2014) <- NULL
colnames(lo2014) <- c("lat", "long", "level")

# put data filenames into begin and end variables, run lines 9-18, use function 'rechvol' with begin=begin and end=end to get m^3 recharge volumes
begin <- read.csv("R_input_low_1617.csv")
end <- read.csv("R_input_high_1617.csv")

# make sure column names are compatible with the function
colnames(hi2014) <- c("lat", "long", "level")

# load packages
packages <- c("raster", "geosphere", "gstat", "splancs", "dplyr", "ggplot2")
lapply(packages, library, character.only=T)

# calculate recharge volume w/specific yield range
rechvol <- function(begin, end, print = TRUE) {
  # Determine area of entire raster
  # Raster has 68523 cells
  y_bound <- distm(c(-121.3914, 38.2917), c(-121.3914, 38.31267))
  x_bound <- distm(c(-121.3914, 38.2917), c(-121.3686, 38.2917))
  # Calculate area in m^2
  area <- y_bound * x_bound
  # Total area is 4656185 m^2
  
  # define coordinate system
  coordinates(begin) <- ~ long + lat
  coordinates(end) <- ~ long + lat
  
  # load spatial domain to interpolate over
  grid <- raster(begin, ncols=100, nrows=100)
  
  # interpolate highs
  idw_begin <- gstat(formula = begin$level~1, locations = begin, nmax = 8, set = list(idp=2))
  idw_begin <- interpolate(grid, idw_begin)
  
  # interpolate lows
  idw_end <- gstat(formula = end$level~1, locations = end, nmax = 8, set=list(idp=2))
  idw_end <- interpolate(grid, idw_end)
  
  # subtract to find net water levels throughout the event
  event_vol <- abs(idw_end-idw_begin)
  
  # import outline of boundary, as a set of points in lat/long that we generated in ArcMap as a digitized polyline
  bound <- read.csv("bound_table.csv", stringsAsFactors = FALSE, header = TRUE)
  bound_pts <- bound[,c(4,5)]
  colnames(bound_pts) <- c('x', 'y')
  
  # convert raster to SPDF, or at least get some coords
  event_pts <- coordinates(event_vol)[!is.na(values(event_vol)),]
  colnames(event_pts) <- c('x', 'y')
  
  # clip event idw by the innundation extent generated form our digitized polyline in ArcMap
  library(splancs)
  temp <- rasterToPoints(event_vol) %>% as.data.frame()
  event_clip <- temp[inout(event_pts, bound_pts), ]
  colnames(event_clip) <- c('x','y','z')
  
  # plot idwevent_clip as a heatmap
  library(viridis)
  library(magrittr)
  event_clip %>% 
    ggplot() +
    geom_tile(aes(x,y, fill = z)) +
    coord_fixed(1) +
    scale_fill_viridis(name="Event Recharged Head (m)") +
    theme_void()
  
  coordinates(event_clip) <- ~ x + y
  gridded(event_clip) <- TRUE
  event_rast <- raster(event_clip, "z")
  plot(event_rast)
  title(main = "Change in Water Levels 2015WY (m)")
  
  # calculate area per cell 
  m2_cell_idw <- area/ncell(event_rast)
  m2_cell_idw <- as.numeric(m2_cell_idw)
  
  # m3 per cell
  m3percell_idw <- m2_cell_idw * event_rast
  totm3_idw <- cellStats(m3percell_idw, "sum")
  
  # apply Sy range
  lowIDW <- totm3_idw*0.07
  midIDW <- totm3_idw*0.16
  highIDW <- totm3_idw*0.25
  
  result <-  list(lowIDW,midIDW,highIDW)
  return(result)
}
