# Function that takes inputs of groundwater levels at the start and end of a recharge event and returns
# volume of groundwater recharged throughout the event. 

# import water levels at the beginning and end of the event
# input files' columns should be ordered: lat, long, level
# the 'level' column should be in meters of groundwater elevation relative to mean sea level

# This version is modified to give the proper output for estimating lateral outflow from the study area
# Needs to output an untrimmed .tif of the inverse distance weighted water levels
# `` and a df of the values at the trimmed area, ultimately need to calculate the length of the lateral boundaries

# calculate recharge volume w/specific yield range
rechvol_lat <- function(begin, end, r_begin, r_end, print = TRUE) {
  # load packages
  packages <- c("raster", "geosphere", "gstat", "splancs", "dplyr", "ggplot2", "here", "knitr", "viridis")
  lapply(packages, library, character.only=T)
  
  # Determine area of entire raster
  # Raster has 68523 cells
  y_bound <- distm(c(-121.3914, 38.2917), c(-121.3914, 38.31267))
  x_bound <- distm(c(-121.3914, 38.2917), c(-121.3686, 38.2917))
  # Calculate area in m^2
  area <- y_bound * x_bound
  # Total area is 4656185 m^2
  
  #deal with NA vals
  #complete.cases selects only the rows that are complete, eliminating all rows containing NA values
  #need to also do this in formatting function.. somehow
  begin <- begin[complete.cases(begin), ]
  end <- end[complete.cases(end), ]
  
  # define coordinate system
  coordinates(begin) <- ~ long + lat
  coordinates(end) <- ~ long + lat
  
  # load spatial domain to interpolate over
  grid <- raster(begin, ncols=100, nrows=100)
  
  # interpolate highs
  idw_begin <- gstat(formula = begin$level~1, locations = begin, nmax = 6, set = list(idp=2))
  idw_begin <- interpolate(grid, idw_begin)
  
  # interpolate lows
  idw_end <- gstat(formula = end$level~1, locations = end, nmax = 6, set=list(idp=2))
  idw_end <- interpolate(grid, idw_end)
  

  # subtract to find net water levels throughout the event
  event_vol <- abs(idw_end-idw_begin)
  
    # import outline of boundary, as a set of points in lat/long that we generated in ArcMap as a digitized polyline
  bound <- read.csv(here::here("data", "bound_table.csv"), stringsAsFactors = FALSE, header = TRUE)
  bound_pts <- bound[,c(4,5)]
  colnames(bound_pts) <- c('x', 'y')
 
 ###IDW Begin
  # convert raster to SPDF, or at least get some coords
  event_ptsbegin <- coordinates(idw_begin)[!is.na(values(idw_begin)),]
  colnames(event_ptsbegin) <- c('x', 'y')
  
  # clip event idw by the innundation extent generated form our digitized polyline in ArcMap
  library(splancs)
  temp_begin <- rasterToPoints(idw_begin) %>% as.data.frame()
  event_clipbegin <- temp_begin[inout(event_ptsbegin, bound_pts), ]
  colnames(event_clipbegin) <- c('x','y','z')
  
  ###IDW End
  # convert raster to SPDF, or at least get some coords
  event_ptsend<- coordinates(idw_end)[!is.na(values(idw_end)),]
  colnames(event_ptsend) <- c('x', 'y')
  
  # clip event idw by the innundation extent generated form our digitized polyline in ArcMap
  library(splancs)
  temp_end <- rasterToPoints(idw_end) %>% as.data.frame()
  event_clipend <- temp_end[inout(event_ptsend, bound_pts), ]
  colnames(event_clipend) <- c('x','y','z')
  
  
  # plot idwevent_clip as a heatmap
  library(viridis)
  library(magrittr)
 
  #plot begin
  coordinates(event_clipbegin) <- ~ x + y
  gridded(event_clipbegin) <- TRUE
  event_clipbegin <- raster(event_clipbegin, "z")
  plot(event_clipbegin, main = "Low Heads")
  plot(begin, add= TRUE)
  
  #plot end
  coordinates(event_clipend) <- ~ x + y
  gridded(event_clipend) <- TRUE
  event_clipend <- raster(event_clipend, "z")
  plot(event_clipend, main = "High Heads")
  plot(end, add= TRUE)
  
  # write trimmed raster to file
 writeRaster(event_clipbegin, r_begin, format = "GTiff") 
 writeRaster(event_clipend, r_end, format = "GTiff")
  
}
