# Cosumnes Floodplain Recharge Calculations
# setwd()
setwd("C:/Users/ayoder/Desktop/CH")

# load packages
packagelist = list("ggplot2", "colorspace", "reshape2")
lapply(packagelist, library, character.only = TRUE)

# Area Values for acre-ft/acre 

# 2012-2013 Modeled Area----
vert_meters_12_13          <- 2334.83 
horiz_meters_12_13         <- 1307.56
area_meter_squared_12_13   = vert_meters_12_13 * horiz_meters_12_13
acre_12_13            = area_meter_squared_12_13 * 0.000247105
Est_Rech_12_13 <- 490
rech_per_acre_12_13 = Est_Rech_12_13 / acre_12_13
rech_error_12_13 <- 220h
rech_error_per_acre_12_13 <- rech_error_12_13 / acre_12_13

# 2013-2017 Modeled Area----
y <- 2312.51
x <- 1968.21
m2_13_17 <- x * y
acre_13_17 <- m2_13_17 * 0.000247105
Est_Rech_15_16 <- 3180
rech_per_acre_15_16 <- Est_Rech_15_16 / acre_13_17
rech_error_15_16 <- 1430
rech_error_per_acre_15_16 <- rech_error_15_16 / acre_13_17
df <- c(rech_per_acre_12_13, rech_error_per_acre_12_13, rech_per_acre_15_16, rech_error_per_acre_15_16)
