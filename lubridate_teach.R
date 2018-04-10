library(lubridate)
harMet_15min <- read.csv(file="NEON-DS-Met-Time-Series/HARV/FisherTower-Met/hf001-10-15min-m.csv", stringsAsFactors = FALSE)
class(harMet_15min$datetime)
head(harMet_15min)
# convert column to date class
dateOnly_HARV <- as.Date(harMet_15min$datetime)
head(dateOnly_HARV)
# convert character data to date (no time)
myDate <- as.Date("2015-10-19 10:15yoder")
str(myDate)

# convert character data to date and time
timeDate <- as.POSIXct("2015-10-19 10:15")
timeDate

# see data in raw format, i.e. not formatted according to the class type to show us a date we recognize, use unclass() 
unclass(timeDate)

# POSIXlt
timeDatelt <- as.POSIXlt("2015-10-19 10:15")
str(timeDatelt)
timeDatelt
unclass(timeDatelt)

# Convert to Date-time class
# view one date time field
harMet_15min$datetime[1]
# convert single instance of date/time in format year-month-day hour:min:sec
as.POSIXct(harMet_15min$datetime[1], format = "%Y-%m-%dT%H:%M")

# convert entire column to date-time
new.date.time <- as.POSIXct(harMet_15min$datetime, format = "%Y-%m-%dT%H:%M")
head(new.date.time)
class(new.date.time)

