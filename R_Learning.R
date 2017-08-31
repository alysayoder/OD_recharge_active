install.packages("swirl")
library("swirl")
swirl()
ayoder

# Subsetting and cleaning MW data for water year---- 
MW_11 <- read.csv("MW-11.csv")

mydat <- MW_11
head(mydat)
# make column titles uniform/simple
colnames(mydat) <- c("date", NA, "SWL")
# remove unnecessary columns
mydat <- mydat[,c(1,3)]
# prepare formatting to plot and index
mydat$date <- as.POSIXct(mydat$date, format = '%m/%d/%Y %H:%M')
# subset to desired dates
subset(mydat, date >= "2015-12-31 23:45:00")


# New column names for multiple data frames---- 
lapply(mylist, setNames, nm = coln)

