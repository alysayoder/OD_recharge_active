library(ggplot2)
library(timeSeries)
install.packages("ggthemes")
library(ggthemes)
## Load All Well Data ------

setwd("~/Continuous Data")
MW_2 <- read.csv("~/Continuous Data/MW-2.csv")  # import all original files
MW_3 <- read.csv("~/Continuous Data/MW-3.csv")
MW_5 <- read.csv("~/Continuous Data/MW-5.csv")
MW_7 <- read.csv("~/Continuous Data/MW-7.csv")
MW_9 <- read.csv("~/Continuous Data/MW-9.csv")
MW_11 <- read.csv("~/Continuous Data/MW-11.csv")
MW_13<- read.csv("~/Continuous Data/MW-13.csv")
MW_14 <- read.csv("~/Continuous Data/MW-14.csv")
MW_17 <- read.csv("~/Continuous Data/MW-17.csv")
MW_19 <- read.csv("~/Continuous Data/MW-19.csv")
MW_20 <- read.csv("~/Continuous Data/MW-20.csv")
MW_22 <- read.csv("~/Continuous Data/MW-22.csv")
MW_23 <- read.csv("~/Continuous Data/MW-23.csv")
MW_CP1 <- read.csv("~/Continuous Data/MW-CP1.csv")
Oneto_Ag <- read.csv("~/Continuous Data/Oneto_Ag.csv")

## General Function------

Hydrograph <- function (DataSet, Start_time, End_time, Plot_title) 
  # Choose Well to input as a DataSet from those loaded
  # Input times in format of 'mm/dd/yyyy hh:mm'
  # Input Plot_title as a character in quotations, i.e. "MW 5 August 2014"
  # Produces Output Hydrograph of custom time interval for well of choice
  { 
    
  Dates <- DataSet[,1]
  DataSet[,3] <- as.numeric(DataSet[,3])
  
  SWL <- interpNA(DataSet[,3], method = "linear")
  Complete_Set <- cbind(Dates, SWL)
  Complete_Set <- data.frame(Complete_Set)
  Dates <- as.POSIXct(DataSet[,1], format = '%m/%d/%Y %H:%M')
  Complete_Set[,1] <- Dates
  colnames(Complete_Set) <- c('Date', 'SWL')
  Complete_Plot <- ggplot(data = Complete_Set, aes(x = Date, y = SWL)) + 
    geom_point(color = "springgreen4") + 
    labs(title = "Complete Time Series", y = "SWL(m)") + 
    geom_path(size = 2, color = "springgreen4") +
    theme_bw()
  
  Check_start <- as.POSIXct(Start_time, format = '%m/%d/%Y %H:%M')
  Check_end <- as.POSIXct(End_time, format = '%m/%d/%Y %H:%M')
  
  specific_Dates <- Dates[which(Dates == Check_start): which(Dates == Check_end)]
  specific_Data <- DataSet[,3][which(Dates == Check_start): which(Dates == Check_end)]
  specific_Set <- cbind(specific_Dates, specific_Data)
  
  specific_Set <- data.frame(specific_Set)
  specific_Set[,1] <- as.POSIXct(specific_Dates, format = '%m/%d/%Y %H:%M')
  colnames(specific_Set) <- c('Date', 'SWL')
  specific_Plot <- ggplot(data = specific_Set, aes(x = Date, y = SWL)) + 
    labs(title = Plot_title, y = "SWL(m)") + 
    geom_line(colour = "purple") +
    theme_bw()
  
  
  specific_Plot
  }
  


## Define Dates and Data Columns to Create Dataframe------

MW_2_Dates <- MW_2$Date.Time..PDT.
MW_2$SWL..m. <- as.numeric(MW_2$SWL..m.)
MW_2_SWL <- interpNA(MW_2$SWL..m., method = "linear")
MW_2_Total <- cbind(MW_2_Dates, MW_2_SWL)
MW_2_Total <- data.frame(MW_2_Total)
MW_2_Dates <- as.POSIXct(MW_2$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_2_Total[,1] <- MW_2_Dates
colnames(MW_2_Total) <- c('Date', 'SWL')


MW_3_Dates <- MW_3$Date.Time..PDT.
MW_3$SWL..m. <- as.numeric(MW_3$SWL..m.)
MW_3_SWL <- interpNA(MW_3$SWL..m., method = "linear")
MW_3_Total <- cbind(MW_3_Dates, MW_3_SWL)
MW_3_Total <- data.frame(MW_3_Total)
MW_3_Dates <- as.POSIXct(MW_3$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_3_Total[,1] <- MW_3_Dates
colnames(MW_3_Total) <- c('Date', 'SWL')


MW_5_Dates <- MW_5$Date.Time..PDT.
MW_5$SWL..m. <- as.numeric(as.character(MW_5$SWL..m.))
MW_5_SWL <- interpNA(MW_5$SWL..m., method = "linear")
MW_5_Total <- cbind(MW_5_Dates, MW_5_SWL)
MW_5_Total <- data.frame(MW_5_Total)
MW_5_Dates <- as.POSIXct(MW_5$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_5_Total[,1] <- MW_5_Dates
colnames(MW_5_Total) <- c('Date', 'SWL')


MW_7_Dates <- MW_7$Date.Time..PDT.
MW_7$SWL..m. <- as.numeric(MW_7$SWL..m.)
MW_7_SWL <- interpNA(MW_7$SWL..m., method = "linear")
MW_7_Total <- cbind(MW_7_Dates, MW_7_SWL)
MW_7_Total <- data.frame(MW_7_Total)
MW_7_Dates <- as.POSIXct(MW_7$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_7_Total[,1] <- MW_7_Dates
colnames(MW_7_Total) <- c('Date', 'SWL')


MW_9_Dates <- MW_9$Date.Time..PDT.
MW_9$SWL..m. <- as.numeric(MW_9$SWL..m.)
MW_9_SWL <- interpNA(MW_9$SWL..m., method = "linear")
MW_9_Total <- cbind(MW_9_Dates, MW_9_SWL)
MW_9_Total <- data.frame(MW_9_Total)
MW_9_Dates <- as.POSIXct(MW_9$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_9_Total[,1] <- MW_9_Dates
colnames(MW_9_Total) <- c('Date', 'SWL')


MW_11_Dates <- MW_11$Date.Time..PDT.
MW_11$SWL..m. <- as.numeric(MW_11$SWL..m.)
MW_11_SWL <- interpNA(MW_11$SWL..m., method = "linear")
MW_11_Total <- cbind(MW_11_Dates, MW_11_SWL)
MW_11_Total <- data.frame(MW_11_Total)
MW_11_Dates <- as.POSIXct(MW_11$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_11_Total[,1] <- MW_11_Dates
colnames(MW_11_Total) <- c('Date', 'SWL')


MW_13_Dates <- MW_13$Date.Time..PDT.
MW_13$SWL..m. <- as.numeric(MW_13$SWL..m.)
MW_13_SWL <- interpNA(MW_13$SWL..m., method = "linear")
MW_13_Total <- cbind(MW_13_Dates, MW_9_SWL)
MW_13_Total <- data.frame(MW_13_Total)
MW_13_Dates <- as.POSIXct(MW_13$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_13_Total[,1] <- MW_13_Dates
colnames(MW_13_Total) <- c('Date', 'SWL')


MW_14_Dates <- MW_14$Date.Time..PDT.
MW_14$SWL..m. <- as.numeric(MW_14$SWL..m.)
MW_14_SWL <- interpNA(MW_14$SWL..m., method = "linear")
MW_14_Total <- cbind(MW_14_Dates, MW_14_SWL)
MW_14_Total <- data.frame(MW_14_Total)
MW_14_Dates <- as.POSIXct(MW_14$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_14_Total[,1] <- MW_14_Dates
colnames(MW_14_Total) <- c('Date', 'SWL')


MW_17_Dates <- MW_17$Date.Time..PDT.
MW_17$SWL..m. <- as.numeric(MW_17$SWL..m.)
MW_17_SWL <- interpNA(MW_17$SWL..m., method = "linear")
MW_17_Total <- cbind(MW_17_Dates, MW_17_SWL)
MW_17_Total <- data.frame(MW_17_Total)
MW_17_Dates <- as.POSIXct(MW_17$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_17_Total[,1] <- MW_17_Dates
colnames(MW_17_Total) <- c('Date', 'SWL')


MW_19_Dates <- MW_19$Date.Time..PDT.
MW_19$SWL..m. <- as.numeric(MW_19$SWL..m.)
MW_19_SWL <- interpNA(MW_19$SWL..m., method = "linear")
MW_19_Total <- cbind(MW_19_Dates, MW_19_SWL)
MW_19_Total <- data.frame(MW_19_Total)
MW_19_Dates <- as.POSIXct(MW_19$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_19_Total[,1] <- MW_19_Dates
colnames(MW_19_Total) <- c('Date', 'SWL')


MW_20_Dates <- MW_20$Date.Time..PDT.
MW_20$SWL..m. <- as.numeric(MW_20$SWL..m.)
MW_20_SWL <- interpNA(MW_20$SWL..m., method = "linear")
MW_20_Total <- cbind(MW_20_Dates, MW_20_SWL)
MW_20_Total <- data.frame(MW_20_Total)
MW_20_Dates <- as.POSIXct(MW_20$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_20_Total[,1] <- MW_20_Dates
colnames(MW_20_Total) <- c('Date', 'SWL')


MW_22_Dates <- MW_22$Date.Time..PDT.
MW_22$SWL..m. <- as.numeric(MW_22$SWL..m.)
MW_22_SWL <- interpNA(MW_22$SWL..m., method = "linear")
MW_22_Total <- cbind(MW_22_Dates, MW_22_SWL)
MW_22_Total <- data.frame(MW_22_Total)
MW_22_Dates <- as.POSIXct(MW_22$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_22_Total[,1] <- MW_22_Dates
colnames(MW_22_Total) <- c('Date', 'SWL')


MW_23_Dates <- MW_23$Date.Time..PDT.
MW_23$SWL..m. <- as.numeric(MW_23$SWL..m.)
MW_23_SWL <- interpNA(MW_23$SWL..m., method = "linear")
MW_23_Total <- cbind(MW_23_Dates, MW_23_SWL)
MW_23_Total <- data.frame(MW_23_Total)
MW_23_Dates <- as.POSIXct(MW_23$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_23_Total[,1] <- MW_23_Dates
colnames(MW_23_Total) <- c('Date', 'SWL')


MW_CP1_Dates <- MW_CP1$Date.Time..PDT.
MW_CP1$SWL..m. <- as.numeric(MW_CP1$SWL..m.)
MW_CP1_SWL <- interpNA(MW_CP1$SWL..m., method = "linear")
MW_CP1_Total <- cbind(MW_CP1_Dates, MW_CP1_SWL)
MW_CP1_Total <- data.frame(MW_CP1_Total)
MW_CP1_Dates <- as.POSIXct(MW_CP1$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_CP1_Total[,1] <- MW_CP1_Dates
colnames(MW_CP1_Total) <- c('Date', 'SWL')


OA_Dates <- OA$Date.Time..PDT.
OA$SWL..m. <- as.numeric(OA$SWL..m.)
OA_SWL <- interpNA(OA$SWL..m., method = "linear")
OA_Total <- cbind(OA_Dates, OA_SWL)
OA_Total <- data.frame(OA_Total)
OA_Dates <- as.POSIXct(OA$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
OA_Total[,1] <- OA_Dates
colnames(OA_Total) <- c('Date', 'SWL')

## Pick Specific Date-------

# Yearly----

MW_2_2015_Dates <- MW_2_Dates[which(MW_2_Dates == "2014-10-01 05:00:00 PST"): which(MW_2_Dates == "2015-09-30 22:00:00 PST")]
MW_2_2015_Data <- MW_2$SWL..m.[which(MW_2_Dates == "2014-10-01 05:00:00 PST"): which(MW_2_Dates == "2015-09-30 22:00:00 PST")]
MW_2_2015 <- cbind(MW_2_2015_Dates, MW_2_2015_Data)
MW_2_2015 <- data.frame(MW_2_2015)
MW_2_2015[,1] <- as.POSIXct(MW_2_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015) <- c('Date', 'SWL')


MW_3_2015_Dates <- MW_3_Dates[which(MW_3_Dates == "2014-10-01 05:00:00 PST"): which(MW_3_Dates == "2015-09-30 22:00:00 PST")]
MW_3_2015_Data <- MW_3$SWL..m.[which(MW_3_Dates == "2014-10-01 05:00:00 PST"): which(MW_3_Dates == "2015-09-30 22:00:00 PST")]
MW_3_2015 <- cbind(MW_3_2015_Dates, MW_3_2015_Data)
MW_3_2015 <- data.frame(MW_3_2015)
MW_3_2015[,1] <- as.POSIXct(MW_3_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015) <- c('Date', 'SWL')


MW_5_2015_Dates <- MW_5_Dates[which(MW_5_Dates == "2014-10-01 05:00:00 PST"): which(MW_5_Dates == "2015-09-30 22:00:00 PST")]
MW_5_2015_Data <- MW_5$SWL..m.[which(MW_5_Dates == "2014-10-01 05:00:00 PST"): which(MW_5_Dates == "2015-09-30 22:00:00 PST")]
MW_5_2015 <- cbind(MW_5_2015_Dates, MW_5_2015_Data)
MW_5_2015 <- data.frame(MW_5_2015)
MW_5_2015[,1] <- as.POSIXct(MW_5_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015) <- c('Date', 'SWL')


MW_7_2015_Dates <- MW_7_Dates[which(MW_7_Dates == "2014-10-01 05:00:00 PST"): which(MW_7_Dates == "2015-09-30 22:00:00 PST")]
MW_7_2015_Data <- MW_7$SWL..m.[which(MW_7_Dates == "2014-10-01 05:00:00 PST"): which(MW_7_Dates == "2015-09-30 22:00:00 PST")]
MW_7_2015 <- cbind(MW_7_2015_Dates, MW_7_2015_Data)
MW_7_2015 <- data.frame(MW_7_2015)
MW_7_2015[,1] <- as.POSIXct(MW_7_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015) <- c('Date', 'SWL')


MW_9_2015_Dates <- MW_9_Dates[which(MW_9_Dates == "2014-10-01 05:00:00 PST"): which(MW_9_Dates == "2015-09-30 22:32:00 PST")]
MW_9_2015_Data <- MW_9$SWL..m.[which(MW_9_Dates == "2014-10-01 05:00:00 PST"): which(MW_9_Dates == "2015-09-30 22:32:00 PST")]
MW_9_2015 <- cbind(MW_9_2015_Dates, MW_9_2015_Data)
MW_9_2015 <- data.frame(MW_9_2015)
MW_9_2015[,1] <- as.POSIXct(MW_9_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015) <- c('Date', 'SWL')


MW_11_2015_Dates <- MW_11_Dates[which(MW_11_Dates == "2014-10-01 05:00:00 PST"): which(MW_11_Dates == "2015-09-30 23:02:00 PST")]
MW_11_2015_Data <- MW_11$SWL..m.[which(MW_11_Dates == "2014-10-01 05:00:00 PST"): which(MW_11_Dates == "2015-09-30 23:02:00 PST")]
MW_11_2015 <- cbind(MW_11_2015_Dates, MW_11_2015_Data)
MW_11_2015 <- data.frame(MW_11_2015)
MW_11_2015[,1] <- as.POSIXct(MW_11_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015) <- c('Date', 'SWL')


MW_13_2015_Dates <- MW_13_Dates[which(MW_13_Dates == "2015-10-01 05:02:00 PST"): which(MW_13_Dates == "2016-09-30 23:00:00 PST")]
MW_13_2015_Data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-10-01 05:02:00 PST"): which(MW_13_Dates == "2016-09-30 23:00:00 PST")]
MW_13_2015 <- cbind(MW_13_2015_Dates, MW_13_2015_Data)
MW_13_2015 <- data.frame(MW_13_2015)
MW_13_2015[,1] <- as.POSIXct(MW_13_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015) <- c('Date', 'SWL')


MW_14_2015_Dates <- MW_14_Dates[which(MW_14_Dates == "2014-10-01 05:00:00 PST"): which(MW_14_Dates == "2015-09-30 22:32:00 PST")]
MW_14_2015_Data <- MW_14$SWL..m.[which(MW_14_Dates == "2014-10-01 05:00:00 PST"): which(MW_14_Dates == "2015-09-30 22:32:00 PST")]
MW_14_2015 <- cbind(MW_14_2015_Dates, MW_14_2015_Data)
MW_14_2015 <- data.frame(MW_14_2015)
MW_14_2015[,1] <- as.POSIXct(MW_14_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015) <- c('Date', 'SWL')


MW_17_2015_Dates <- MW_17_Dates[which(MW_17_Dates == "2014-10-01 05:00:00 PST"): which(MW_17_Dates == "2015-09-30 22:32:00 PST")]
MW_17_2015_Data <- MW_17$SWL..m.[which(MW_17_Dates == "2014-10-01 05:00:00 PST"): which(MW_17_Dates == "2015-09-30 22:32:00 PST")]
MW_17_2015 <- cbind(MW_17_2015_Dates, MW_17_2015_Data)
MW_17_2015 <- data.frame(MW_17_2015)
MW_17_2015[,1] <- as.POSIXct(MW_17_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015) <- c('Date', 'SWL')


MW_19_2015_Dates <- MW_19_Dates[which(MW_19_Dates == "2014-10-01 05:00:00 PST"): which(MW_19_Dates == "2015-09-30 22:32:00 PST")]
MW_19_2015_Data <- MW_19$SWL..m.[which(MW_19_Dates == "2014-10-01 05:00:00 PST"): which(MW_19_Dates == "2015-09-30 22:32:00 PST")]
MW_19_2015 <- cbind(MW_19_2015_Dates, MW_19_2015_Data)
MW_19_2015 <- data.frame(MW_19_2015)
MW_19_2015[,1] <- as.POSIXct(MW_19_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015) <- c('Date', 'SWL')


MW_20_2015_Dates <- MW_20_Dates[which(MW_20_Dates == "2014-10-01 05:00:00 PST"): which(MW_20_Dates == "2015-09-30 22:32:00 PST")]
MW_20_2015_Data <- MW_20$SWL..m.[which(MW_20_Dates == "2014-10-01 05:00:00 PST"): which(MW_20_Dates == "2015-09-30 22:32:00 PST")]
MW_20_2015 <- cbind(MW_20_2015_Dates, MW_20_2015_Data)
MW_20_2015 <- data.frame(MW_20_2015)
MW_20_2015[,1] <- as.POSIXct(MW_20_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015) <- c('Date', 'SWL')


MW_22_2015_Dates <- MW_22_Dates[which(MW_22_Dates == "2014-10-01 05:00:00 PST"): which(MW_22_Dates == "2015-09-30 22:32:00 PST")]
MW_22_2015_Data <- MW_22$SWL..m.[which(MW_22_Dates == "2014-10-01 05:00:00 PST"): which(MW_22_Dates == "2015-09-30 22:32:00 PST")]
MW_22_2015 <- cbind(MW_22_2015_Dates, MW_22_2015_Data)
MW_22_2015 <- data.frame(MW_22_2015)
MW_22_2015[,1] <- as.POSIXct(MW_22_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015) <- c('Date', 'SWL')


MW_23_2015_Dates <- MW_23_Dates[which(MW_23_Dates == "2014-10-01 05:00:00 PST"): which(MW_23_Dates == "2015-09-30 22:32:00 PST")]
MW_23_2015_Data <- MW_23$SWL..m.[which(MW_23_Dates == "2014-10-01 05:00:00 PST"): which(MW_23_Dates == "2015-09-30 22:32:00 PST")]
MW_23_2015 <- cbind(MW_23_2015_Dates, MW_23_2015_Data)
MW_23_2015 <- data.frame(MW_23_2015)
MW_23_2015[,1] <- as.POSIXct(MW_23_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015) <- c('Date', 'SWL')


MW_CP1_2015_Dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2014-10-01 05:00:00 PST"): which(MW_CP1_Dates == "2015-09-30 22:32:00 PST")]
MW_CP1_2015_Data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2014-10-01 05:00:00 PST"): which(MW_CP1_Dates == "2015-09-30 22:32:00 PST")]
MW_CP1_2015 <- cbind(MW_CP1_2015_Dates, MW_CP1_2015_Data)
MW_CP1_2015 <- data.frame(MW_CP1_2015)
MW_CP1_2015[,1] <- as.POSIXct(MW_CP1_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015) <- c('Date', 'SWL')


OA_2015_Dates <- OA_Dates[which(OA_Dates == "2014-10-01 05:00:00 PST"): which(OA_Dates == "2015-09-30 22:32:00 PST")]
OA_2015_Data <- OA$SWL..m.[which(OA_Dates == "2014-10-01 05:00:00 PST"): which(OA_Dates == "2015-09-30 22:32:00 PST")]
OA_2015 <- cbind(OA_2015_Dates, OA_2015_Data)
OA_2015 <- data.frame(OA_2015)
OA_2015[,1] <- as.POSIXct(OA_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015) <- c('Date', 'SWL')

# Monthly----

MW_2_2015_april_dates <- MW_2_Dates[which(MW_2_Dates == "2015-04-01 02:00:00 PST"): which(MW_2_Dates == "2015-04-30 23:00:00 PST")]
MW_2_2015_april_data <- MW_2$SWL..m.[which(MW_2_Dates == "2015-04-01 02:00:00 PST"): which(MW_2_Dates == "2015-04-30 23:00:00 PST")]
MW_2_2015_april <- cbind(MW_2_2015_april_dates, MW_2_2015_april_data)
MW_2_2015_april <- data.frame(MW_2_2015_april)
MW_2_2015_april[,1] <- as.POSIXct(MW_2_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015_april) <- c('Date', 'SWL')


MW_3_2015_april_dates <- MW_3_Dates[which(MW_3_Dates == "2015-04-01 02:00:00 PST"): which(MW_3_Dates == "2015-04-30 23:00:00 PST")]
MW_3_2015_april_data <- MW_3$SWL..m.[which(MW_3_Dates == "2015-04-01 02:00:00 PST"): which(MW_3_Dates == "2015-04-30 23:00:00 PST")]
MW_3_2015_april <- cbind(MW_3_2015_april_dates, MW_3_2015_april_data)
MW_3_2015_april <- data.frame(MW_3_2015_april)
MW_3_2015_april[,1] <- as.POSIXct(MW_3_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015_april) <- c('Date', 'SWL')


MW_5_2015_april_dates <- MW_5_Dates[which(MW_5_Dates == "2015-04-01 02:00:00 PST"): which(MW_5_Dates == "2015-04-30 23:00:00 PST")]
MW_5_2015_april_data <- MW_5$SWL..m.[which(MW_5_Dates == "2015-04-01 02:00:00 PST"): which(MW_5_Dates == "2015-04-30 23:00:00 PST")]
MW_5_2015_april <- cbind(MW_5_2015_april_dates, MW_5_2015_april_data)
MW_5_2015_april <- data.frame(MW_5_2015_april)
MW_5_2015_april[,1] <- as.POSIXct(MW_5_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015_april) <- c('Date', 'SWL')


MW_7_2015_april_dates <- MW_7_Dates[which(MW_7_Dates == "2015-04-01 02:00:00 PST"): which(MW_7_Dates == "2015-04-30 23:00:00 PST")]
MW_7_2015_april_data <- MW_7$SWL..m.[which(MW_7_Dates == "2015-04-01 02:00:00 PST"): which(MW_7_Dates == "2015-04-30 23:00:00 PST")]
MW_7_2015_april <- cbind(MW_7_2015_april_dates, MW_7_2015_april_data)
MW_7_2015_april <- data.frame(MW_7_2015_april)
MW_7_2015_april[,1] <- as.POSIXct(MW_7_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015_april) <- c('Date', 'SWL')


MW_9_2015_april_dates <- MW_9_Dates[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_9_Dates == "2015-04-30 23:02:00 PST")]
MW_9_2015_april_data <- MW_9$SWL..m.[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_9_Dates == "2015-04-30 23:02:00 PST")]
MW_9_2015_april <- cbind(MW_9_2015_april_dates, MW_9_2015_april_data)
MW_9_2015_april <- data.frame(MW_9_2015_april)
MW_9_2015_april[,1] <- as.POSIXct(MW_9_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015_april) <- c('Date', 'SWL')


MW_11_2015_april_dates <- MW_11_Dates[which(MW_11_Dates == "2015-04-01 02:02:00 PST"): which(MW_11_Dates == "2015-04-30 23:02:00 PST")]
MW_11_2015_april_data <- MW_11$SWL..m.[which(MW_11_Dates == "2015-04-01 02:02:00 PST"): which(MW_11_Dates == "2015-04-30 23:02:00 PST")]
MW_11_2015_april <- cbind(MW_11_2015_april_dates, MW_11_2015_april_data)
MW_11_2015_april <- data.frame(MW_11_2015_april)
MW_11_2015_april[,1] <- as.POSIXct(MW_11_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015_april) <- c('Date', 'SWL')


MW_13_2015_april_dates <- MW_13_Dates[which(MW_13_Dates == "2015-04-01 02:02:00 PST"): which(MW_13_Dates == "2015-04-30 23:02:00 PST")]
MW_13_2015_april_data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-04-01 02:02:00 PST"): which(MW_13_Dates == "2015-04-30 23:02:00 PST")]
MW_13_2015_april <- cbind(MW_13_2015_april_dates, MW_13_2015_april_data)
MW_13_2015_april <- data.frame(MW_13_2015_april)
MW_13_2015_april[,1] <- as.POSIXct(MW_13_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015_april) <- c('Date', 'SWL')


MW_14_2015_april_dates <- MW_14_Dates[which(MW_14_Dates == "2015-04-01 02:02:00 PST"): which(MW_14_Dates == "2015-04-30 23:02:00 PST")]
MW_14_2015_april_data <- MW_9$SWL..m.[which(MW_14_Dates == "2015-04-01 02:02:00 PST"): which(MW_14_Dates == "2015-04-30 23:02:00 PST")]
MW_14_2015_april <- cbind(MW_14_2015_april_dates, MW_14_2015_april_data)
MW_14_2015_april <- data.frame(MW_14_2015_april)
MW_14_2015_april[,1] <- as.POSIXct(MW_14_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015_april) <- c('Date', 'SWL')


MW_17_2015_april_dates <- MW_17_Dates[which(MW_17_Dates == "2015-04-01 02:02:00 PST"): which(MW_17_Dates == "2015-04-30 23:02:00 PST")]
MW_17_2015_april_data <- MW_17$SWL..m.[which(MW_17_Dates == "2015-04-01 02:02:00 PST"): which(MW_17_Dates == "2015-04-30 23:02:00 PST")]
MW_17_2015_april <- cbind(MW_17_2015_april_dates, MW_17_2015_april_data)
MW_17_2015_april <- data.frame(MW_17_2015_april)
MW_17_2015_april[,1] <- as.POSIXct(MW_17_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015_april) <- c('Date', 'SWL')


MW_19_2015_april_dates <- MW_19_Dates[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_19_Dates == "2015-04-30 23:02:00 PST")]
MW_19_2015_april_data <- MW_19$SWL..m.[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_19_Dates == "2015-04-30 23:02:00 PST")]
MW_19_2015_april <- cbind(MW_19_2015_april_dates, MW_19_2015_april_data)
MW_19_2015_april <- data.frame(MW_19_2015_april)
MW_19_2015_april[,1] <- as.POSIXct(MW_19_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015_april) <- c('Date', 'SWL')


MW_20_2015_april_dates <- MW_20_Dates[which(MW_20_Dates == "2015-04-01 02:02:00 PST"): which(MW_20_Dates == "2015-04-30 23:02:00 PST")]
MW_20_2015_april_data <- MW_20$SWL..m.[which(MW_20_Dates == "2015-04-01 02:02:00 PST"): which(MW_20_Dates == "2015-04-30 23:02:00 PST")]
MW_20_2015_april <- cbind(MW_20_2015_april_dates, MW_20_2015_april_data)
MW_20_2015_april <- data.frame(MW_20_2015_april)
MW_20_2015_april[,1] <- as.POSIXct(MW_20_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015_april) <- c('Date', 'SWL')


MW_22_2015_april_dates <- MW_22_Dates[which(MW_22_Dates == "2015-04-01 02:02:00 PST"): which(MW_22_Dates == "2015-04-30 23:02:00 PST")]
MW_22_2015_april_data <- MW_22$SWL..m.[which(MW_22_Dates == "2015-04-01 02:02:00 PST"): which(MW_22_Dates == "2015-04-30 23:02:00 PST")]
MW_22_2015_april <- cbind(MW_22_2015_april_dates, MW_22_2015_april_data)
MW_22_2015_april <- data.frame(MW_22_2015_april)
MW_22_2015_april[,1] <- as.POSIXct(MW_22_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015_april) <- c('Date', 'SWL')


MW_23_2015_april_dates <- MW_23_Dates[which(MW_23_Dates == "2015-04-01 02:02:00 PST"): which(MW_23_Dates == "2015-04-30 23:02:00 PST")]
MW_23_2015_april_data <- MW_23$SWL..m.[which(MW_23_Dates == "2015-04-01 02:02:00 PST"): which(MW_23_Dates == "2015-04-30 23:02:00 PST")]
MW_23_2015_april <- cbind(MW_23_2015_april_dates, MW_23_2015_april_data)
MW_23_2015_april <- data.frame(MW_23_2015_april)
MW_23_2015_april[,1] <- as.POSIXct(MW_23_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015_april) <- c('Date', 'SWL')


MW_CP1_2015_april_dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP12015_april_data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP1_2015_april <- cbind(CP1_2015_april_dates, CP1_2015_april_data)
MW_CP1_2015_april <- data.frame(MW_CP1_2015_april)
MW_CP1_2015_april[,1] <- as.POSIXct(MW_CP1_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015_april) <- c('Date', 'SWL')


OA_2015_april_dates <- OA_Dates[which(OA_Dates == "2015-04-01 02:02:00 PST"): which(OA_Dates == "2015-04-30 23:02:00 PST")]
OA_2015_april_data <- OA$SWL..m.[which(OA_Dates == "2015-04-01 02:02:00 PST"): which(OA_Dates == "2015-04-30 23:02:00 PST")]
OA_2015_april <- cbind(OA_2015_april_dates, OA_2015_april_data)
OA_2015_april <- data.frame(OA_2015_april)
OA_2015_april[,1] <- as.POSIXct(OA_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015_april) <- c('Date', 'SWL')

# Weekly----

MW_2_2015_april_week_dates <- MW_2_Dates[which(MW_2_Dates == "2015-04-15 02:00:00 PST"): which(MW_2_Dates == "2015-04-22 23:00:00 PST")]
MW_2_2015_april_week_data <- MW_2$SWL..m.[which(MW_2_Dates == "2015-04-15 02:00:00 PST"): which(MW_2_Dates == "2015-04-22 23:00:00 PST")]
MW_2_2015_april_week <- cbind(MW_2_2015_april_week_dates, MW_2_2015_april_week_data)
MW_2_2015_april_week <- data.frame(MW_2_2015_april_week)
MW_2_2015_april_week[,1] <- as.POSIXct(MW_2_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015_april_week) <- c('Date', 'SWL')


MW_3_2015_april_week_dates <- MW_3_Dates[which(MW_3_Dates == "2015-04-15 02:00:00 PST"): which(MW_3_Dates == "2015-04-22 23:00:00 PST")]
MW_3_2015_april_week_data <- MW_3$SWL..m.[which(MW_3_Dates == "2015-04-15 02:00:00 PST"): which(MW_3_Dates == "2015-04-22 23:00:00 PST")]
MW_3_2015_april_week <- cbind(MW_3_2015_april_week_dates, MW_3_2015_april_week_data)
MW_3_2015_april_week <- data.frame(MW_3_2015_april_week)
MW_3_2015_april_week[,1] <- as.POSIXct(MW_3_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015_april_week) <- c('Date', 'SWL')


MW_5_2015_april_week_dates <- MW_5_Dates[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_5_Dates == "2015-04-22 23:00:00 PST")]
MW_5_2015_april_week_data <- MW_5$SWL..m.[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_5_Dates == "2015-04-22 23:00:00 PST")]
MW_5_2015_april_week <- cbind(MW_5_2015_april_week_dates, MW_5_2015_april_week_data)
MW_5_2015_april_week <- data.frame(MW_5_2015_april_week)
MW_5_2015_april_week[,1] <- as.POSIXct(MW_5_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015_april_week) <- c('Date', 'SWL')


MW_7_2015_april_week_dates <- MW_7_Dates[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_7_Dates == "2015-04-22 23:00:00 PST")]
MW_7_2015_april_week_data <- MW_7$SWL..m.[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_7_Dates == "2015-04-22 23:00:00 PST")]
MW_7_2015_april_week <- cbind(MW_7_2015_april_week_dates, MW_7_2015_april_week_data)
MW_7_2015_april_week <- data.frame(MW_7_2015_april_week)
MW_7_2015_april_week[,1] <- as.POSIXct(MW_7_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015_april_week) <- c('Date', 'SWL')


MW_9_2015_april_week_dates <- MW_9_Dates[which(MW_9_Dates == "2015-04-15 02:02:00 PST"): which(MW_9_Dates == "2015-04-22 23:02:00 PST")]
MW_9_2015_april_week_data <- MW_9$SWL..m.[which(MW_9_Dates == "2015-04-15 02:02:00 PST"): which(MW_9_Dates == "2015-04-22 23:02:00 PST")]
MW_9_2015_april_week <- cbind(MW_9_2015_april_week_dates, MW_9_2015_april_week_data)
MW_9_2015_april_week <- data.frame(MW_9_2015_april_week)
MW_9_2015_april_week[,1] <- as.POSIXct(MW_9_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015_april_week) <- c('Date', 'SWL')


MW_11_2015_april_week_dates <- MW_11_Dates[which(MW_11_Dates == "2015-04-15 02:02:00 PST"): which(MW_11_Dates == "2015-04-22 23:02:00 PST")]
MW_11_2015_april_week_data <- MW_11$SWL..m.[which(MW_11_Dates == "2015-04-15 02:02:00 PST"): which(MW_11_Dates == "2015-04-22 23:02:00 PST")]
MW_11_2015_april_week <- cbind(MW_11_2015_april_week_dates, MW_11_2015_april_week_data)
MW_11_2015_april_week <- data.frame(MW_11_2015_april_week)
MW_11_2015_april_week[,1] <- as.POSIXct(MW_11_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015_april_week) <- c('Date', 'SWL')


MW_13_2015_april_week_dates <- MW_13_Dates[which(MW_13_Dates == "2015-04-15 02:02:00 PST"): which(MW_13_Dates == "2015-04-22 23:02:00 PST")]
MW_13_2015_april_week_data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-04-15 02:02:00 PST"): which(MW_13_Dates == "2015-04-22 23:02:00 PST")]
MW_13_2015_april_week <- cbind(MW_13_2015_april_week_dates, MW_13_2015_april_week_data)
MW_13_2015_april_week <- data.frame(MW_13_2015_april_week)
MW_13_2015_april_week[,1] <- as.POSIXct(MW_13_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015_april_week) <- c('Date', 'SWL')


MW_14_2015_april_week_dates <- MW_14_Dates[which(MW_14_Dates == "2015-04-15 02:02:00 PST"): which(MW_14_Dates == "2015-04-22 23:02:00 PST")]
MW_14_2015_april_week_data <- MW_14$SWL..m.[which(MW_14_Dates == "2015-04-15 02:02:00 PST"): which(MW_14_Dates == "2015-04-22 23:02:00 PST")]
MW_14_2015_april_week <- cbind(MW_14_2015_april_week_dates, MW_14_2015_april_week_data)
MW_14_2015_april_week <- data.frame(MW_14_2015_april_week)
MW_14_2015_april_week[,1] <- as.POSIXct(MW_14_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015_april_week) <- c('Date', 'SWL')


MW_17_2015_april_week_dates <- MW_17_Dates[which(MW_17_Dates == "2015-04-15 02:02:00 PST"): which(MW_17_Dates == "2015-04-22 23:02:00 PST")]
MW_17_2015_april_week_data <- MW_17$SWL..m.[which(MW_17_Dates == "2015-04-15 02:02:00 PST"): which(MW_17_Dates == "2015-04-22 23:02:00 PST")]
MW_17_2015_april_week <- cbind(MW_17_2015_april_week_dates, MW_17_2015_april_week_data)
MW_17_2015_april_week <- data.frame(MW_17_2015_april_week)
MW_17_2015_april_week[,1] <- as.POSIXct(MW_17_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015_april_week) <- c('Date', 'SWL')


MW_19_2015_april_week_dates <- MW_19_Dates[which(MW_19_Dates == "2015-04-15 02:02:00 PST"): which(MW_19_Dates == "2015-04-22 23:02:00 PST")]
MW_19_2015_april_week_data <- MW_19$SWL..m.[which(MW_19_Dates == "2015-04-15 02:02:00 PST"): which(MW_19_Dates == "2015-04-22 23:02:00 PST")]
MW_19_2015_april_week <- cbind(MW_19_2015_april_week_dates, MW_19_2015_april_week_data)
MW_19_2015_april_week <- data.frame(MW_19_2015_april_week)
MW_19_2015_april_week[,1] <- as.POSIXct(MW_19_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015_april_week) <- c('Date', 'SWL')


MW_20_2015_april_week_dates <- MW_20_Dates[which(MW_20_Dates == "2015-04-15 02:02:00 PST"): which(MW_20_Dates == "2015-04-22 23:02:00 PST")]
MW_20_2015_april_week_data <- MW_20$SWL..m.[which(MW_20_Dates == "2015-04-15 02:02:00 PST"): which(MW_20_Dates == "2015-04-22 23:02:00 PST")]
MW_20_2015_april_week <- cbind(MW_20_2015_april_week_dates, MW_20_2015_april_week_data)
MW_20_2015_april_week <- data.frame(MW_20_2015_april_week)
MW_20_2015_april_week[,1] <- as.POSIXct(MW_20_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015_april_week) <- c('Date', 'SWL')


MW_22_2015_april_week_dates <- MW_22_Dates[which(MW_22_Dates == "2015-04-15 02:02:00 PST"): which(MW_22_Dates == "2015-04-22 23:02:00 PST")]
MW_22_2015_april_week_data <- MW_22$SWL..m.[which(MW_22_Dates == "2015-04-15 02:02:00 PST"): which(MW_22_Dates == "2015-04-22 23:02:00 PST")]
MW_22_2015_april_week <- cbind(MW_22_2015_april_week_dates, MW_22_2015_april_week_data)
MW_22_2015_april_week <- data.frame(MW_22_2015_april_week)
MW_22_2015_april_week[,1] <- as.POSIXct(MW_22_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015_april_week) <- c('Date', 'SWL')


MW_23_2015_april_week_dates <- MW_23_Dates[which(MW_23_Dates == "2015-04-15 02:02:00 PST"): which(MW_23_Dates == "2015-04-22 23:02:00 PST")]
MW_23_2015_april_week_data <- MW_23$SWL..m.[which(MW_23_Dates == "2015-04-15 02:02:00 PST"): which(MW_23_Dates == "2015-04-22 23:02:00 PST")]
MW_23_2015_april_week <- cbind(MW_23_2015_april_week_dates, MW_23_2015_april_week_data)
MW_23_2015_april_week <- data.frame(MW_23_2015_april_week)
MW_23_2015_april_week[,1] <- as.POSIXct(MW_23_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015_april_week) <- c('Date', 'SWL')


MW_CP1_2015_april_dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP12015_april_data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP1_2015_april <- cbind(CP1_2015_april_dates, CP1_2015_april_data)
MW_CP1_2015_april <- data.frame(MW_CP1_2015_april)
MW_CP1_2015_april[,1] <- as.POSIXct(MW_CP1_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015_april) <- c('Date', 'SWL')


OA_2015_april_week_dates <- OA_Dates[which(OA_Dates == "2015-04-15 02:02:00 PST"): which(OA_Dates == "2015-04-22 23:02:00 PST")]
OA_2015_april_week_data <- OA$SWL..m.[which(OA_Dates == "2015-04-15 02:02:00 PST"): which(OA_Dates == "2015-04-22 23:02:00 PST")]
OA_2015_april_week <- cbind(OA_2015_april_week_dates, OA_2015_april_week_data)
OA_2015_april_week <- data.frame(OA_2015_april_week)
OA_2015_april_week[,1] <- as.POSIXct(OA_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015_april_week) <- c('Date', 'SWL')

## Create Hydrographs------

MW_2_Complete <- ggplot(data = MW_2_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 2 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

MW_3_Complete <- ggplot(data = MW_3_Total, aes(x = Date, y = SWL)) +
  geom_point() +
  labs(title = "MW 3 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

MW_5_Complete <- ggplot(data = MW_5_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 5 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) + 
  theme_bw()

MW_7_Complete <- ggplot(data = MW_7_Total, aes(x = Date, y = SWL)) +
  geom_point() +
  labs(title = "MW 7 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

MW_9_Complete <- ggplot(data = MW_9_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

MW_11_Complete <- ggplot(data = MW_11_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 11 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

MW_13_Complete <- ggplot(data = MW_13_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 13 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_14_Complete <- ggplot(data = MW_14_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 14 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_17_Complete <- ggplot(data = MW_17_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_19_Complete <- ggplot(data = MW_19_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_20_Complete <- ggplot(data = MW_20_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 20 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_22_Complete <- ggplot(data = MW_22_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 22 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_23_Complete <- ggplot(data = MW_23_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 23 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


MW_CP1_Complete <- ggplot(data = MW_CP1_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW CP1 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()


OA_Complete <- ggplot(data = OA_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "Oneto Ag Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()

# Yearly----

MW_2_OneYear <- ggplot(data = MW_2_2015, aes(x = Date, y = SWL)) + 
  geom_point(colour = "springgreen4") +
  labs(title = "MW 2 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") +
  theme_bw()

MW_3_OneYear <- ggplot(data = MW_3_2015, aes(x = Date, y = SWL)) + 
  geom_point(colour = "springgreen4") + 
  labs(title = "MW 3 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") +
  theme_bw()

MW_5_OneYear <- ggplot(data = MW_5_2015, aes(x = Date, y = SWL)) + 
  geom_point(colour = "springgreen4") +
  labs(title = "MW 5 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") +
  theme_bw()

MW_7_OneYear <- ggplot(data = MW_7_2015, aes(x = Date, y = SWL)) + 
  geom_point(colour = "springgreen4") + 
  labs(title = "MW 7 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_9_OneYear <- ggplot(data = MW_9_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 9 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_11_OneYear <- ggplot(data = MW_11_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 11 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_13_OneYear <- ggplot(data = MW_13_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 13 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_14_OneYear <- ggplot(data = MW_14_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 14 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_17_OneYear <- ggplot(data = MW_17_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 17 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_19_OneYear <- ggplot(data = MW_19_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 19 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_20_OneYear <- ggplot(data = MW_20_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 20 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_22_OneYear <- ggplot(data = MW_22_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 22 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_23_OneYear <- ggplot(data = MW_23_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 23 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

MW_CP1_OneYear <- ggplot(data = MW_CP1_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW CP1 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

OA_OneYear <- ggplot(data = OA_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "Oneto Ag - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()

# Monthly----

MW_2_OneMonth <- ggplot(data = MW_2_2015_april, aes(x = Date, y = SWL)) + 
  labs(title = "MW 2 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_3_OneMonth <- ggplot(data = MW_3_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 3 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") +
  theme_bw()

MW_5_OneMonth <- ggplot(data = MW_5_2015_april, aes(x = Date, y = SWL)) + 
  labs(title = "MW 5 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") +
  theme_bw()

MW_7_OneMonth <- ggplot(data = MW_7_2015_april, aes(x = Date, y = SWL)) + 
  labs(title = "MW 7 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_9_OneMonth <- ggplot(data = MW_9_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 9 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_11_OneMonth <- ggplot(data = MW_11_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 11 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_13_OneMonth <- ggplot(data = MW_13_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 13 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_14_OneMonth <- ggplot(data = MW_14_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 14 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_17_OneMonth <- ggplot(data = MW_17_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 17 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_19_OneMonth <- ggplot(data = MW_19_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 19 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_20_OneMonth <- ggplot(data = MW_20_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 20 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_22_OneMonth <- ggplot(data = MW_22_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 22 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_23_OneMonth <- ggplot(data = MW_23_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 23 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

MW_CP1_OneMonth <- ggplot(data = MW_CP1_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW CP1 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

OA_OneMonth <- ggplot(data = OA_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "Oneto Ag - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()

# Weekly----

MW_2_OneWeek <- ggplot(data = MW_2_2015_april_week, aes(x = Date, y = SWL)) +
  labs(title = "MW 2 - 4/15-4/22 (2015)", y = "SWL(m)") + 
  geom_line(colour = "blue") +
  theme_bw()

MW_3_OneWeek <- ggplot(data = MW_3_2015_april_week, aes(x = Date, y = SWL)) +
  labs(title = "MW 3 - 4/15-4/22 (2015)", y = "SWL(m)") + 
  geom_line(colour = "blue") +
  theme_bw()

MW_5_OneWeek <- ggplot(data = MW_5_2015_april_week, aes(x = Date, y = SWL)) +
  labs(title = "MW 5 - 4/15-4/22 (2015)", y = 'SWL(m)') + 
  geom_line(colour = "blue") +
  theme_bw()

MW_7_OneWeek <- ggplot(data = MW_7_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 7 - 4/15-4/22 (2015)", y = "SWL(m)") + 
  geom_line(colour = "blue") +
  theme_bw()

MW_9_OneWeek <- ggplot(data = MW_9_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 9 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_11_OneWeek <- ggplot(data = MW_11_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 11 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_13_OneWeek <- ggplot(data = MW_13_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 13 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_14_OneWeek <- ggplot(data = MW_14_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 14 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_17_OneWeek <- ggplot(data = MW_17_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 17 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_19_OneWeek <- ggplot(data = MW_19_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 19 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_20_OneWeek <- ggplot(data = MW_20_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 20 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_22_OneWeek <- ggplot(data = MW_22_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 22 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_23_OneWeek <- ggplot(data = MW_23_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 23 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_CP1_OneWeek <- ggplot(data = MW_CP1_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW CP1 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

OA_OneWeek <- ggplot(data = OA_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "Oneto Ag - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

## Plot Hydrgraphs------

# create a graphical device to store pdf outputs in working directory
pdf(file = "MW_Hydrographs.pdf", onefile = TRUE, height = 4.25, width = 5.5, paper = "special")
# all files will be saved on PDF until dev.off is commanded (by defualt it is afterall weekly plots but may be moved)
MW_2_Complete
MW_3_Complete
MW_3_Complete
MW_5_Complete
MW_7_Complete
MW_9_Complete
MW_11_Complete
MW_13_Complete
MW_14_Complete
MW_17_Complete
MW_19_Complete
MW_20_Complete
MW_22_Complete
MW_23_Complete
MW_CP1_Complete
OA_Complete

# Yearly----

MW_2_OneYear
MW_3_OneYear
MW_5_OneYear
MW_7_OneYear
MW_9_OneYear
MW_11_OneYear
MW_13_OneYear
MW_14_OneYear
MW_17_OneYear
MW_19_OneYear
MW_20_OneYear
MW_22_OneYear
MW_23_OneYear
MW_CP1_OneYear
OA_OneYear

# Monthly----

MW_2_OneMonth
MW_3_OneMonth
MW_5_OneMonth
MW_7_OneMonth
MW_9_OneMonth
MW_11_OneMonth
MW_13_OneMonth
MW_14OneMonth
MW_17_OneMonth
MW_19_OneMonth
MW_20_OneMonth
MW_22_OneMonth
MW_23_OneMonth
MW_CP1_OneMonth
OA_OneMonth

# Weekly---- 

MW_2_OneWeek
MW_3_OneWeek
MW_5_OneWeek
MW_7_OneWeek
MW_9_OneWeek
MW_11_OneWeek
MW_13_OneWeek
MW_14_OneWeek
MW_17_OneWeek
MW_19_OneWeek
MW_20_OneWeek
MW_22_OneWeek
MW_23_OneWeek
MW_CP1_OneWeek
OA_OneWeek

# turn off saving and storing plots
dev.off()

## Operations Divided By Well------

# MW 2----

MW_2 <- read.csv("~/Continuous Data/MW-2.csv")  
MW_2_Dates <- MW_2$Date.Time..PDT.
MW_2$SWL..m. <- as.numeric(MW_2$SWL..m.)
MW_2_SWL <- interpNA(MW_2$SWL..m., method = "linear")
MW_2_Total <- cbind(MW_2_Dates, MW_2_SWL)
MW_2_Total <- data.frame(MW_2_Total)
MW_2_Dates <- as.POSIXct(MW_2$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_2_Total[,1] <- MW_2_Dates
colnames(MW_2_Total) <- c('Date', 'SWL')
MW_2_Complete <- ggplot(data = MW_2_Total, aes(x = Date, y = SWL)) + geom_point() + labs(title = "MW 2 Complete Time Series") + 
  geom_path(size = 2)+ labs(y = "SWL(m)")



MW_2_2015_Dates <- MW_2_Dates[which(MW_2_Dates == "2014-10-01 05:00:00 PST"): which(MW_2_Dates == "2015-09-30 22:00:00 PST")]
MW_2_2015_Data <- MW_2$SWL..m.[which(MW_2_Dates == "2014-10-01 05:00:00 PST"): which(MW_2_Dates == "2015-09-30 22:00:00 PST")]
MW_2_2015 <- cbind(MW_2_2015_Dates, MW_2_2015_Data)
MW_2_2015 <- data.frame(MW_2_2015)
MW_2_2015[,1] <- as.POSIXct(MW_2_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015) <- c('Date', 'SWL')
MW_2_OneYear <- ggplot(data = MW_2_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + labs(title = "MW 2 - 2015") + 
  geom_line(colour = "springgreen4")
MW_2_OneYear+ labs(y = "SWL(m)")



MW_2_2015_april_dates <- MW_2_Dates[which(MW_2_Dates == "2015-04-01 02:00:00 PST"): which(MW_2_Dates == "2015-04-30 23:00:00 PST")]
MW_2_2015_april_data <- MW_2$SWL..m.[which(MW_2_Dates == "2015-04-01 02:00:00 PST"): which(MW_2_Dates == "2015-04-30 23:00:00 PST")]
MW_2_2015_april <- cbind(MW_2_2015_april_dates, MW_2_2015_april_data)
MW_2_2015_april <- data.frame(MW_2_2015_april)
MW_2_2015_april[,1] <- as.POSIXct(MW_2_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015_april) <- c('Date', 'SWL')
MW_2_OneMonth <- ggplot(data = MW_2_2015_april, aes(x = Date, y = SWL)) + labs(title = "MW 2 - 4/2015") + 
  geom_line(colour = "purple")
MW_2_OneMonth+ labs(y = "SWL(m)")


MW_2_2015_april_week_dates <- MW_2_Dates[which(MW_2_Dates == "2015-04-15 02:00:00 PST"): which(MW_2_Dates == "2015-04-22 23:00:00 PST")]
MW_2_2015_april_week_data <- MW_2$SWL..m.[which(MW_2_Dates == "2015-04-15 02:00:00 PST"): which(MW_2_Dates == "2015-04-22 23:00:00 PST")]
MW_2_2015_april_week <- cbind(MW_2_2015_april_week_dates, MW_2_2015_april_week_data)
MW_2_2015_april_week <- data.frame(MW_2_2015_april_week)
MW_2_2015_april_week[,1] <- as.POSIXct(MW_2_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_2_2015_april_week) <- c('Date', 'SWL')
MW_2_OneWeek <- ggplot(data = MW_2_2015_april_week, aes(x = Date, y = SWL)) + labs(title = "MW 2 - 4/15-4/22 (2015)") + 
  geom_line(colour = "blue")
MW_2_OneWeek+ labs(y = "SWL(m)")



# MW 3------

MW_3 <- read.csv("~/Continuous Data/MW-3.csv")

MW_3_Dates <- MW_3$Date.Time..PDT.
MW_3$SWL..m. <- as.numeric(MW_3$SWL..m.)
MW_3_SWL <- interpNA(MW_3$SWL..m., method = "linear")
MW_3_Total <- cbind(MW_3_Dates, MW_3_SWL)
MW_3_Total <- data.frame(MW_3_Total)
MW_3_Dates <- as.POSIXct(MW_3$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_3_Total[,1] <- MW_3_Dates
colnames(MW_3_Total) <- c('Date', 'SWL')
MW_3_Complete <- ggplot(data = MW_3_Total, aes(x = Date, y = SWL)) + geom_point() + labs(title = "MW 3 Complete Time Series") + 
  geom_path(size = 2)
MW_3_Complete + labs(y = "SWL(m)")


MW_3_2015_Dates <- MW_3_Dates[which(MW_3_Dates == "2014-10-01 05:00:00 PST"): which(MW_3_Dates == "2015-09-30 22:00:00 PST")]
MW_3_2015_Data <- MW_3$SWL..m.[which(MW_3_Dates == "2014-10-01 05:00:00 PST"): which(MW_3_Dates == "2015-09-30 22:00:00 PST")]
MW_3_2015 <- cbind(MW_3_2015_Dates, MW_3_2015_Data)
MW_3_2015 <- data.frame(MW_3_2015)
MW_3_2015[,1] <- as.POSIXct(MW_3_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015) <- c('Date', 'SWL')
MW_3_OneYear <- ggplot(data = MW_3_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + labs(title = "MW 3 - 2015") + 
  geom_line(colour = "springgreen4")
MW_3_OneYear+ labs(y = "SWL(m)")



MW_3_2015_april_dates <- MW_3_Dates[which(MW_3_Dates == "2015-04-01 02:00:00 PST"): which(MW_3_Dates == "2015-04-30 23:00:00 PST")]
MW_3_2015_april_data <- MW_3$SWL..m.[which(MW_3_Dates == "2015-04-01 02:00:00 PST"): which(MW_3_Dates == "2015-04-30 23:00:00 PST")]
MW_3_2015_april <- cbind(MW_3_2015_april_dates, MW_3_2015_april_data)
MW_3_2015_april <- data.frame(MW_3_2015_april)
MW_3_2015_april[,1] <- as.POSIXct(MW_3_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015_april) <- c('Date', 'SWL')
MW_3_OneMonth <- ggplot(data = MW_3_2015_april, aes(x = Date, y = SWL)) + labs(title = "MW 3 - 4/2015") + 
  geom_line(colour = "purple")
MW_3_OneMonth+ labs(y = "SWL(m)")


MW_3_2015_april_week_dates <- MW_3_Dates[which(MW_3_Dates == "2015-04-15 02:00:00 PST"): which(MW_3_Dates == "2015-04-22 23:00:00 PST")]
MW_3_2015_april_week_data <- MW_3$SWL..m.[which(MW_3_Dates == "2015-04-15 02:00:00 PST"): which(MW_3_Dates == "2015-04-22 23:00:00 PST")]
MW_3_2015_april_week <- cbind(MW_3_2015_april_week_dates, MW_3_2015_april_week_data)
MW_3_2015_april_week <- data.frame(MW_3_2015_april_week)
MW_3_2015_april_week[,1] <- as.POSIXct(MW_3_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_3_2015_april_week) <- c('Date', 'SWL')
MW_3_OneWeek <- ggplot(data = MW_3_2015_april_week, aes(x = Date, y = SWL)) + labs(title = "MW 3 - 4/15-4/22 (2015)") + 
  geom_line(colour = "blue")
MW_3_OneWeek+ labs(y = "SWL(m)")



# MW 5 ------

MW_5 <- read.csv("~/Continuous Data/MW-5.csv")

MW_5_Dates <- MW_5$Date.Time..PDT.
MW_5$SWL..m. <- as.numeric(as.character(MW_5$SWL..m.))
MW_5_SWL <- interpNA(MW_5$SWL..m., method = "linear")
MW_5_Total <- cbind(MW_5_Dates, MW_5_SWL)
MW_5_Total <- data.frame(MW_5_Total)
MW_5_Dates <- as.POSIXct(MW_5$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_5_Total[,1] <- MW_5_Dates
colnames(MW_5_Total) <- c('Date', 'SWL')
MW_5_Complete <- ggplot(data = MW_5_Total, aes(x = Date, y = SWL)) + geom_point() + labs(title = "MW 5 Complete Time Series") + 
  geom_path(size = 2)
MW_5_Complete + labs(y = "SWL(m)")


MW_5_2015_Dates <- MW_5_Dates[which(MW_5_Dates == "2014-10-01 05:00:00 PST"): which(MW_5_Dates == "2015-09-30 22:00:00 PST")]
MW_5_2015_Data <- MW_5$SWL..m.[which(MW_5_Dates == "2014-10-01 05:00:00 PST"): which(MW_5_Dates == "2015-09-30 22:00:00 PST")]
MW_5_2015 <- cbind(MW_5_2015_Dates, MW_5_2015_Data)
MW_5_2015 <- data.frame(MW_5_2015)
MW_5_2015[,1] <- as.POSIXct(MW_5_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015) <- c('Date', 'SWL')
MW_5_OneYear <- ggplot(data = MW_5_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + labs(title = "MW 5 - 2015") + 
  geom_line(colour = "springgreen4")
MW_5_OneYear+ labs(y = "SWL(m)")



MW_5_2015_april_dates <- MW_5_Dates[which(MW_5_Dates == "2015-04-01 02:00:00 PST"): which(MW_5_Dates == "2015-04-30 23:00:00 PST")]
MW_5_2015_april_data <- MW_5$SWL..m.[which(MW_5_Dates == "2015-04-01 02:00:00 PST"): which(MW_5_Dates == "2015-04-30 23:00:00 PST")]
MW_5_2015_april <- cbind(MW_5_2015_april_dates, MW_5_2015_april_data)
MW_5_2015_april <- data.frame(MW_5_2015_april)
MW_5_2015_april[,1] <- as.POSIXct(MW_5_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015_april) <- c('Date', 'SWL')
MW_5_OneMonth <- ggplot(data = MW_5_2015_april, aes(x = Date, y = SWL)) + labs(title = "MW 5 - 4/2015") + 
  geom_line(colour = "purple")
MW_5_OneMonth+ labs(y = "SWL(m)")


MW_5_2015_april_week_dates <- MW_5_Dates[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_5_Dates == "2015-04-22 23:00:00 PST")]
MW_5_2015_april_week_data <- MW_5$SWL..m.[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_5_Dates == "2015-04-22 23:00:00 PST")]
MW_5_2015_april_week <- cbind(MW_5_2015_april_week_dates, MW_5_2015_april_week_data)
MW_5_2015_april_week <- data.frame(MW_5_2015_april_week)
MW_5_2015_april_week[,1] <- as.POSIXct(MW_5_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_5_2015_april_week) <- c('Date', 'SWL')
MW_5_OneWeek <- ggplot(data = MW_5_2015_april_week, aes(x = Date, y = SWL)) + labs(title = "MW 5 - 4/15-4/22 (2015)") + 
  geom_line(colour = "blue")
MW_5_OneWeek+ labs(y = "SWL(m)")



# MW 7------

MW_7 <- read.csv("~/Continuous Data/MW-7.csv")

MW_7_Dates <- MW_7$Date.Time..PDT.
MW_7$SWL..m. <- as.numeric(MW_7$SWL..m.)
MW_7_SWL <- interpNA(MW_7$SWL..m., method = "linear")
MW_7_Total <- cbind(MW_7_Dates, MW_7_SWL)
MW_7_Total <- data.frame(MW_7_Total)
MW_7_Dates <- as.POSIXct(MW_7$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_7_Total[,1] <- MW_7_Dates
colnames(MW_7_Total) <- c('Date', 'SWL')
MW_7_Complete <- ggplot(data = MW_7_Total, aes(x = Date, y = SWL)) + geom_point() + labs(title = "MW 7 Complete Time Series") + 
  geom_path(size = 2)
MW_7_Complete + labs(y = "SWL(m)")


MW_7_2015_Dates <- MW_7_Dates[which(MW_7_Dates == "2014-10-01 05:00:00 PST"): which(MW_7_Dates == "2015-09-30 22:00:00 PST")]
MW_7_2015_Data <- MW_7$SWL..m.[which(MW_7_Dates == "2014-10-01 05:00:00 PST"): which(MW_7_Dates == "2015-09-30 22:00:00 PST")]
MW_7_2015 <- cbind(MW_7_2015_Dates, MW_7_2015_Data)
MW_7_2015 <- data.frame(MW_7_2015)
MW_7_2015[,1] <- as.POSIXct(MW_7_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015) <- c('Date', 'SWL')
MW_7_OneYear <- ggplot(data = MW_7_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + labs(title = "MW 7 - 2015") + 
  geom_line(colour = "springgreen4")
MW_7_OneYear+ labs(y = "SWL(m)")



MW_7_2015_april_dates <- MW_7_Dates[which(MW_7_Dates == "2015-04-01 02:00:00 PST"): which(MW_7_Dates == "2015-04-30 23:00:00 PST")]
MW_7_2015_april_data <- MW_7$SWL..m.[which(MW_7_Dates == "2015-04-01 02:00:00 PST"): which(MW_7_Dates == "2015-04-30 23:00:00 PST")]
MW_7_2015_april <- cbind(MW_7_2015_april_dates, MW_7_2015_april_data)
MW_7_2015_april <- data.frame(MW_7_2015_april)
MW_7_2015_april[,1] <- as.POSIXct(MW_7_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015_april) <- c('Date', 'SWL')

MW_7_OneMonth <- ggplot(data = MW_7_2015_april, aes(x = Date, y = SWL)) + 
  labs(title = "MW 7 - 4/2015") + 
  geom_line(colour = "purple") + 
  labs(y = "SWL(m)") + 
  theme_bw()


MW_7_2015_april_week_dates <- MW_7_Dates[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_7_Dates == "2015-04-22 23:00:00 PST")]
MW_7_2015_april_week_data <- MW_7$SWL..m.[which(MW_5_Dates == "2015-04-15 02:00:00 PST"): which(MW_7_Dates == "2015-04-22 23:00:00 PST")]
MW_7_2015_april_week <- cbind(MW_7_2015_april_week_dates, MW_7_2015_april_week_data)
MW_7_2015_april_week <- data.frame(MW_7_2015_april_week)
MW_7_2015_april_week[,1] <- as.POSIXct(MW_7_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_7_2015_april_week) <- c('Date', 'SWL')
MW_7_OneWeek <- ggplot(data = MW_7_2015_april_week, aes(x = Date, y = SWL)) + labs(title = "MW 7 - 4/15-4/22 (2015)") + 
  geom_line(colour = "blue")
MW_7_OneWeek+ labs(y = "SWL(m)")


# MW 9------

MW_9 <- read.csv("~/Continuous Data/MW-9.csv")

MW_9_Dates <- MW_9$Date.Time..PDT.
MW_9$SWL..m. <- as.numeric(MW_9$SWL..m.)
MW_9_SWL <- interpNA(MW_9$SWL..m., method = "linear")
MW_9_Total <- cbind(MW_9_Dates, MW_9_SWL)
MW_9_Total <- data.frame(MW_9_Total)
MW_9_Dates <- as.POSIXct(MW_9$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_9_Total[,1] <- MW_9_Dates
colnames(MW_9_Total) <- c('Date', 'SWL')
MW_9_Complete <- ggplot(data = MW_9_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_9_Complete 


MW_9_2015_Dates <- MW_9_Dates[which(MW_9_Dates == "2014-10-01 05:00:00 PST"): which(MW_9_Dates == "2015-09-30 22:32:00 PST")]
MW_9_2015_Data <- MW_9$SWL..m.[which(MW_9_Dates == "2014-10-01 05:00:00 PST"): which(MW_9_Dates == "2015-09-30 22:32:00 PST")]
MW_9_2015 <- cbind(MW_9_2015_Dates, MW_9_2015_Data)
MW_9_2015 <- data.frame(MW_9_2015)
MW_9_2015[,1] <- as.POSIXct(MW_9_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015) <- c('Date', 'SWL')
MW_9_OneYear <- ggplot(data = MW_9_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 9 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_9_OneYear



MW_9_2015_april_dates <- MW_9_Dates[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_9_Dates == "2015-04-30 23:02:00 PST")]
MW_9_2015_april_data <- MW_9$SWL..m.[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_9_Dates == "2015-04-30 23:02:00 PST")]
MW_9_2015_april <- cbind(MW_9_2015_april_dates, MW_9_2015_april_data)
MW_9_2015_april <- data.frame(MW_9_2015_april)
MW_9_2015_april[,1] <- as.POSIXct(MW_9_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015_april) <- c('Date', 'SWL')
MW_9_OneMonth <- ggplot(data = MW_9_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 9 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_9_OneMonth


MW_9_2015_april_week_dates <- MW_9_Dates[which(MW_9_Dates == "2015-04-15 02:02:00 PST"): which(MW_9_Dates == "2015-04-22 23:02:00 PST")]
MW_9_2015_april_week_data <- MW_9$SWL..m.[which(MW_9_Dates == "2015-04-15 02:02:00 PST"): which(MW_9_Dates == "2015-04-22 23:02:00 PST")]
MW_9_2015_april_week <- cbind(MW_9_2015_april_week_dates, MW_9_2015_april_week_data)
MW_9_2015_april_week <- data.frame(MW_9_2015_april_week)
MW_9_2015_april_week[,1] <- as.POSIXct(MW_9_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_9_2015_april_week) <- c('Date', 'SWL')
MW_9_OneWeek <- ggplot(data = MW_9_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 9 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_9_OneWeek


# MW 11------

MW_11 <- read.csv("~/Continuous Data/MW-9.csv")

MW_11_Dates <- MW_11$Date.Time..PDT.
MW_11$SWL..m. <- as.numeric(MW_11$SWL..m.)
MW_11_SWL <- interpNA(MW_11$SWL..m., method = "linear")
MW_11_Total <- cbind(MW_11_Dates, MW_11_SWL)
MW_11_Total <- data.frame(MW_11_Total)
MW_11_Dates <- as.POSIXct(MW_11$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_11_Total[,1] <- MW_11_Dates
colnames(MW_11_Total) <- c('Date', 'SWL')
MW_11_Complete <- ggplot(data = MW_11_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 11 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_11_Complete 


MW_11_2015_Dates <- MW_11_Dates[which(MW_11_Dates == "2014-10-01 05:00:00 PST"): which(MW_11_Dates == "2015-09-30 23:02:00 PST")]
MW_11_2015_Data <- MW_11$SWL..m.[which(MW_11_Dates == "2014-10-01 05:00:00 PST"): which(MW_11_Dates == "2015-09-30 23:02:00 PST")]
MW_11_2015 <- cbind(MW_11_2015_Dates, MW_11_2015_Data)
MW_11_2015 <- data.frame(MW_11_2015)
MW_11_2015[,1] <- as.POSIXct(MW_11_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015) <- c('Date', 'SWL')
MW_11_OneYear <- ggplot(data = MW_11_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 11 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_11_OneYear



MW_11_2015_april_dates <- MW_11_Dates[which(MW_11_Dates == "2015-04-01 02:02:00 PST"): which(MW_11_Dates == "2015-04-30 23:02:00 PST")]
MW_11_2015_april_data <- MW_11$SWL..m.[which(MW_11_Dates == "2015-04-01 02:02:00 PST"): which(MW_11_Dates == "2015-04-30 23:02:00 PST")]
MW_11_2015_april <- cbind(MW_11_2015_april_dates, MW_11_2015_april_data)
MW_11_2015_april <- data.frame(MW_11_2015_april)
MW_11_2015_april[,1] <- as.POSIXct(MW_11_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015_april) <- c('Date', 'SWL')
MW_11_OneMonth <- ggplot(data = MW_11_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 11 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_11_OneMonth


MW_11_2015_april_week_dates <- MW_11_Dates[which(MW_11_Dates == "2015-04-15 02:02:00 PST"): which(MW_11_Dates == "2015-04-22 23:02:00 PST")]
MW_11_2015_april_week_data <- MW_11$SWL..m.[which(MW_11_Dates == "2015-04-15 02:02:00 PST"): which(MW_11_Dates == "2015-04-22 23:02:00 PST")]
MW_11_2015_april_week <- cbind(MW_11_2015_april_week_dates, MW_11_2015_april_week_data)
MW_11_2015_april_week <- data.frame(MW_11_2015_april_week)
MW_11_2015_april_week[,1] <- as.POSIXct(MW_11_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_11_2015_april_week) <- c('Date', 'SWL')
MW_11_OneWeek <- ggplot(data = MW_11_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 11 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()

MW_11_OneWeek


# MW 13------

MW_13 <- read.csv("~/Continuous Data/MW-13.csv")

MW_13_Dates <- MW_13$Date.Time..PDT.
MW_13$SWL..m. <- as.numeric(MW_13$SWL..m.)
MW_13_SWL <- interpNA(MW_13$SWL..m., method = "linear")
MW_13_Total <- cbind(MW_13_Dates, MW_9_SWL)
MW_13_Total <- data.frame(MW_13_Total)
MW_13_Dates <- as.POSIXct(MW_13$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_13_Total[,1] <- MW_13_Dates
colnames(MW_13_Total) <- c('Date', 'SWL')
MW_13_Complete <- ggplot(data = MW_13_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 13 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_13_Complete 


MW_13_2015_Dates <- MW_13_Dates[which(MW_13_Dates == "2015-10-01 05:02:00 PST"): which(MW_13_Dates == "2016-09-30 23:00:00 PST")]
MW_13_2015_Data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-10-01 05:02:00 PST"): which(MW_13_Dates == "2016-09-30 23:00:00 PST")]
MW_13_2015 <- cbind(MW_13_2015_Dates, MW_13_2015_Data)
MW_13_2015 <- data.frame(MW_13_2015)
MW_13_2015[,1] <- as.POSIXct(MW_13_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015) <- c('Date', 'SWL')
MW_13_OneYear <- ggplot(data = MW_13_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 13 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_13_OneYear



MW_13_2015_april_dates <- MW_13_Dates[which(MW_13_Dates == "2015-04-01 02:02:00 PST"): which(MW_13_Dates == "2015-04-30 23:02:00 PST")]
MW_13_2015_april_data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-04-01 02:02:00 PST"): which(MW_13_Dates == "2015-04-30 23:02:00 PST")]
MW_13_2015_april <- cbind(MW_13_2015_april_dates, MW_13_2015_april_data)
MW_13_2015_april <- data.frame(MW_13_2015_april)
MW_13_2015_april[,1] <- as.POSIXct(MW_13_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015_april) <- c('Date', 'SWL')
MW_13_OneMonth <- ggplot(data = MW_13_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 13 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_13_OneMonth


MW_13_2015_april_week_dates <- MW_13_Dates[which(MW_13_Dates == "2015-04-15 02:02:00 PST"): which(MW_13_Dates == "2015-04-22 23:02:00 PST")]
MW_13_2015_april_week_data <- MW_13$SWL..m.[which(MW_13_Dates == "2015-04-15 02:02:00 PST"): which(MW_13_Dates == "2015-04-22 23:02:00 PST")]
MW_13_2015_april_week <- cbind(MW_13_2015_april_week_dates, MW_13_2015_april_week_data)
MW_13_2015_april_week <- data.frame(MW_13_2015_april_week)
MW_13_2015_april_week[,1] <- as.POSIXct(MW_13_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_13_2015_april_week) <- c('Date', 'SWL')
MW_13_OneWeek <- ggplot(data = MW_13_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 13 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_13_OneWeek



# MW 14------

MW_14 <- read.csv("~/Continuous Data/MW-14.csv")

MW_14_Dates <- MW_14$Date.Time..PDT.
MW_14$SWL..m. <- as.numeric(MW_14$SWL..m.)
MW_14_SWL <- interpNA(MW_14$SWL..m., method = "linear")
MW_14_Total <- cbind(MW_14_Dates, MW_14_SWL)
MW_14_Total <- data.frame(MW_14_Total)
MW_14_Dates <- as.POSIXct(MW_14$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_14_Total[,1] <- MW_14_Dates
colnames(MW_14_Total) <- c('Date', 'SWL')
MW_14_Complete <- ggplot(data = MW_14_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 14 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_14_Complete 


MW_14_2015_Dates <- MW_14_Dates[which(MW_14_Dates == "2014-10-01 05:00:00 PST"): which(MW_14_Dates == "2015-09-30 22:32:00 PST")]
MW_14_2015_Data <- MW_14$SWL..m.[which(MW_14_Dates == "2014-10-01 05:00:00 PST"): which(MW_14_Dates == "2015-09-30 22:32:00 PST")]
MW_14_2015 <- cbind(MW_14_2015_Dates, MW_14_2015_Data)
MW_14_2015 <- data.frame(MW_14_2015)
MW_14_2015[,1] <- as.POSIXct(MW_14_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015) <- c('Date', 'SWL')
MW_14_OneYear <- ggplot(data = MW_14_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 14 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_14_OneYear



MW_14_2015_april_dates <- MW_14_Dates[which(MW_14_Dates == "2015-04-01 02:02:00 PST"): which(MW_14_Dates == "2015-04-30 23:02:00 PST")]
MW_14_2015_april_data <- MW_9$SWL..m.[which(MW_14_Dates == "2015-04-01 02:02:00 PST"): which(MW_14_Dates == "2015-04-30 23:02:00 PST")]
MW_14_2015_april <- cbind(MW_14_2015_april_dates, MW_14_2015_april_data)
MW_14_2015_april <- data.frame(MW_14_2015_april)
MW_14_2015_april[,1] <- as.POSIXct(MW_14_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015_april) <- c('Date', 'SWL')
MW_14_OneMonth <- ggplot(data = MW_14_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 14 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_14_OneMonth


MW_14_2015_april_week_dates <- MW_14_Dates[which(MW_14_Dates == "2015-04-15 02:02:00 PST"): which(MW_14_Dates == "2015-04-22 23:02:00 PST")]
MW_14_2015_april_week_data <- MW_14$SWL..m.[which(MW_14_Dates == "2015-04-15 02:02:00 PST"): which(MW_14_Dates == "2015-04-22 23:02:00 PST")]
MW_14_2015_april_week <- cbind(MW_14_2015_april_week_dates, MW_14_2015_april_week_data)
MW_14_2015_april_week <- data.frame(MW_14_2015_april_week)
MW_14_2015_april_week[,1] <- as.POSIXct(MW_14_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_14_2015_april_week) <- c('Date', 'SWL')
MW_14_OneWeek <- ggplot(data = MW_14_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 14 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_14_OneWeek



# MW 17------

MW_17 <- read.csv("~/Continuous Data/MW-17.csv")

MW_17_Dates <- MW_17$Date.Time..PDT.
MW_17$SWL..m. <- as.numeric(MW_17$SWL..m.)
MW_17_SWL <- interpNA(MW_17$SWL..m., method = "linear")
MW_17_Total <- cbind(MW_17_Dates, MW_17_SWL)
MW_17_Total <- data.frame(MW_17_Total)
MW_17_Dates <- as.POSIXct(MW_17$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_17_Total[,1] <- MW_17_Dates
colnames(MW_17_Total) <- c('Date', 'SWL')
MW_17_Complete <- ggplot(data = MW_17_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_17_Complete 


MW_17_2015_Dates <- MW_17_Dates[which(MW_17_Dates == "2014-10-01 05:00:00 PST"): which(MW_17_Dates == "2015-09-30 22:32:00 PST")]
MW_17_2015_Data <- MW_17$SWL..m.[which(MW_17_Dates == "2014-10-01 05:00:00 PST"): which(MW_17_Dates == "2015-09-30 22:32:00 PST")]
MW_17_2015 <- cbind(MW_17_2015_Dates, MW_17_2015_Data)
MW_17_2015 <- data.frame(MW_17_2015)
MW_17_2015[,1] <- as.POSIXct(MW_17_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015) <- c('Date', 'SWL')
MW_17_OneYear <- ggplot(data = MW_17_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 17 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_17_OneYear



MW_17_2015_april_dates <- MW_17_Dates[which(MW_17_Dates == "2015-04-01 02:02:00 PST"): which(MW_17_Dates == "2015-04-30 23:02:00 PST")]
MW_17_2015_april_data <- MW_17$SWL..m.[which(MW_17_Dates == "2015-04-01 02:02:00 PST"): which(MW_17_Dates == "2015-04-30 23:02:00 PST")]
MW_17_2015_april <- cbind(MW_17_2015_april_dates, MW_17_2015_april_data)
MW_17_2015_april <- data.frame(MW_17_2015_april)
MW_17_2015_april[,1] <- as.POSIXct(MW_17_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015_april) <- c('Date', 'SWL')
MW_17_OneMonth <- ggplot(data = MW_17_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 17 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_17_OneMonth


MW_17_2015_april_week_dates <- MW_17_Dates[which(MW_17_Dates == "2015-04-15 02:02:00 PST"): which(MW_17_Dates == "2015-04-22 23:02:00 PST")]
MW_17_2015_april_week_data <- MW_17$SWL..m.[which(MW_17_Dates == "2015-04-15 02:02:00 PST"): which(MW_17_Dates == "2015-04-22 23:02:00 PST")]
MW_17_2015_april_week <- cbind(MW_17_2015_april_week_dates, MW_17_2015_april_week_data)
MW_17_2015_april_week <- data.frame(MW_17_2015_april_week)
MW_17_2015_april_week[,1] <- as.POSIXct(MW_17_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_17_2015_april_week) <- c('Date', 'SWL')
MW_17_OneWeek <- ggplot(data = MW_17_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 17 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_17_OneWeek



# MW 19------

MW_19 <- read.csv("~/Continuous Data/MW-19.csv")

MW_19_Dates <- MW_19$Date.Time..PDT.
MW_19$SWL..m. <- as.numeric(MW_19$SWL..m.)
MW_19_SWL <- interpNA(MW_19$SWL..m., method = "linear")
MW_19_Total <- cbind(MW_19_Dates, MW_19_SWL)
MW_19_Total <- data.frame(MW_19_Total)
MW_19_Dates <- as.POSIXct(MW_19$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_19_Total[,1] <- MW_19_Dates
colnames(MW_19_Total) <- c('Date', 'SWL')
MW_19_Complete <- ggplot(data = MW_19_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_19_Complete 


MW_19_2015_Dates <- MW_19_Dates[which(MW_19_Dates == "2014-10-01 05:00:00 PST"): which(MW_19_Dates == "2015-09-30 22:32:00 PST")]
MW_19_2015_Data <- MW_19$SWL..m.[which(MW_19_Dates == "2014-10-01 05:00:00 PST"): which(MW_19_Dates == "2015-09-30 22:32:00 PST")]
MW_19_2015 <- cbind(MW_19_2015_Dates, MW_19_2015_Data)
MW_19_2015 <- data.frame(MW_19_2015)
MW_19_2015[,1] <- as.POSIXct(MW_19_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015) <- c('Date', 'SWL')
MW_19_OneYear <- ggplot(data = MW_19_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 19 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_19_OneYear



MW_19_2015_april_dates <- MW_19_Dates[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_19_Dates == "2015-04-30 23:02:00 PST")]
MW_19_2015_april_data <- MW_19$SWL..m.[which(MW_9_Dates == "2015-04-01 02:02:00 PST"): which(MW_19_Dates == "2015-04-30 23:02:00 PST")]
MW_19_2015_april <- cbind(MW_19_2015_april_dates, MW_19_2015_april_data)
MW_19_2015_april <- data.frame(MW_19_2015_april)
MW_19_2015_april[,1] <- as.POSIXct(MW_19_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015_april) <- c('Date', 'SWL')
MW_19_OneMonth <- ggplot(data = MW_19_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 19 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_19_OneMonth


MW_19_2015_april_week_dates <- MW_19_Dates[which(MW_19_Dates == "2015-04-15 02:02:00 PST"): which(MW_19_Dates == "2015-04-22 23:02:00 PST")]
MW_19_2015_april_week_data <- MW_19$SWL..m.[which(MW_19_Dates == "2015-04-15 02:02:00 PST"): which(MW_19_Dates == "2015-04-22 23:02:00 PST")]
MW_19_2015_april_week <- cbind(MW_19_2015_april_week_dates, MW_19_2015_april_week_data)
MW_19_2015_april_week <- data.frame(MW_19_2015_april_week)
MW_19_2015_april_week[,1] <- as.POSIXct(MW_19_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_19_2015_april_week) <- c('Date', 'SWL')
MW_19_OneWeek <- ggplot(data = MW_19_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 19 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_19_OneWeek



# MW 20------

MW_20 <- read.csv("~/Continuous Data/MW-20.csv")

MW_20_Dates <- MW_20$Date.Time..PDT.
MW_20$SWL..m. <- as.numeric(MW_20$SWL..m.)
MW_20_SWL <- interpNA(MW_20$SWL..m., method = "linear")
MW_20_Total <- cbind(MW_20_Dates, MW_20_SWL)
MW_20_Total <- data.frame(MW_20_Total)
MW_20_Dates <- as.POSIXct(MW_20$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_20_Total[,1] <- MW_20_Dates
colnames(MW_20_Total) <- c('Date', 'SWL')
MW_20_Complete <- ggplot(data = MW_20_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 20 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_20_Complete 


MW_20_2015_Dates <- MW_20_Dates[which(MW_20_Dates == "2014-10-01 05:00:00 PST"): which(MW_20_Dates == "2015-09-30 22:32:00 PST")]
MW_20_2015_Data <- MW_20$SWL..m.[which(MW_20_Dates == "2014-10-01 05:00:00 PST"): which(MW_20_Dates == "2015-09-30 22:32:00 PST")]
MW_20_2015 <- cbind(MW_20_2015_Dates, MW_20_2015_Data)
MW_20_2015 <- data.frame(MW_20_2015)
MW_20_2015[,1] <- as.POSIXct(MW_20_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015) <- c('Date', 'SWL')
MW_20_OneYear <- ggplot(data = MW_20_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 20 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_20_OneYear



MW_20_2015_april_dates <- MW_20_Dates[which(MW_20_Dates == "2015-04-01 02:02:00 PST"): which(MW_20_Dates == "2015-04-30 23:02:00 PST")]
MW_20_2015_april_data <- MW_20$SWL..m.[which(MW_20_Dates == "2015-04-01 02:02:00 PST"): which(MW_20_Dates == "2015-04-30 23:02:00 PST")]
MW_20_2015_april <- cbind(MW_20_2015_april_dates, MW_20_2015_april_data)
MW_20_2015_april <- data.frame(MW_20_2015_april)
MW_20_2015_april[,1] <- as.POSIXct(MW_20_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015_april) <- c('Date', 'SWL')
MW_20_OneMonth <- ggplot(data = MW_20_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 20 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_20_OneMonth


MW_20_2015_april_week_dates <- MW_20_Dates[which(MW_20_Dates == "2015-04-15 02:02:00 PST"): which(MW_20_Dates == "2015-04-22 23:02:00 PST")]
MW_20_2015_april_week_data <- MW_20$SWL..m.[which(MW_20_Dates == "2015-04-15 02:02:00 PST"): which(MW_20_Dates == "2015-04-22 23:02:00 PST")]
MW_20_2015_april_week <- cbind(MW_20_2015_april_week_dates, MW_20_2015_april_week_data)
MW_20_2015_april_week <- data.frame(MW_20_2015_april_week)
MW_20_2015_april_week[,1] <- as.POSIXct(MW_20_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_20_2015_april_week) <- c('Date', 'SWL')
MW_20_OneWeek <- ggplot(data = MW_20_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 20 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_20_OneWeek



# MW 22------

MW_22 <- read.csv("~/Continuous Data/MW-22.csv")

MW_22_Dates <- MW_22$Date.Time..PDT.
MW_22$SWL..m. <- as.numeric(MW_22$SWL..m.)
MW_22_SWL <- interpNA(MW_22$SWL..m., method = "linear")
MW_22_Total <- cbind(MW_22_Dates, MW_22_SWL)
MW_22_Total <- data.frame(MW_22_Total)
MW_22_Dates <- as.POSIXct(MW_22$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_22_Total[,1] <- MW_22_Dates
colnames(MW_22_Total) <- c('Date', 'SWL')
MW_22_Complete <- ggplot(data = MW_22_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 22 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_22_Complete 


MW_22_2015_Dates <- MW_22_Dates[which(MW_22_Dates == "2014-10-01 05:00:00 PST"): which(MW_22_Dates == "2015-09-30 22:32:00 PST")]
MW_22_2015_Data <- MW_22$SWL..m.[which(MW_22_Dates == "2014-10-01 05:00:00 PST"): which(MW_22_Dates == "2015-09-30 22:32:00 PST")]
MW_22_2015 <- cbind(MW_22_2015_Dates, MW_22_2015_Data)
MW_22_2015 <- data.frame(MW_22_2015)
MW_22_2015[,1] <- as.POSIXct(MW_22_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015) <- c('Date', 'SWL')
MW_22_OneYear <- ggplot(data = MW_22_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 22 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_22_OneYear



MW_22_2015_april_dates <- MW_22_Dates[which(MW_22_Dates == "2015-04-01 02:02:00 PST"): which(MW_22_Dates == "2015-04-30 23:02:00 PST")]
MW_22_2015_april_data <- MW_22$SWL..m.[which(MW_22_Dates == "2015-04-01 02:02:00 PST"): which(MW_22_Dates == "2015-04-30 23:02:00 PST")]
MW_22_2015_april <- cbind(MW_22_2015_april_dates, MW_22_2015_april_data)
MW_22_2015_april <- data.frame(MW_22_2015_april)
MW_22_2015_april[,1] <- as.POSIXct(MW_22_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015_april) <- c('Date', 'SWL')
MW_22_OneMonth <- ggplot(data = MW_22_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 22 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_22_OneMonth


MW_22_2015_april_week_dates <- MW_22_Dates[which(MW_22_Dates == "2015-04-15 02:02:00 PST"): which(MW_22_Dates == "2015-04-22 23:02:00 PST")]
MW_22_2015_april_week_data <- MW_22$SWL..m.[which(MW_22_Dates == "2015-04-15 02:02:00 PST"): which(MW_22_Dates == "2015-04-22 23:02:00 PST")]
MW_22_2015_april_week <- cbind(MW_22_2015_april_week_dates, MW_22_2015_april_week_data)
MW_22_2015_april_week <- data.frame(MW_22_2015_april_week)
MW_22_2015_april_week[,1] <- as.POSIXct(MW_22_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_22_2015_april_week) <- c('Date', 'SWL')
MW_22_OneWeek <- ggplot(data = MW_22_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 22 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_22_OneWeek



# MW 23------

MW_23 <- read.csv("~/Continuous Data/MW-23.csv")

MW_23_Dates <- MW_23$Date.Time..PDT.
MW_23$SWL..m. <- as.numeric(MW_23$SWL..m.)
MW_23_SWL <- interpNA(MW_23$SWL..m., method = "linear")
MW_23_Total <- cbind(MW_23_Dates, MW_23_SWL)
MW_23_Total <- data.frame(MW_23_Total)
MW_23_Dates <- as.POSIXct(MW_23$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_23_Total[,1] <- MW_23_Dates
colnames(MW_23_Total) <- c('Date', 'SWL')
MW_23_Complete <- ggplot(data = MW_23_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW 23 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_23_Complete 


MW_23_2015_Dates <- MW_23_Dates[which(MW_23_Dates == "2014-10-01 05:00:00 PST"): which(MW_23_Dates == "2015-09-30 22:32:00 PST")]
MW_23_2015_Data <- MW_23$SWL..m.[which(MW_23_Dates == "2014-10-01 05:00:00 PST"): which(MW_23_Dates == "2015-09-30 22:32:00 PST")]
MW_23_2015 <- cbind(MW_23_2015_Dates, MW_23_2015_Data)
MW_23_2015 <- data.frame(MW_23_2015)
MW_23_2015[,1] <- as.POSIXct(MW_23_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015) <- c('Date', 'SWL')
MW_23_OneYear <- ggplot(data = MW_23_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW 23 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_23_OneYear



MW_23_2015_april_dates <- MW_23_Dates[which(MW_23_Dates == "2015-04-01 02:02:00 PST"): which(MW_23_Dates == "2015-04-30 23:02:00 PST")]
MW_23_2015_april_data <- MW_23$SWL..m.[which(MW_23_Dates == "2015-04-01 02:02:00 PST"): which(MW_23_Dates == "2015-04-30 23:02:00 PST")]
MW_23_2015_april <- cbind(MW_23_2015_april_dates, MW_23_2015_april_data)
MW_23_2015_april <- data.frame(MW_23_2015_april)
MW_23_2015_april[,1] <- as.POSIXct(MW_23_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015_april) <- c('Date', 'SWL')
MW_23_OneMonth <- ggplot(data = MW_23_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW 23 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_23_OneMonth


MW_23_2015_april_week_dates <- MW_23_Dates[which(MW_23_Dates == "2015-04-15 02:02:00 PST"): which(MW_23_Dates == "2015-04-22 23:02:00 PST")]
MW_23_2015_april_week_data <- MW_23$SWL..m.[which(MW_23_Dates == "2015-04-15 02:02:00 PST"): which(MW_23_Dates == "2015-04-22 23:02:00 PST")]
MW_23_2015_april_week <- cbind(MW_23_2015_april_week_dates, MW_23_2015_april_week_data)
MW_23_2015_april_week <- data.frame(MW_23_2015_april_week)
MW_23_2015_april_week[,1] <- as.POSIXct(MW_23_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_23_2015_april_week) <- c('Date', 'SWL')
MW_23_OneWeek <- ggplot(data = MW_23_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW 23 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_23_OneWeek





# MW CP1------

MW_CP1 <- read.csv("~/Continuous Data/MW-CP1.csv")

MW_CP1_Dates <- MW_CP1$Date.Time..PDT.
MW_CP1$SWL..m. <- as.numeric(MW_CP1$SWL..m.)
MW_CP1_SWL <- interpNA(MW_CP1$SWL..m., method = "linear")
MW_CP1_Total <- cbind(MW_CP1_Dates, MW_CP1_SWL)
MW_CP1_Total <- data.frame(MW_CP1_Total)
MW_CP1_Dates <- as.POSIXct(MW_CP1$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_CP1_Total[,1] <- MW_CP1_Dates
colnames(MW_CP1_Total) <- c('Date', 'SWL')
MW_CP1_Complete <- ggplot(data = MW_CP1_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "MW CP1 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
MW_CP1_Complete 


MW_CP1_2015_Dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2014-10-01 05:00:00 PST"): which(MW_CP1_Dates == "2015-09-30 22:32:00 PST")]
MW_CP1_2015_Data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2014-10-01 05:00:00 PST"): which(MW_CP1_Dates == "2015-09-30 22:32:00 PST")]
MW_CP1_2015 <- cbind(MW_CP1_2015_Dates, MW_CP1_2015_Data)
MW_CP1_2015 <- data.frame(MW_CP1_2015)
MW_CP1_2015[,1] <- as.POSIXct(MW_CP1_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015) <- c('Date', 'SWL')
MW_CP1_OneYear <- ggplot(data = MW_CP1_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "MW CP1 - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
MW_CP1_OneYear



MW_CP1_2015_april_dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP12015_april_data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2015-04-01 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-30 23:02:00 PST")]
MW_CP1_2015_april <- cbind(CP1_2015_april_dates, CP1_2015_april_data)
MW_CP1_2015_april <- data.frame(MW_CP1_2015_april)
MW_CP1_2015_april[,1] <- as.POSIXct(MW_CP1_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015_april) <- c('Date', 'SWL')
MW_CP1_OneMonth <- ggplot(data = MW_CP1_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "MW CP1 - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
MW_CP1_OneMonth


MW_CP1_2015_april_week_dates <- MW_CP1_Dates[which(MW_CP1_Dates == "2015-04-15 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-22 23:02:00 PST")]
MW_CP1_2015_april_week_data <- MW_CP1$SWL..m.[which(MW_CP1_Dates == "2015-04-15 02:02:00 PST"): which(MW_CP1_Dates == "2015-04-22 23:02:00 PST")]
MW_CP1_2015_april_week <- cbind(MW_CP1_2015_april_week_dates, MW_CP1_2015_april_week_data)
MW_CP1_2015_april_week <- data.frame(MW_CP1_2015_april_week)
MW_CP1_2015_april_week[,1] <- as.POSIXct(MW_CP1_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(MW_CP1_2015_april_week) <- c('Date', 'SWL')
MW_CP1_OneWeek <- ggplot(data = MW_CP1_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "MW CP1 - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
MW_CP1_OneWeek





# Oneto Ag------

OA <- read.csv("~/Continuous Data/Oneto_Ag.csv")

OA_Dates <- OA$Date.Time..PDT.
OA$SWL..m. <- as.numeric(OA$SWL..m.)
OA_SWL <- interpNA(OA$SWL..m., method = "linear")
OA_Total <- cbind(OA_Dates, OA_SWL)
OA_Total <- data.frame(OA_Total)
OA_Dates <- as.POSIXct(OA$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
OA_Total[,1] <- OA_Dates
colnames(OA_Total) <- c('Date', 'SWL')
OA_Complete <- ggplot(data = OA_Total, aes(x = Date, y = SWL)) +
  geom_point() + 
  labs(title = "Oneto Ag Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 2) +
  theme_bw()
OA_Complete 

OA_2015_Dates <- OA_Dates[which(OA_Dates == "2014-10-01 05:00:00 PST"): which(OA_Dates == "2015-09-30 22:32:00 PST")]
OA_2015_Data <- OA$SWL..m.[which(OA_Dates == "2014-10-01 05:00:00 PST"): which(OA_Dates == "2015-09-30 22:32:00 PST")]
OA_2015 <- cbind(OA_2015_Dates, OA_2015_Data)
OA_2015 <- data.frame(OA_2015)
OA_2015[,1] <- as.POSIXct(OA_2015_Dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015) <- c('Date', 'SWL')
OA_OneYear <- ggplot(data = OA_2015, aes(x = Date, y = SWL)) + geom_point(colour = "springgreen4") + 
  labs(title = "Oneto Ag - 2015", y = "SWL(m)") + 
  geom_line(colour = "springgreen4") + 
  theme_bw()
OA_OneYear



OA_2015_april_dates <- OA_Dates[which(OA_Dates == "2015-04-01 02:02:00 PST"): which(OA_Dates == "2015-04-30 23:02:00 PST")]
OA_2015_april_data <- OA$SWL..m.[which(OA_Dates == "2015-04-01 02:02:00 PST"): which(OA_Dates == "2015-04-30 23:02:00 PST")]
OA_2015_april <- cbind(OA_2015_april_dates, OA_2015_april_data)
OA_2015_april <- data.frame(OA_2015_april)
OA_2015_april[,1] <- as.POSIXct(OA_2015_april_dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015_april) <- c('Date', 'SWL')
OA_OneMonth <- ggplot(data = OA_2015_april, aes(x = Date, y = SWL)) +
  labs(title = "Oneto Ag - 4/2015", y = "SWL(m)") + 
  geom_line(colour = "purple") + 
  theme_bw()
OA_OneMonth


OA_2015_april_week_dates <- OA_Dates[which(OA_Dates == "2015-04-15 02:02:00 PST"): which(OA_Dates == "2015-04-22 23:02:00 PST")]
OA_2015_april_week_data <- OA$SWL..m.[which(OA_Dates == "2015-04-15 02:02:00 PST"): which(OA_Dates == "2015-04-22 23:02:00 PST")]
OA_2015_april_week <- cbind(OA_2015_april_week_dates, OA_2015_april_week_data)
OA_2015_april_week <- data.frame(OA_2015_april_week)
OA_2015_april_week[,1] <- as.POSIXct(OA_2015_april_week_dates, format = '%m/%d/%Y %H:%M')
colnames(OA_2015_april_week) <- c('Date', 'SWL')
OA_OneWeek <- ggplot(data = OA_2015_april_week, aes(x = Date, y = SWL)) + 
  labs(title = "Oneto Ag - 4/15-4/22 (2015)",y = "SWL(m)") + 
  geom_line(colour = "blue")+ 
  theme_bw()
OA_OneWeek





