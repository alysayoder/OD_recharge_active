## Load All Well Data ------
# Load packages 
library(rmarkdown)
library(ggplot2)
library(timeSeries)
library(ggthemes)
library(scales)
library(readr)
library(dplyr)
library(ggmap)
library(data.table)

# Updates: 

MW_2 <- data.frame(read_csv("MW-2_10.5.17.csv"))
MW_3 <- data.frame(read_csv("MW-3_10.23.17update.csv"))
MW_5 <- read_csv("MW-5_9.15.17.csv")
MW_7 <- read_csv("MW-7_11.3.17.csv")
MW_9 <- data.frame(read_csv("MW-9_10.5.17.csv"))
MW_11 <- data.frame(read_csv("MW-11_9.15.17.csv"))
MW_13 <- data.frame(read_csv("MW-13_11.3.17.csv"))
MW_14 <- data.frame(read_csv("MW-14_11.2.17.csv"))
MW_17 <- data.frame(read_csv("MW-17_8.25.17.csv"))
MW_19 <- data.frame(read_csv("MW-19_11.3.17.csv"))
MW_20 <- data.frame(read_csv("MW-20_11.3.17.csv"))
MW_22 <- data.frame(read_csv("MW-22_8.25.17.csv"))
MW_23 <- data.frame(read_csv("MW-23_9.15.17.csv"))
#O_A <- data.frame(read_csv("Oneto_Ag_9.15.17.csv"))

##  Amy Data Upload----



setwd("C:/Users/ayoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data")

MW_2 <- data.frame(read_csv("C:/Users/ayoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-2_10.5.17.csv"))
MW_3 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-3_10.23.17update.csv"))
MW_5 <- read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-5_9.15.17.csv")
MW_7 <- read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-7_11.3.17.csv")
MW_9 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-9_10.5.17.csv"))
MW_11 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-11_9.15.17.csv"))
MW_13 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-13_11.3.17.csv"))
MW_14 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-14_11.2.17.csv"))
MW_17 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-17_8.25.17.csv"))
MW_19 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-19_11.3.17.csv"))
MW_20 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-20_11.3.17.csv"))
MW_22 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-22_8.25.17.csv"))
MW_23 <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/MW-23_9.15.17.csv"))
O_A <- data.frame(read_csv("C:/Users/amyoder/Box Sync/CosumnesGO_SharedFiles/Continuous Data/Oneto_Ag_9.15.17.csv"))


# Formatting Functions ----
  
DeleteIndexColumn <- function(DataSet)  { 
  if (is.integer(DataSet[1,1])) {
    DataSet[,1] <- NULL
    return(DataSet)
  }
  else {
    return(DataSet)
  }
}



Dates_to_Dates <- function(DataSet)  {
  
  dates <- as.POSIXct(DataSet[,1], format = '%m/%d/%Y %H:%M')
  DataSet[,1] <- dates
  return(DataSet)
  
}


convertDates = function(df){
  
  df[,1] = 
    round(
      as.double(
        as.POSIXct(
          df[,1],
          format = "%m/%d/%Y %H:%M", origin = as.POSIXct("1970-01-01"))) / (15*60)) * (15*60)
  
  df[,1] = as.POSIXct(df[,1], format = '%m/%d/%Y %H:%M', origin = as.POSIXct('1969-12-31 16:00'))
  return(df)
}


ALLWELLS <- list(MW_2, MW_3, MW_5, MW_7, MW_9, MW_11, MW_13, MW_14, MW_17, MW_19, MW_20, MW_22, MW_23)
ALLWELLS <- lapply(ALLWELLS, DeleteIndexColumn)
ALLWELLS <- lapply(ALLWELLS, data.frame)
# ALLWELLS <- lapply(ALLWELLS, Dates_to_Dates)
ALLWELLS = lapply(well_list, convertDates)


MW_2 <- ALLWELLS[[1]]
MW_3 <- ALLWELLS[[2]]
MW_5 <- ALLWELLS[[3]]
MW_7 <- ALLWELLS[[4]]
MW_9 <- ALLWELLS[[5]]
MW_11 <- ALLWELLS[[6]]
MW_13 <- ALLWELLS[[7]]
MW_14 <- ALLWELLS[[8]]
MW_17 <- ALLWELLS[[9]]
MW_19 <- ALLWELLS[[10]]
MW_20 <- ALLWELLS[[11]]
MW_22 <- ALLWELLS[[12]]
MW_23 <- ALLWELLS[[13]]
#O_A <- ALLWELLS[[14]]



## Create Datasets from original Well Data----



MW_2_Dates <- MW_2[,1]
MW_2_SWL <- as.numeric(MW_2$SWL..m.)
MW_2_SWL <- interpNA(MW_2_SWL, method = "linear")
MW_2_Total <- cbind(MW_2_Dates, MW_2_SWL)
MW_2_Total <- data.frame(MW_2_Total)
colnames(MW_2_Total) <- c('Date', 'SWL.2')
Dates <- as.POSIXct(MW_2$Date.Time, format = '%m/%d/%Y %H:%M')
MW_2_Total[,1] <- Dates
MW_2_Total[,2] <- as.numeric(MW_2[,3])



MW_3_Dates <- MW_3[,1]
MW_3_SWL <- as.numeric(MW_3$SWL..m.)
MW_3_Total <- cbind(MW_3_Dates, MW_3_SWL)
colnames(MW_3_Total) <- c('Date', 'SWL.3')
MW_3_Total <- data.frame(MW_3_Total)
Dates <- as.POSIXct(MW_3$Date.Time, format = '%m/%d/%Y %H:%M')
MW_3_Total[,1] <- Dates
MW_3_Total[,2] <- as.numeric(MW_3$SWL..m.)



MW_5_Dates <- MW_5$Date.Time
MW_5_SWL <- as.numeric(as.character(MW_5$SWL..m.))
MW_5_Total <- cbind(MW_5_Dates, MW_5_SWL)
colnames(MW_5_Total) <- c('Date', 'SWL.5')
MW_5_Total <- data.frame(MW_5_Total)
Dates <- as.POSIXct(MW_5$Date.Time, format = '%m/%d/%Y %H:%M')
MW_5_Total[,1] <- Dates
MW_5_Total[,2] <- as.numeric(MW_5$SWL..m.)



MW_7_Dates <- MW_7$Date.Time
MW_7_SWL <- as.numeric(MW_7$SWL..m.)
MW_7_Total <- cbind(MW_7_Dates, MW_7_SWL)
colnames(MW_7_Total) <- c('Date', 'SWL.7')
MW_7_Total <- data.frame(MW_7_Total)
Dates <- as.POSIXct(MW_7$Date.Time, format = '%m/%d/%Y %H:%M')
MW_7_Total[,1] <- Dates
MW_7_Total[,2] <- as.numeric(MW_7$SWL..m.)



MW_9_Dates <- MW_9[,1]
MW_9_SWL <- as.numeric(MW_9$SWL..m.)
MW_9_Total <- cbind(MW_9_Dates, MW_9_SWL)
colnames(MW_9_Total) <- c('Date', 'SWL.9')
MW_9_Total <- data.frame(MW_9_Total)
Dates <- as.POSIXct(MW_9$Date.Time, format = '%m/%d/%Y %H:%M')
MW_9_Total[,1] <- Dates
MW_9_Total[,2] <- as.numeric(MW_9$SWL..m.)



MW_11_Dates <- MW_11[,1]
MW_11_SWL <- as.numeric(MW_11$SWL..m.)
MW_11_Total <- cbind(MW_11_Dates, MW_11_SWL)
colnames(MW_11_Total) <- c('Date', 'SWL.11')
MW_11_Total <- data.frame(MW_11_Total)
Dates <- as.POSIXct(MW_11$Date.Time, format = '%m/%d/%Y %H:%M')
MW_11_Total[,1] <- Dates
MW_11_Total[,2] <- as.numeric(MW_11$SWL..m.)



MW_13_Dates <- MW_13[,1]
MW_13_SWL <- as.numeric(MW_13$SWL..m.)
MW_13_Total <- cbind(MW_13_Dates, MW_13_SWL)
colnames(MW_13_Total) <- c('Date', 'SWL.13')
MW_13_Total <- data.frame(MW_13_Total)
Dates <- as.POSIXct(MW_13$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
MW_13_Total[,1] <- Dates
MW_13_Total[,2] <- as.numeric(MW_13$SWL..m.)



MW_14_Dates <- MW_14[,1]
MW_14_SWL <- as.numeric(MW_14$SWL..m.)
MW_14_Total <- cbind(MW_14_Dates, MW_14_SWL)
colnames(MW_14_Total) <- c('Date', 'SWL.14')
MW_14_Total <- data.frame(MW_14_Total)
Dates <- as.POSIXct(MW_14$Date.Time, format = '%m/%d/%Y %H:%M')
MW_14_Total[,1] <- Dates
MW_14_Total[,2] <- as.numeric(MW_14$SWL..m.)



MW_17_Dates <- MW_17[,1]
MW_17_SWL <- as.numeric(MW_17$SWL..m.)
MW_17_Total <- cbind(MW_17_Dates, MW_17_SWL)
colnames(MW_17_Total) <- c('Date', 'SWL.17')
MW_17_Total <- data.frame(MW_17_Total)
Dates <- as.POSIXct(MW_17$Date.Time, format = '%m/%d/%Y %H:%M')
MW_17_Total[,1] <- Dates
MW_17_Total[,2] <- as.numeric(MW_17$SWL..m.)



MW_19_Dates <- MW_19[,1]
MW_19_SWL <- as.numeric(MW_19$SWL..m.)
MW_19_Total <- cbind(MW_19_Dates, MW_19_SWL)
colnames(MW_19_Total) <- c('Date', 'SWL.19')
MW_19_Total <- data.frame(MW_19_Total)
Dates <- as.POSIXct(MW_19$Date.Time, format = '%m/%d/%Y %H:%M')
MW_19_Total[,1] <- Dates
MW_19_Total[,2] <- as.numeric(MW_19$SWL..m.)



MW_20_Dates <- MW_20[,1]
MW_20_SWL <- as.numeric(MW_20$SWL..m.)
MW_20_Total <- cbind(MW_20_Dates, MW_20_SWL)
colnames(MW_20_Total) <- c('Date', 'SWL.20')
MW_20_Total <- data.frame(MW_20_Total)
Dates <- as.POSIXct(MW_20$Date.Time, format = '%m/%d/%Y %H:%M')
MW_20_Total[,1] <- Dates
MW_20_Total[,2] <- as.numeric(MW_20_Total[,2])



MW_22_Dates <- MW_22[,1]
MW_22_SWL <- as.numeric(MW_22$SWL..m.)
MW_22_Total <- cbind(MW_22_Dates, MW_22_SWL)
colnames(MW_22_Total) <- c('Date', 'SWL.22')
MW_22_Total <- data.frame(MW_22_Total)
Dates <- as.POSIXct(MW_22$Date.Time, format = '%m/%d/%Y %H:%M')
MW_22_Total[,1] <- Dates
MW_22_Total[,2] <- as.numeric(MW_22$SWL..m.)



MW_23_Dates <- MW_23[,1]
MW_23_SWL <- as.numeric(MW_23$SWL..m.)
MW_23_Total <- cbind(MW_23_Dates, MW_23_SWL)
colnames(MW_23_Total) <- c('Date', 'SWL.23')
MW_23_Total <- data.frame(MW_23_Total)
Dates <- as.POSIXct(MW_23$Date.Time, format = '%m/%d/%Y %H:%M')
MW_23_Total[,1] <- Dates
MW_23_Total[,2] <- as.numeric(MW_23$SWL..m.)



#MW_CP1_Dates <- MW_CP1$Date.Time..PDT.
#MW_CP1$SWL..m. <- as.numeric(MW_CP1$SWL..m.)
#MW_CP1_SWL <- interpNA(MW_CP1$SWL..m., method = "linear")
#MW_CP1_Total <- cbind(MW_CP1_Dates, MW_CP1_SWL)
#MW_CP1_Total <- data.frame(MW_CP1_Total)
#MW_CP1_Dates <- as.POSIXct(MW_CP1[,1], format = '%m/%d/%Y %H:%M', origin = as.POSIXct('1969-12-31 16:00'))
#MW_CP1_Total[,1] <- MW_CP1_Dates
#colnames(MW_CP1_Total) <- c('Date', 'SWL.CP1')


#OA_Dates <- O_A[,1]
#OA_SWL <- as.numeric(O_A$SWL..m.)
#OA_Total <- cbind(OA_Dates, OA_SWL)
#colnames(OA_Total) <- c('Date', 'SWL.OA')
#OA_Total <- data.frame(OA_Total)
#Dates <- as.POSIXct(O_A$Date.Time, format = '%m/%d/%Y %H:%M')
#OA_Total[,1] <- Dates
#OA_Total[,2] <- as.numeric(O_A$SWL..m.)



Streamflow_Michigan_Bar <- read_csv("~/Continuous Data/Streamflow Michigan Bar.csv")
Streamflow <- data.frame(Streamflow_Michigan_Bar)
colnames(Streamflow) <- c("Date", "Gage", "Discharge")
S_gage <- Streamflow[,3]
S_Discharge <- Streamflow[,2]
Streamflow$Gage <- S_gage
Streamflow$Discharge <- S_Discharge
Dates <- as.POSIXct(Streamflow[,1], format = '%m/%d/%Y %H:%M', origin = '1970-01-01')
Streamflow[,1] <- Dates
MI_Bar_Stream <- ggplot(data = Streamflow, aes(x = Date, y = Discharge)) + geom_path(color = "blue")

FairOaks_P_1_22_12_7_28_17 <- read_csv("~/Continuous Data/FairOaks_P_1.22.12_7.28.17.csv")
Dates <- as.POSIXct(FairOaks_P_1_22_12_7_28_17$Date, format = '%m/%d/%Y', origin = '1970-01-01')
Rainfall <- FairOaks_P_1_22_12_7_28_17$`Precip (in)`
FairOaks_P <- cbind(Dates, Rainfall)
FairOaks_P <- data.frame(FairOaks_P)
colnames(FairOaks_P) <- c('Date', 'Rainfall')
FairOaks_P$Date <- as.POSIXct(FairOaks_P_1_22_12_7_28_17$Date, format = '%m/%d/%Y')
FairOaks_P$CM <- FairOaks_P$Rainfall * 2.54
FO_AllRain <- ggplot(data = FairOaks_P, aes(x = Date, y = Rainfall)) +
  geom_path() +
  theme_bw()

FairOaks_P$Year <- format(as.Date(FairOaks_P$Date, format = "%m/%d/%Y"), "%Y")
for (i in 1:2015) {
  if (FairOaks_P$Year[i] == 2012)
  { P_2012 <- 0
    P_2012 <- P_2012 + FairOaks_P$Rainfall[i] }
  else if (FairOaks_P$Year[i] == 2013)
  { P_2013 <- 0
    P_2013 <- P_2013 + FairOaks_P$Rainfall[1]}
  else if (FairOaks_P$Year[i] == 2014)
  { P_2014 <- 0
  P_2014 <- P_2014 + FairOaks_P$Rainfall[1]}
  else if (FairOaks_P$Year[i] == 2015)
  { P_2015 <- 0
  P_2015 <- P_2015 + FairOaks_P$Rainfall[1]}
  else if (FairOaks_P$Year[i] == 2016)
  { P_2016 <- 0
  P_2013 <- P_2013 + FairOaks_P$Rainfall[1]}
  else (FairOaks_P$Year[i] == 2017)
  { P_2017 <- 0
  P_2017 <- P_2017 + FairOaks_P$Rainfall[1]}
}







# All Dataframes should have title: MW_##_Total with colnames: Date & SWL.## (exception: Oneto Ag is OA_Total with colname SWL.OA)


Heads_Frame <-  matrix(nrow = 13, ncol = 2)
Heads_Frame[,1] <- c("MW 2", "MW 3", "MW 5", "MW 7", "MW 9", "MW 11", "MW 13", "MW 14", "MW 17", "MW 19", "MW 20",
                     "MW 22", "MW 23")
Heads_Frame[1,2] <- MW_2_Total$SWL.2[which(MW_2_Total$Date == "2016-10-02 12:00")]
Heads_Frame[2,2] <- MW_3_Total$SWL.3[which(MW_3_Total$Date == "2016-10-02 12:00")]
Heads_Frame[3,2] <- MW_5_Total$SWL.5[which(MW_5_Total$Date == "2016-10-02 12:14")]
Heads_Frame[4,2] <- MW_7_Total$SWL.7[which(MW_7_Total$Date == "2016-10-02 12:14")]
Heads_Frame[5,2] <- MW_9_Total$SWL.9[which(MW_9_Total$Date == "2016-10-02 12:00")]
Heads_Frame[6,2] <- MW_11_Total$SWL.11[which(MW_11_Total$Date == "2016-10-02 12:00")]
Heads_Frame[7,2] <- MW_13_Total$SWL.13[which(MW_13_Total$Date == "2016-10-02 12:00")]
Heads_Frame[8,2] <- MW_14_Total$SWL.14[which(MW_14_Total$Date == "2016-10-02 12:00")]
Heads_Frame[9,2] <- MW_17_Total$SWL.17[which(MW_17_Total$Date == "2016-10-02 12:00")]
Heads_Frame[10,2] <- MW_19_Total$SWL.19[which(MW_19_Total$Date == "2016-10-02 12:00")]
Heads_Frame[11,2] <- MW_20_Total$SWL.20[which(MW_20_Total$Date == "2016-10-02 12:00")]
Heads_Frame[12,2] <- MW_22_Total$SWL.22[which(MW_22_Total$Date == "2016-10-02 12:14")]
Heads_Frame[13,2] <- MW_23_Total$SWL.23[which(MW_23_Total$Date == "2016-10-02 12:00")]
#Heads_Frame[14,2] <- OA_Total$SWL.OA[which(OA_Total$Date == "2016-10-02 12:00")]
#clean data for each well to be 1460 long, first dp on Oct 1 2016
Mw.3 <- subset(MW_3_Total, MW_3_Total$Date > "2016-10-01 12:00")
MW.3 <- MW.3[1:1460,]

#create Data Frame with all well data and add column for average 
avgall <- MW_2_Total[-(1:(which(MW_2_Total$Date == "2016-10-01 12:00"))),]
avgall <- avgall[1:1460,] #shorten to 1 water year

avgall[,3] <- 

avgall[,3] <- MW_3_Total[-(1:(which(MW_3_Total$Date == "2016-10-01 12:00"))),]



## Create Hydrographs----



 # This Section creates fixed complete hydrographs of all the data for that well. 

MW_2_Complete <- ggplot() +
  labs(title = "MW 2 Complete Time Series", y = "meters") +
  geom_path(data = MW_2_Total, aes(x = Date, y = SWL.2), 
            size = 1) +
  theme_bw() 
                     

MW_3_Complete <- ggplot(data = MW_3_Total, aes(x = Date, y = SWL.3)) +
  labs(title = "MW 3 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()

MW_5_Complete <- ggplot(data = MW_5_Total, aes(x = Date, y = SWL.5)) +
  labs(title = "MW 5 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) + 
  theme_bw()

MW_7_Complete <- ggplot(data = MW_7_Total, aes(x = Date, y = SWL.7)) +
  labs(title = "MW 7 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()

MW_9_Complete <- ggplot(data = MW_9_Total, aes(x = Date, y = SWL.9)) +
  labs(title = "MW 9 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()

MW_11_Complete <- ggplot(data = MW_11_Total, aes(x = Date, y = SWL.11)) +
  labs(title = "MW 11 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()

MW_13_Complete <- ggplot(data = MW_13_Total, aes(x = Date, y = SWL.13)) +
  labs(title = "MW 13 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_14_Complete <- ggplot(data = MW_14_Total, aes(x = Date, y = SWL.14)) +
  labs(title = "MW 14 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_17_Complete <- ggplot(data = MW_17_Total, aes(x = Date, y = SWL.17)) +
  labs(title = "MW 17 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_19_Complete <- ggplot(data = MW_19_Total, aes(x = Date, y = SWL.19)) +
  labs(title = "MW 19 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_20_Complete <- ggplot(data = MW_20_Total, aes(x = Date, y = SWL.20)) +
  labs(title = "MW 20 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_22_Complete <- ggplot(data = MW_22_Total, aes(x = Date, y = SWL.22)) +
  labs(title = "MW 22 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


MW_23_Complete <- ggplot(data = MW_23_Total, aes(x = Date, y = SWL.23)) +
  labs(title = "MW 23 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


#MW_CP1_Complete <- ggplot(data = MW_CP1_Total, aes(x = Date, y = SWL.CP1)) +
  labs(title = "MW CP1 Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


#OA_Complete <- ggplot(data = OA_Total, aes(x = Date, y = SWL.OA)) +
  labs(title = "Oneto Ag Complete Time Series", y = "SWL(m)") + 
  geom_path(size = 1) +
  theme_bw()


## Plot Hydrographs----

# Rangevalue's: You can adjust these dates to whatever you want/need. You can then use them as inputs into yLimits or yBreaks
# For example, in yLimits or yBreaks, you will just input 1,2,3 or 4 and it will correspond to whatever you edited for that rangevalue
lim1 <- as.POSIXct(strptime(c("2012-10-13 12:00", "2017-09-10 12:00"), 
                            format = "%Y-%m-%d %H:%M"))
# change breaks to be of appropriate displacement
breaks1 <- date_breaks("9 month")
# format style of date labels that will appear on x-axis
labels1 <- date_format("%b %Y")

lim2 <- as.POSIXct(strptime(c("2016-03-20 12:00", "2016-03-27 12:00"), 
                            format = "%Y-%m-%d %H:%M"))
breaks2 <- date_breaks("1 day")
labels2 <- date_format("%b-%d-%y")

lim3 <- as.POSIXct(strptime(c("2016-03-01 12:00", "2016-03-07 12:00"), 
                            format = "%Y-%m-%d %H:%M"))
breaks3 <- date_breaks("1 day")
labels3 <- date_format("%b-%d-%y")

lim4 <- as.POSIXct(strptime(c("2015-03-01 12:00", "2015-03-07 12:00"), 
                            format = "%Y-%m-%d %H:%M"))
breaks4 <- date_breaks("2 day")
labels4 <- date_format("%b-%d")


yLimits <- function(X_Limits, df1, df2)  {
  if (df1[23,2] == df2[23,2]) {
    df1_DateNumber <- as.numeric(df1[,1])
    XLimit_Number <- as.numeric(X_Limits)
    
    df1_Subset <- df1[df1_DateNumber > XLimit_Number[1] & df1_DateNumber < XLimit_Number[2],]
    
    df1_Subset <- removeNA(df1_Subset)
    
    Y.subset1_max <- max(df1_Subset[,2])
    Y.subset1_min <- min(df1_Subset[,2])
    
    Y.Range <- Y.subset1_max - Y.subset1_min
    Y.Breaks <- round(seq(Y.subset1_min, Y.subset1_max, Y.Range/8), 2)
    
    return(c(Y.subset1_min, Y.subset1_max))
    
  }
  
  else {
  df1_DateNumber <- as.numeric(df1[,1])
  df2_DateNumber <- as.numeric(df2[,1])
  XLimit_Number <- as.numeric(X_Limits)
  
  df1_Subset <- df1[df1_DateNumber > XLimit_Number[1] & df1_DateNumber < XLimit_Number[2],]
  df2_Subset <- df2[df2_DateNumber > XLimit_Number[1] & df2_DateNumber < XLimit_Number[2],]
  
  df1_Subset <- removeNA(df1_Subset)
  df2_Subset <- removeNA(df2_Subset)
  
  Y.subset1_max <- max(df1_Subset[,2])
  Y.subset1_min <- min(df1_Subset[,2])
  
  Y.subset2_max <- max(df2_Subset[,2])
  Y.subset2_min <- min(df2_Subset[,2])
  
  Y.subset_max <- max(Y.subset1_max, Y.subset2_max)
  Y.subset_min <- min(Y.subset1_min, Y.subset2_min)
  
  Y.Range <- Y.subset_max - Y.subset_min
  Y.Breaks <- seq(Y.subset_min, Y.subset_max, 8)
  
  return(c(Y.subset_min, Y.subset_max))
  }
}
#sets the limits of y axis based on date range. Date Range input (X_Limits) could be manually written as two dates,
# or lim1, lim2, lim3, or lim4. 

yBreaks <- function(X_Limits, df1, df2)  {
  if (names(df1)[2] == names(df2)[2]) {
    df1_DateNumber <- as.numeric(df1[,1])
    XLimit_Number <- as.numeric(X_Limits)
    
    df1_Subset <- df1[df1_DateNumber > XLimit_Number[1] & df1_DateNumber < XLimit_Number[2],]
    
    df1_Subset <- removeNA(df1_Subset)
    
    Y.subset1_max <- max(df1_Subset[,2])
    Y.subset1_min <- min(df1_Subset[,2])
    
    Y.Range <- Y.subset1_max - Y.subset1_min
    Y.Breaks <- round(seq(Y.subset1_min, Y.subset1_max, Y.Range/6), 2)
    
    return(Y.Breaks)
  }
  
  else  {
  
  df1_DateNumber <- as.numeric(df1[,1])
  df2_DateNumber <- as.numeric(df2[,1])
  XLimit_Number <- as.numeric(X_Limits)
  
  df1_Subset <- df1[df1_DateNumber > XLimit_Number[1] & df1_DateNumber < XLimit_Number[2],]
  df2_Subset <- df2[df2_DateNumber > XLimit_Number[1] & df2_DateNumber < XLimit_Number[2],]
  
  df1_Subset <- removeNA(df1_Subset)
  df2_Subset <- removeNA(df2_Subset)
  
  Y.subset1_max <- max(df1_Subset[,2])
  Y.subset1_min <- min(df1_Subset[,2])
  
  Y.subset2_max <- max(df2_Subset[,2])
  Y.subset2_min <- min(df2_Subset[,2])
  
  Y.subset_max <- max(Y.subset1_max, Y.subset2_max)
  Y.subset_min <- min(Y.subset1_min, Y.subset2_min)
  
  Y.Range <- Y.subset_max - Y.subset_min
  Y.Breaks <- round(seq(Y.subset_min, Y.subset_max, Y.Range/5), 2)
  
  return(Y.Breaks)
  }
}
#has the same inputs as yLimits but instead of setting the y-axis limits, it sets the breaks, on the y-axis

Hydrograph.Plot <- function(df, RangeValue, Plot_Title) 
  # RangeValue corresponds to a number 1-4 which sets a specific date range to be used when plotting that well's hydograph
  {
  
  Plot_Complete <- ggplot(data = df, aes(x = Date, y = SWL)) +
    labs(title = Plot_Title, y = "SWL(m) / Precip (in)") + 
    geom_path(color = "wheat4", size = 1.25) +
    theme_bw() + 
    geom_hline(yintercept = 0)
    # geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
    # geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "royalblue1") +
    
  
  if (RangeValue == 1) { 
    Plot_Complete + 
      scale_x_datetime(labels = labels1, 
                       breaks = breaks1, 
                       limits = lim1) + 
      scale_y_continuous(breaks = yBreaks(lim1, df, df), 
                         limits = yLimits(lim1, df, df)
                         )
  }
  else if (RangeValue == 2) {
    Plot_Complete + 
      scale_x_datetime(labels = labels2, 
                       breaks = breaks2, 
                       limits = lim2) + 
      scale_y_continuous(breaks = yBreaks(lim2, df, df), 
                         limits = yLimits(lim2, df, df),
                         sec.axis = sec_axis(~.*1, name = "Streamflow (ft)"))
  } 
  else if (RangeValue ==3) {
    Plot_Complete + 
      scale_x_datetime(labels = labels3, 
                       breaks = breaks3, 
                       limits = lim3) + 
      scale_y_continuous(breaks = yBreaks(lim3, df, df),
                         limits = yLimits(lim3, df, df),
                         sec.axis = sec_axis(~.*1, name = "Streamflow (ft)"))
  }
  else if (RangeValue ==4) {
    Plot_Complete + 
      scale_x_datetime(labels = labels4, 
                       breaks = breaks4, 
                       limits = lim4) + 
      scale_y_continuous(breaks = yBreaks(lim4, df, df),
                         limits = yLimits(lim4, df, df),
                         sec.axis = sec_axis(~.*1, name = "Streamflow (ft)"))
  }
}









all_wells <- list(MW_2_Total, MW_3_Total, MW_5_Total, MW_7_Total, MW_9_Total, MW_11_Total, MW_13_Total, 
                  MW_14_Total, MW_17_Total, MW_19_Total, MW_20_Total, MW_22_Total, MW_23_Total, OA_Total)
names(all_wells) <- c("MW 2", "MW 3", "MW 5", "MW 7", "MW 9", "MW 11", "MW 13", "MW 14", "MW 17", "MW 19", "MW 20",
                      "MW 22", "MW 23", "Oneto Ag")


####PDF Printing-----
 PDF
pdf finction prints out a pdf of whatever plots you make until you command dev.off()

# pdf(file = "Confined Response.pdf", onefile = TRUE, height = 8.5, width = 11, paper = "special")
MW_2_Complete
MW_3_Complete
MW_5_Complete
MW_7_Complete
MW_23_Complete
OA_Complete

dev.off()



Hydrograph.Plot(MW_2_Total, 1, "MW 2")
Hydrograph.Plot(MW_3_Total, 1, "MW 3")
Hydrograph.Plot(MW_5_Total, 1, "MW 5")
Hydrograph.Plot(MW_7_Total, 1, "MW 7")
Hydrograph.Plot(MW_9_Total, 1, "MW 9")
Hydrograph.Plot(MW_11_Total, 1, "MW 11")
Hydrograph.Plot(MW_13_Total, 1, "MW 13")
Hydrograph.Plot(MW_14_Total, 1, "MW 14")
Hydrograph.Plot(MW_17_Total, 1, "MW 17")
Hydrograph.Plot(MW_19_Total, 1, "MW 19")
Hydrograph.Plot(MW_20_Total, 1, "MW 20")
Hydrograph.Plot(MW_22_Total, 1, "MW 22")
Hydrograph.Plot(MW_23_Total, 1, "MW 23")
Hydrograph.Plot(MW_CP1_Total, 1, "MW CP1")
Hydrograph.Plot(OA_Total, 1, "Oneto Ag")

 
dev.off()


colnames(MW_9_Total) <- c("Date", "swl")
geom_path(data = MW_19_Total, aes(x = Date, y = SWL.19), color = "goldenrod1", size = 1) + 
  geom_path(data = MW_9_Total, aes(x = Date, y = SWL.9), color = "cadetblue", size = 1) + 
  geom_path(data = MW_7_Total, aes(x = Date, y = SWL.7), color = "tomato3", size = 1)  +
  geom_path(data = MW_11_Total, aes(x = Date, y = SWL.11), color = "steelblue1", size = 1)  +


#This is an example of a plot I made for UCWater
  Five <- ggplot(data = MW_5_Total, aes(x = Date, y = SWL.5)) + geom_line()

Fivet <- ggplot(data = MW_5_Total, aes(x = Date, y = SWL.5)) + geom_path() +
  labs(y = "SWL (m)") +
  theme_bw() +
  theme(axis.title = element_text(color = "#666666", size = 18)) + # changes format of axis title
  theme(axis.text = element_text(color = "black", size = 15)) + # changes format of axis text labels
  scale_x_datetime(labels = labels4, 
                   breaks = breaks4, 
                   limits = lim4) + 
  scale_y_continuous(breaks = yBreaks(lim4, MW_5_Total, MW_5_Total),
                    limits = yLimits(lim4, MW_5_Total, MW_5_Total))   + 
  geom_smooth() # creates smoothed average over plot
  

  
  ggplot(data = MW_5_Total, aes(x = Date, y = SWL.5)) + geom_path(color = "salmon", size = 1) +
    geom_path(data = MW_19_Total, aes(x = Date, y = SWL.19), color = "goldenrod1", size = 1) + 
    geom_path(data = MW_9_Total, aes(x = Date, y = SWL.9), color = "cadetblue", size = 1) + 
    geom_path(data = MW_7_Total, aes(x = Date, y = SWL.7), color = "tomato3", size = 1)  +
    geom_path(data = MW_11_Total, aes(x = Date, y = SWL.11), color = "steelblue1", size = 1)  +
    geom_path(data = MW_14_Total, aes(x = Date, y = SWL.14), color = "grey51", size = 1) +
    geom_path(data = MW_17_Total, aes(x = Date, y = SWL.17), color = "gray88", size = 1) +
    geom_path(data = MW_20_Total, aes(x = Date, y = SWL.20), color = "plum1", size = 1) +
    geom_path(data = MW_22_Total, aes(x = Date, y = SWL.22), color = "palegreen1", size = 1) +
    geom_path(data = MW_23_Total, aes(x = Date, y = SWL.23), color = "chocolate3", size = 1) +
    geom_path(data = OA_Total, aes(x = Date, y = SWL.OA), color = "navajowhite", size = 1) +
    geom_path(data = MW_2_Total, aes(x = Date, y = SWL.2), color = "black") +
    geom_path(data = MW_3_Total, aes(x = Date, y = SWL.3), color = "orange") + 
    stat_function(fun = mean, color = "yellow", size = 1.75) + 
    labs(y = "SWL (m)") + 
    theme_bw() +
    theme(axis.title = element_text(color = "#666666", size = 18)) +
    theme(axis.text = element_text(color = "black", size = 15)) +
    scale_x_datetime(labels = labels4, 
                     breaks = breaks4, 
                     limits = lim4) + 
    scale_y_continuous(breaks = yBreaks(lim4, MW_5_Total, MW_5_Total),
                       limits = yLimits(lim4, MW_5_Total, MW_5_Total)) 
    
    
  stat_smooth(se = TRUE, size = 2.25, color = "blue", aes(outfit=fit<<-..y..)) 
    











library(plotly)



## Maps----
library(ggmap)
  library(leaflet)
Wells <- c(MW_2_Total, MW_3_Total, MW_5_Total, MW_7_Total, MW_9_Total, MW_11_Total, MW_13_Total, 
                  MW_14_Total, MW_17_Total, MW_19_Total, MW_20_Total, MW_22_Total, MW_23_Total, OA_Total)
Latitude <- c(38.184545, 38.183480, 38.183475, 38.181862, 38.181817, 38.181757, 38.18363, 38.18502, 38.174813, 38.174810, 38.174799,
              38.173058, 38.173026, 38.18975)
Longitude <- c(121.224612, 121.23112, 121.223294, 121.232768, 121.225307, 121.22700, 121.23140, 121.224212, 121.225443, 121.224570, 121.222750,
               121.232882, 121.225415, 121.224344)
well <-  c("MW 2", "MW 3", "MW 5", "MW 7", "MW 9", "MW 11", "MW 13", "MW 14", "MW 17", "MW 19", "MW 20",
           "MW 22", "MW 23", "Oneto Ag")
WellMap <- cbind(well, Latitude, Longitude)
WellMap <- data.frame(WellMap)


map <- qmap('Cosumnes River', zoom = 12, maptype = 'satellite')

map + geom_point(data = WellMap, aes(x = Longitude, y = Latitude))


coordsOHWD <- read.csv(textConnection("Well, Lat, Long
                                  MW 2, 38.3122625, -121.37947778
                                  MW 5, 38.30965278, -121.37581667
                                  MW 7, 38.30517222, -121.39102222
                                  MW 9, 38.30504722, -121.38140833
                                  "))

OHWD <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addMarkers(lng = 38.3122625, lat = -121.37947778, 
             label = "MW 2",
             labelOptions = labelOptions(noHide = TRUE, textsize = "12px", direction = "top")) %>%
  addMarkers(lng = 38.30965278, lat = -121.37581667, 
             label = "MW 5",
             labelOptions = labelOptions(noHide = TRUE, textsize = "12px", direction = "left")) %>%
  addMarkers(lng = 38.30517222, lat = -121.39102222, 
             label = "MW 7",
             labelOptions = labelOptions(noHide = TRUE, textsize = "12px", direction = "top")) %>%
  addMarkers(lng = 38.30504722, lat = -121.38140833, 
             label = "MW 9",
             labelOptions = labelOptions(noHide = TRUE, textsize = "12px", direction = "top")) 
  
## Hard Plots-----
for(i in 1:length(output))
{
  df1 = output[[i]]
  lims <- as.POSIXct(strptime(c("2013-10-01 00:00", "2016-09-29 20:00"), format = "%Y-%m-%d %H:%M"))
  plotdf1 <- ggplot(data = df1, aes(x = df1[,1], y = df1[,3])) + 
    geom_line(colour = "springgreen4", size = 2.5) +
    labs(title = paste("MW", i, sep = " "), y = "SWL(m)") + 
    geom_line(colour = "springgreen4") +
    scale_x_datetime(limits = lims, breaks = date_breaks("3 months"), labels= date_format("%m/%Y")) + 
    theme_bw(axis.text.x = element_text(angle = 74))
  print(plotdf1)
}

MW_2_Complete + 
  labs(title = "MW 2 Complete Time Series", y = "meters") +
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_2_Total, Streamflow)) 


MW_3_Complete +
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_3_Total, Streamflow))


MW_5_Complete +
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_5_Total, Streamflow))


MW_7_Complete +
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_7_Total, Streamflow))
 

MW_9_Complete + 
geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_9_Total, Streamflow))


MW_11_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_11_Total, Streamflow))


MW_13_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_13_Total, Streamflow))


MW_14_Complete + 
geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_14_Total, Streamflow))


MW_17_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_17_Total, Streamflow))


MW_19_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_19_Total, Streamflow))


MW_20_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_20_Total, Streamflow))


MW_22_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_22_Total, Streamflow))


MW_23_Complete +
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_23_Total, Streamflow))


MW_CP1_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, MW_CP1_Total, Streamflow))


OA_Complete + 
  geom_area(data = FairOaks_P, aes(x = Dates, y = Rainfall), color = "Red") + 
  geom_path(data = Streamflow, aes(x = Date, y = Gage), color = "Blue") +
  scale_x_datetime(labels = date_format("%m/%y"),
                   breaks = date_breaks("1 month"),
                   limits = lim2)           + 
  scale_y_continuous(breaks = waiver(), limits = yLimits(lim2, OA_Total, Streamflow))
