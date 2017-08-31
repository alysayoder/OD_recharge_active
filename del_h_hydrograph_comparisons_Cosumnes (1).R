# setwd()
setwd("C:/Users/ayoder/BoxSync/CH")


#merged.data.frame=Reduce(function(...)merge(...,all=T),d)

# load packages
packagelist = list("ggplot2", "colorspace", "reshape2", "tidyverse")
lapply(packagelist, library, character.only = TRUE)

# load data
MW_11 <- read.csv("MW-11.csv")
MW_2 <- read.csv("MW-2.csv")
MW_3 <- read.csv("MW-3.csv")
MW_5 <- read.csv("MW-5.csv", header = TRUE, stringsAsFactors = FALSE)
MW_7 <- read.csv("MW-7.csv")
MW_9 <- read.csv("MW-9.csv")
MW_13 <- read.csv("MW-13.csv")
MW_14 <- read.csv("MW-14.csv")
MW_17 <- read.csv("MW-17.csv")
MW_19 <- read.csv("MW-19.csv")
MW_20 <- read.csv("MW-20.csv")
MW_22 <- read.csv("MW-22.csv")
MW_23 <- read.csv("MW-23.csv" )
MW_CP1 <- read.csv("MW-CP1.csv")

# new approach
well_names <- paste("MW-", c(11,2,3,5,7,9,13,14,17,19,20,22,23,'CP1'), sep="")
well_files <- paste(well_names, ".csv", sep="")
dat <- sapply(well_files, read.csv, header = TRUE, stringsAsFactors = FALSE)

# manipulate data for plotting
names(dat)[[1]] <- "test"

# subsetting


#MW11----
MW11_1617 = MW_11[-(1:(which(MW_11$Date.Time == '12/31/2015 23:45'))),]
MW11_1617 = MW11_1617[,c(1,3)]
Dates11 = as.POSIXct(MW11_1617$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
ggplot() + geom_line(aes(x=Dates11, y=MW11_1617$SWL..m.))

#MW13----
MW13_1617 = MW_13[-(1:76729),]
MW13_1617 = MW13_1617[,c(1,3)]
Dates13 = as.POSIXct(MW13_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW13_1617, aes(Dates13, MW13_1617$SWL..m.))+geom_line()

#MW14----
MW14_1617 = MW_14[-(1:76734),]
MW14_1617 = MW14_1617[,c(1,3)]
Dates14= as.POSIXct(MW14_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW14_1617, aes(Dates14, MW14_1617$SWL..m.))+geom_line()

#MW17----
MW17_1617 = MW_17[-(1:76728),]
MW17_1617 = MW17_1617[,c(1,3)]
Dates17= as.POSIXct(MW17_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW17_1617, aes(Dates17, MW17_1617$SWL..m.))+geom_line()

#MW19----
MW19_1617 = MW_19[-(1:106891),]
MW19_1617 = MW19_1617[,c(1,3)]
Dates19= as.POSIXct(MW19_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW19_1617, aes(Dates19, MW19_1617$SWL..m.))+geom_line()

#MW2----
MW2_1617 = MW_2[-(1:106885),]
MW2_1617 = MW2_1617[,c(1,3)]
Dates2= as.POSIXct(MW2_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW2_1617, aes(Dates2, MW2_1617$SWL..m.))+geom_line()

#MW20----
MW20_1617 = MW_20[-(1:76734),]
MW20_1617 = MW20_1617[,c(1,3)]
Dates20= as.POSIXct(MW20_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW20_1617, aes(Dates20, MW20_1617$SWL..m.))+geom_line()

#MW22----
MW22_1617 = MW_22[-(1:106892),]
MW22_1617 = MW22_1617[,c(1,3)]
Dates22= as.POSIXct(MW22_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW22_1617, aes(Dates22, MW22_1617$SWL..m.))+geom_line()

#MW23----
MW23_1617 = MW_23[-(1:(which(MW_23$Date.Time == '12/31/15 23:45'))),]
MW23_1617 = MW23_1617[,c(1,3)]
Dates23= as.POSIXct(MW23_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW23_1617, aes(Dates23, MW23_1617$SWL..m.))+geom_line()

#MW3----
MW3_1617 = MW_3[-(1:(which(MW_3$Date.Time..PDT. == '12/31/15 23:45'))),]
MW3_1617 = MW3_1617[,c(1,3)]
Dates3= as.POSIXct(MW3_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW3_1617, aes(Dates3, MW3_1617$SWL..m.))+geom_line()

#MW5----
#cleaner code here 
MW5_1617 <- MW_5
MW5_1617 = MW5_1617[,c(1,3)]
colnames(MW5_1617) <- c("date", "swl")
MW5_1617$date <- as.POSIXct(MW5_1617$date, format = '%m/%d/%y %H:%M')
MW5_1617 <- filter(MW5_1617, MW5_1617$date >= '2016-01-01 00:00:00')
MW5_1617 <- na.omit(MW5_1617)
MW5_1617$swl <- as.numeric(MW5_1617$swl)

ggplot(data = MW5_1617, mapping = aes(x = MW5_1617$date)) + geom_line(aes(y = MW5_1617$swl))


#MW7----
MW7_1617 = MW_7[-(1:which(MW_7$Date.Time..PDT. == '12/31/15 23:45')),-(4:14)]
MW7_1617 = MW7_1617[,-(2)]
Dates7= as.POSIXct(MW7_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW7_1617, aes(Dates7, MW7_1617$SWL..m.))+geom_line()

#MW9----
MW9_1617 = MW_9[-(1:which(MW_9$Date.Time..PDT. == '12/31/15 23:45')),-(4:14)]
MW9_1617 = MW9_1617[,-(2)]
Dates9= as.POSIXct(MW9_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW9_1617, aes(Dates9, MW9_1617$SWL..m.))+geom_line()

#MWCP1----
MWCP1_1617 = MW_CP1[-(1:140241),-(5:14)]
MWCP1_1617 = MWCP1_1617[,-(2:3)]
DatesCP1= as.POSIXct(MWCP1_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MWCP1_1617, aes(DatesCP1, MWCP1_1617$SWL..m.))+geom_line(colour = "indianred3")

#Plot All----

p <- ggplot() +
  # MW11
  geom_line(data=MW11_1617, aes(x=Dates11, y=MW11_1617$SWL..m.), colour = "black") +
  # MW13
  geom_line(data=MW13_1617, aes(x=Dates13, y=MW13_1617$SWL..m.), colour = "darksalmon") +
  # MW14
  geom_line(data=MW14_1617, aes(x=Dates14, y=MW14_1617$SWL..m.), colour = "darkseagreen4") +
  # MW17
  geom_line(data=MW17_1617, aes(x=Dates17, y=MW17_1617$SWL..m.), colour = "pink") +
  # MW19
  geom_line(data=MW19_1617, aes(x=Dates19, y=MW19_1617$SWL..m.), colour = "turquoise") +
  # MW2
  geom_line(data=MW2_1617, aes(x=Dates2, y=MW2_1617$SWL..m.), colour = "goldenrod") +
  # MW20
  geom_line(data=MW20_1617, aes(x=Dates20, y=MW20_1617$SWL..m.), colour = "indianred4") +
  #MW22
  geom_line(data=MW22_1617, aes(x=Dates22, y=MW22_1617$SWL..m.), colour = "blue") +
  #MW23
  geom_line(data=MW23_1617, aes(x=Dates23, y=MW23_1617$SWL..m.), colour = "red") +
  #MW3
  geom_line(data=MW3_1617, aes(x=Dates3, y=MW3_1617$SWL..m.), colour = "green") +
  #MW5
  geom_line(data=MW5_1617, aes(x=MW5_1617$date, y=MW5_1617$swl), colour = "yellow") +
  #MW7
  geom_line(data=MW7_1617, aes(x=Dates7, y=MW7_1617$SWL..m.), colour = "purple") +
  #MW9
  geom_line(data=MW9_1617, aes(x=Dates9, y=MW9_1617$SWL..m.), colour = "orange") +
  #MWCP1
  geom_line(data=MWCP1_1617, aes(x=DatesCP1, y=MWCP1_1617$SWL..m.), colour = "pink") +
  
  theme_bw() + xlab("Date") + ylab("SWL (m)") + ggtitle("2016-17 Hydrographs from Oneto-Denier Monitoring Wells") 
  
p

#publish to PDF----
pdf(file = "prelim_plot.pdf", paper = "special", width = 11, height = 8.5, onefile = TRUE)
p
dev.off()



#create one complete data frame----

#uniform column names
nlist <- list(MW_11, MW_2, MW_3, MW_5, MW_7, MW_9, MW_13, MW_14, MW_17, MW_19, MW_20, MW_22, MW_23, MW_CP1)
coln <- c("date", "SWL")
lapply(nlist, setNames, nm = coln)

#assign and organize mydat
mydat <- MW_11
mydat <- mydat[,c(1,3)]
head(mydat)
colnames(mydat) <- coln
mydat$date <- as.POSIXct(mydat$date, format = '%m/%d/%Y %H:%M')
mydat <- subset(mydat, date > "2015-12-31 23:45:00")


#assign length of data frame to max length of data
list1617 = list(MW11_1617, MW13_1617, MW14_1617, MWCP1_1617, MW17_1617, MW19_1617, 
                MW2_1617, MW20_1617, MW22_1617, MW23_1617, MW3_1617, MW5_1617, MW7_1617, MW9_1617)
max_length1617 <- lapply(list1617, nrow)
maxrow <- as.integer(max_length1617[which.max(max_length1617)])
maxrow <- 31179
attributes(mydat) <- list(row.names = 1:maxrow, class = 'data.frame')

str(mydat)

mydat[,3] <- c(MW13_1617$SWL..m., rep(NA, maxrow-nrow(MW13_1617)))
mydat[,4] <- c(MW14_1617$SWL..m., rep(NA, maxrow-nrow(MW14_1617)))
mydat[,5] <- c(MWCP1_1617$SWL..m., rep(NA, maxrow-nrow(MWCP1_1617)))
mydat[,6] <- c(MW17_1617$SWL..m., rep(NA, maxrow-nrow(MW17_1617)))
mydat[,7] <- c(MW19_1617$SWL..m., rep(NA, maxrow-nrow(MW19_1617)))
mydat[,8] <- c(MW2_1617$SWL..m., rep(NA, maxrow-nrow(MW2_1617)))
mydat[,9] <- c(MW20_1617$SWL..m., rep(NA, maxrow-nrow(MW20_1617)))
mydat[,10] <- c(MW22_1617$SWL..m., rep(NA, maxrow-nrow(MW22_1617)))
mydat[,11] <- c(MW23_1617$SWL..m., rep(NA, maxrow-nrow(MW23_1617)))
mydat[,12] <- c(MW3_1617$SWL..m., rep(NA, maxrow-nrow(MW3_1617)))                
mydat[,13] <- c(MW5_1617$swl, rep(NA, maxrow-nrow(MW5_1617)))
mydat[,14] <- c(MW7_1617$SWL..m., rep(NA, maxrow-nrow(MW7_1617)))                
mydat[,15] <- c(MW9_1617$SWL..m., rep(NA, maxrow-nrow(MW9_1617)))


#clean data, assign names
colnames(mydat) <- c('Date', 'MW11', 'MW13', 'MW14', 'MWCP1', 'MW17', 
                     'MW19', 'MW2', 'MW20', 'MW22', 'MW23', 'MW3', 'MW5', 'MW7', 'MW9')

#add column of means
mydat[,16] <- rowMeans(mydat, na.rm = FALSE)

