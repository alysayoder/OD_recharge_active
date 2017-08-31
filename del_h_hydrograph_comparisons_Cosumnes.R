# setwd()
setwd("C:/Users/ayoder/BoxSync/CH")

# get data

#merged.data.frame=Reduce(function(...)merge(...,all=T),d)

# load packages
packagelist = list("ggplot2", "colorspace", "reshape2")
lapply(packagelist, library, character.only = TRUE)

# assign data frame variables
MW_11 <- read.csv("MW-11.csv")
MW_2 <- read.csv("MW-2.csv" )
MW_3 <- read.csv("MW-3.csv")
MW_5 <- read.csv("MW-5.csv")
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

#MW11----
MW11_1617 = MW_11[-(1:76734),-(4:14)]
MW11_1617 = MW11_1617[,-(2)]
str(MW11_1617)
Dates11 = as.POSIXct(MW11_1617$Date.Time..PDT., format = '%m/%d/%Y %H:%M')
ggplot(MW11_1617, aes(Dates11, MW11_1617$SWL..m.))+geom_point()

#MW13----
MW13_1617 = MW_13[-(1:76734),-(4:14)]
MW13_1617 = MW13_1617[,-(2)]
Dates13 = as.POSIXct(MW13_1617$Date.Time, format = '%m/%d/%Y %H:%M')
ggplot(MW13_1617, aes(Dates13, MW13_1617$SWL..m.))+geom_point()

#MW14----
MW14_1617 = MW_14[-(1:76734),-(4:14)]
MW14_1617 = MW14_1617[,-(2)]
Dates14= as.POSIXct(MW14_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW14_1617, aes(Dates14, MW14_1617$SWL..m.))+geom_point()

#MW17----
MW17_1617 = MW_17[-(1:76734),-(4:14)]
MW17_1617 = MW17_1617[,-(2)]
Dates17= as.POSIXct(MW17_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW17_1617, aes(Dates17, MW17_1617$SWL..m.))+geom_point()

#MW19----
MW19_1617 = MW_19[-(1:106891),-(4:14)]
MW19_1617 = MW19_1617[,-(2)]
Dates19= as.POSIXct(MW19_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW19_1617, aes(Dates19, MW19_1617$SWL..m.))+geom_point()

#MW2----
MW2_1617 = MW_2[-(1:106885),-(4:14)]
MW2_1617 = MW2_1617[,-(2)]
Dates2= as.POSIXct(MW2_1617$Date.Time, format = '%m/%d/%Y %H:%M')
ggplot(MW2_1617, aes(Dates2, MW2_1617$SWL..m.))+geom_point()

#MW20----
MW20_1617 = MW_20[-(1:76734),-(4:14)]
MW20_1617 = MW20_1617[,-(2)]
Dates20= as.POSIXct(MW20_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW20_1617, aes(Dates20, MW20_1617$SWL..m.))+geom_point()

#MW22----
MW22_1617 = MW_22[-(1:76734),-(4:14)]
MW22_1617 = MW22_1617[,-(2)]
Dates22= as.POSIXct(MW22_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW22_1617, aes(Dates22, MW22_1617$SWL..m.))+geom_point()

#MW23----
MW23_1617 = MW_23[-(1:76734),-(4:14)]
MW23_1617 = MW23_1617[,-(2)]
Dates23= as.POSIXct(MW23_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW23_1617, aes(Dates23, MW23_1617$SWL..m.))+geom_point()

#MW3----
MW3_1617 = MW_3[-(1:76734),-(4:14)]
MW3_1617 = MW3_1617[,-(2)]
Dates3= as.POSIXct(MW3_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW3_1617, aes(Dates3, MW3_1617$SWL..m.))+geom_point()

#MW5----
MW5_1617 = MW_5[-(1:106889),-(4:14)]
MW5_1617 = MW5_1617[,-(2)]
Dates5= as.POSIXct(MW5_1617$Date.Time, format = '%m/%d/%y %H:%M')
plot_MW5 <- ggplot(MW5_1617, aes(Dates5, MW5_1617$SWL..m.)) + geom_point() + theme_bw() + xlab("Date")
 + ylab("SWL (m)")

#MW7----
MW7_1617 = MW_7[-(1:76734),-(4:14)]
MW7_1617 = MW7_1617[,-(2)]
Dates7= as.POSIXct(MW7_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW7_1617, aes(Dates7, MW7_1617$SWL..m.))+geom_point()

#MW9----
MW9_1617 = MW_9[-(1:76734),-(4:14)]
MW9_1617 = MW9_1617[,-(2)]
Dates9= as.POSIXct(MW9_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MW9_1617, aes(Dates9, MW9_1617$SWL..m.))+geom_point()

#MWCP1----
MWCP1_1617 = MW_CP1[-(1:140241),-(5:14)]
MWCP1_1617 = MWCP1_1617[,-(2:3)]
DatesCP1= as.POSIXct(MWCP1_1617$Date.Time, format = '%m/%d/%y %H:%M')
ggplot(MWCP1_1617, aes(DatesCP1, MWCP1_1617$SWL..m.))+geom_point(colour = "indianred3")

#Plot All----

p <- ggplot() +
  # MW11
  geom_point(data=MW11_1617, aes(x=Dates11, y=MW11_1617$SWL..m.), colour = "black") +
  # MW13
  geom_point(data=MW13_1617, aes(x=Dates13, y=MW13_1617$SWL..m.), colour = "darksalmon") +
  # MW14
  geom_point(data=MW14_1617, aes(x=Dates14, y=MW14_1617$SWL..m.), colour = "darkseagreen4") +
  # MW17
  geom_point(data=MW17_1617, aes(x=Dates17, y=MW17_1617$SWL..m.), colour = "pink") +
  # MW19
  geom_point(data=MW19_1617, aes(x=Dates19, y=MW19_1617$SWL..m.), colour = "turquoise") +
  # MW2
  geom_point(data=MW2_1617, aes(x=Dates2, y=MW2_1617$SWL..m.), colour = "goldenrod") +
  # MW20
  geom_point(data=MW20_1617, aes(x=Dates20, y=MW20_1617$SWL..m.), colour = "indianred4") +
  #MW22
  geom_point(data=MW22_1617, aes(x=Dates22, y=MW22_1617$SWL..m.), colour = "blue") +
  #MW23
  geom_point(data=MW23_1617, aes(x=Dates23, y=MW23_1617$SWL..m.), colour = "red") +
  #MW3
  geom_point(data=MW3_1617, aes(x=Dates3, y=MW3_1617$SWL..m.), colour = "green") +
  #MW5
  #geom_point(data=MW5_1617, aes(x=Dates5, y=MW5_1617$SWL..m.), colour = "yellow") +
  #MW7
  geom_point(data=MW7_1617, aes(x=Dates7, y=MW7_1617$SWL..m.), colour = "purple") +
  #MW9
  geom_point(data=MW9_1617, aes(x=Dates9, y=MW9_1617$SWL..m.), colour = "orange") +
  #MWCP1
  geom_point(data=MWCP1_1617, aes(x=DatesCP1, y=MWCP1_1617$SWL..m.), colour = "pink") +
  
  theme_bw() + xlab("Date") + ylab("SWL (m)") + ggtitle("2016-17 Hydrographs from Oneto-Denier Monitoring Wells") 
  
p

### Publish to PDF ####
pdf(file = "prelim_plot.pdf", paper = "special", width = 11, height = 8.5, onefile = TRUE)
p
dev.off()

#### Melt to Fix Legend Issue ####
Dates= as.POSIXct(Dates, format = '%m/%d/%y %H:%M')
Dates= Dates[-(64863:110000)]
dmelt <- melt(list(MW11_1617=MW11_1617, MW13_1617=MW13_1617, MW14_1617=MW14_1617), id.vars = "SWL..m.")
ggplot(dmelt, aes(Dates, SWL..m., colour = L1)) + geom_point() +
  scale_colour_manual(values = c("MW11_1617"="blue", "MW13_1617"="green", "MW14_1617"="red"))
