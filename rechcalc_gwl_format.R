#Formatting groundwater level data at the min or max of desired recharge event to estimate volume of recharge for. 
#Purpose is to format input files for recharge function. 

rechcalc_gwl_format <- function(hilodata) {
rownames(hilodata) <- NULL
colnames(hilodata) <- NULL
hilodata <- evhi14[-1,]

  label <- read.csv(here('data', "MW_label_allsets.csv"), header = F) #get lat long data, already in order of allsets.csv wells
  label[3] <- NULL #removes well ID text column, leaving only lat long coords

hilodata <- data.frame(c(label,hilodata))
colnames(hilodata) <- NULL
rownames(hilodata) <- NULL
colnames(hilodata) <- c("lat", "long", "level")  
}
  
