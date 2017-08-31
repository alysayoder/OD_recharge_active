setwd('/Users/richpauloo/Desktop/test')

dat <- read.csv('MW-5.csv', header = TRUE, stringsAsFactors = FALSE)
dat <- dat[,c(1,3)]
colnames(dat) <- c("date", "swl")
head(dat)
str(dat)
dat$date = as.POSIXct(dat$date, format = '%m/%d/%y %H:%M')
dat$index <- seq(1:nrow(dat))
dat <- na.omit(dat)
dat$swl <- as.numeric(dat$swl)

library(tidyverse)
dat %>% 
  filter(index > 106890) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = swl))



