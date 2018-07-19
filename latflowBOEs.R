library(dplyr)
outw <- c(5.5, 7.2, 7.5, 8.2, 9.2)
inw <- c(6.1, 7, 7.5, 7.8, 8.5)
dfw <- cbind(outw, inw) %>% as.data.frame()
dfw$diff <- dfw$inw - dfw$out 
dfw$slopes <- dfw$diff/670
dfw
mean(dfw$slopes)
mean(dfw$diff)

#now with the 2/19/2016 lows raster
#picking values inside and outside of each side of the boundary in Arc
outw2 <- c(-1.5, -1.8, -1.6, .7, 1.7)
inw2 <- c(-1.5, -1.6, -1.3, .44, 2.1)
dfw2 <- cbind(outw2, inw2) %>% as.data.frame()
dfw2$diff <- dfw2$inw2 - dfw2$outw2
dfw2$slopes <- dfw2$diff/670

outeast <- c(-.8, -.56, .31, .67, 1.45) 
ineast <- c(-.9, -.5, .01, .61, 1.75)
e <- cbind(outeast, ineast) %>% as.data.frame()
e$diff <- e$ineast - e$outeast
e$slopes <- e$diff/670
(2681*15)*.5558*mean(e$slopes)
(2681*15)*.5558*mean(dfw2$slopes)
