date <- c("4/3/2018", "4/19/2018", "5/3/2018", "5/29/2018", "6/15/2018", "7/12/2018")
date <- as.Date(date, format = "%m/%d/%Y")
left_girth <- c(11.25, 12.0, 12.05, 12.5, 12.5, 12.75)
right_girth <- c(13.25, 14.25, 13.5, 13.5, 13.0, 13.5)
df <- data.frame(date,left_girth, right_girth)
plot(df, type = "l")
library(ggplot2)
ggplot(data = df, aes(date)) + 
  geom_smooth(aes(y = left_girth, color = "left_girth")) + 
  geom_smooth(aes(y = right_girth, color = "right_girth")) +
  geom_point(aes(y = right_girth, color = "right_girth"), size = 2) +
  geom_point(aes(y = left_girth, color = "left_girth"), size = 2) +
  labs(title = "Steve's Girth Journey", subtitle = "A Muscular Odyssey; the Triumph over Atrophy", x = "Date", y = "Girth (in)") +
  theme_bw()
