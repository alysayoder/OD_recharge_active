date <- c("4/3/2018", "4/19/2018", "5/3/2018", "5/29/2018", "6/15/2018", "7/12/2018", "8/9/2018")
date <- as.Date(date, format = "%m/%d/%Y")
left_girth <- c(11.25, 12.0, 12.05, 12.5, 12.5, 12.75, 13.0)
right_girth <- c(13.25, NA, 13.5, 13.5, 13.0, 13.5, 13.75)
df <- data.frame(date,left_girth, right_girth)
plot(df, type = "l")

devtools::source_gist("524eade46135f6348140", filename = "ggplot_smooth_func.R")


library(ggplot2)
ggplot(data = df, aes(date)) + 
  geom_smooth(aes(y = left_girth, color = "left_girth"), method = "lm", se=F) + 
  geom_smooth(aes(y = right_girth, color = "right_girth"), method = "lm", se=F) +
  geom_point(aes(y = right_girth, color = "right_girth"), size = 2) +
  geom_point(aes(y = left_girth, color = "left_girth"), size = 2) +
  labs(title = "Gains", subtitle = "Steve's Calf's Girth Journey", x = "Date", y = "Girth (in)") +
  theme_bw() 
  # stat_smooth_func(aes(x=date, y = left_girth), geom="text", method = "lm", hjust= 0, parse=T)+
  # stat_smooth_func(aes(x=date, y = right_girth), geom="text", method = "lm", hjust= 0, parse=T)

