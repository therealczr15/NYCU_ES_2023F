getwd()
setwd("D:/ES/LAB4")
library(rio)
library(lubridate)
library(ggplot2)
library(gridExtra)

xlsx <- import("weatherdata.xlsx")
head(xlsx)

date <- xlsx$Date
dt <- mdy_hms(date)


data <- data.frame(time = dt, RH = xlsx$RH, Rain = xlsx$Rain )


Sys.setlocale("LC_ALL", "English")

p1 <- ggplot(data = data, aes(time, RH)) + 
  geom_point(size = 0.05, color = "black", shape = 0) + 
  geom_smooth(color = "blue") +
  labs(title = "Relative Humidity June-Oct 2020", x = "Date-Time", y = "RH[%]")


p2 <- ggplot(data = data, aes(time, Rain)) +
  geom_bar(stat = "identity", color = "blue") +
  labs(title = "Precipitation June-Oct 2020", x = "Date-Time", y = "Precipitation[mm]")

p <- grid.arrange(p1,p2,nrow = 2)

Sys.setlocale()
ggsave("109511207-蔡宗儒-Visualization.png",plot = p,width = 10,height = 5)
