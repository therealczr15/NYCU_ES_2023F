library(rio)
data.head <- c("全", "北", "東", "香山")

data.5 <- import("month5.xlsx")[data.head]
data.6 <- import("month6.xlsx")[data.head]
data.7 <- import("month7.xlsx")[data.head]
data.8 <- import("month8.xlsx")[data.head]
data.9 <- import("month9.xlsx")[data.head]
data.10 <- import("month10.xlsx")[data.head]
data.11 <- import("month11.xlsx")[data.head]
data <- rbind(data.5, data.6, data.7, data.8, data.9, data.10, data.11)

library(dplyr)
data <- data %>% 
  rename("all" = "全",
         "north" = "北",
         "east" = "東",
         "SiangShan" = "香山"
  )


library(ggplot2)
library(ggfortify)
ts.data <- ts(data$east, start = c(1, 7), frequency = 7)
str(ts.data)

library(forecast)
best <- auto.arima(ts.data)
best

f.data <- forecast(best, level = c(95), h=31)
f.data

autoplot(f.data) +
  ggtitle("Forecasts from ARIMA") +
  xlab("week numvber from May") +
  ylab("Population")

data.12 <- import("month12.xlsx")
data.12
f.data$mean

prediction <- append(data$east, as.numeric(f.data$mean))
length(prediction)
data.12 <- data.12%>%
  rename("all" = "全",
         "north" = "北",
         "east" = "東",
         "SiangShan" = "香山" 
  )
true <- append(data$east, data.12$east)
length(true)

data.plot <- data.frame(prediction = prediction, 
                        observation = true)



comparison.data <- ts(data.plot, start = c(1,1)
                      , frequency = 1)
autoplot(comparison.data,size = 1.0)+
  ggtitle("Forecasting Results") +
  xlab("days from May month") +
  ylab("Population") + 
  xlim(c(180,245)) +
  ylim(c(0,1000))
