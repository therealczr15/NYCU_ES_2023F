library(ggplot2)
library(rio)

DJI.file <- "DJI.xlsx"
COVID.file <- "Covid19US.xlsx"

DJI <- import(DJI.file)
COVID <- import(COVID.file)

DJI$Date <- as.Date(DJI$Date, format='%m/%d/%Y')
COVID$Date <- as.Date(COVID$Date)

# inner join
df <- merge(x=DJI, y=COVID, by="Date")

# adjust the two ranges to create graph
ylim.prim <- c(0, 600000)
ylim.sec <- c(10000, 40000)
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b * (ylim.prim[1] - ylim.sec[1])

ggplot(data=df, aes(x=Date, y=NewCase, group = 1)) +
  geom_col() +
  geom_line(aes(y=a+Close*b), color="red") + scale_x_date(date_breaks="2 month", date_labels="%Y\n%b") +
  scale_y_continuous("Comfirmed Cases", sec.axis = sec_axis(~ (. - a)/b, name="Close")) +
  ggtitle("Covid-DJI")