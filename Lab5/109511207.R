data = c (20.0, 25.0, 30.0, 37.0, 37.5, 38.1, 40.0, 40.2, 40.8, 41.0, 
          42.0, 43.1, 43.9, 44.1, 44.6, 45.0, 46.1, 47.0, 50.2, 55.0, 
          56.0, 57.0, 58.0, 62.0, 64.3, 68.8, 70.1, 74.5, 90.0, 100.0)

library(ggplot2)
#install.packages("moments")
library(moments)
library(gridExtra)


df <- data.frame(data)
density <- density(data, width = 20)
data.density <- data.frame(x = density$x, y = density$y)

hisp <- hist(data, breaks = 6, plot = FALSE)

p1 <- ggplot(data = df, aes(data,..density..)) +
  geom_histogram(fill = "blue", alpha = 0.5, breaks = hisp$breaks) +
  geom_line(data = data.density, aes(x,y), color = "red", size = 1) +
  labs(title = "Histogram and Density curve of data", x = "Measurement data [MPa]", y = "Density") +
  theme_bw()




p2 <- ggplot(data = df, aes(data)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, outlier.size = 1.5, coef = 1.0) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_y_discrete() +
  labs(title = "Boxplot of data", x = "Measurement data [MPa] ") +
  theme_bw()


skewness(data)
kurtosis(data)

grid.arrange(p1,p2,nrow = 2)



  