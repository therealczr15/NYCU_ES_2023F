#install.packages("MASS")
library("MASS")
library(moments)
library(ggplot2)
library(gridExtra)
d = c (37.0 ,37.5, 38.1, 40.0, 40.2, 40.8, 41.0, 42.0, 43.1, 43.9, 
       44.1, 44.6, 45.0, 46.1, 47.0, 50.2, 55.0, 56.0, 57.0, 58.0,
       62.0, 64.3, 68.8, 70.1, 74.5)
x <- seq(20,80,0.1)
data = data.frame(x = d)
density = density(d)
data.density = data.frame(x = density$x, y = density$y)
fit.weibull = fitdistr(d, "weibull")
shape = fit.weibull$estimate[1]
scale = fit.weibull$estimate[2]
d.weibull = dweibull(x, shape = shape, scale = scale)

fit.normal = fitdistr(d, "normal")
mean = fit.normal$estimate[1]
sd = fit.normal$estimate[2]
d.normal = dnorm(x, mean = mean, sd = sd)

fit = data.frame(x = x, wei = d.weibull, nor = d.normal)

p1 = ggplot(data = data ,aes(x,..density..)) +
  labs(title = "Fitting distribution",
       x = "Material Strength",
       y = "Density") +
  scale_x_continuous(breaks = seq(0,100,25)) +
  coord_cartesian(xlim = c(10, 100)) +
  geom_line(data = data.density, aes(x,y),
            color = "black",
            size =1.0) +
  geom_line(data = fit, aes(x,wei), # weibull
            color = "red",
            size =1.0) +
  geom_line(data = fit, aes(x,nor), # normal
            color = "black",
            size =1.0,
            linetype = 2) +
  theme_bw()




p2 = ggplot() +
  aes(sample = d) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(line.p = c(0.25, 0.75), col = "blue") +
  labs(title = "QQplot",
       x = "Theoretical Normal Distribution Quantile",
       y = "Observed Data")

grid.arrange(p1,p2,nrow = 2)

skewness(d) #看起來像是正偏，量出skewness也的確是正偏




