library("datasets")
library(ggplot2)

data(cars)
str(cars)
data = cars

# line
n <- length(data$speed)
r <- cor(data$speed, data$dist, method = "pearson")
r

model1 <- lm(dist~speed+0, data = data)
summary(model1)
b1 <- model1$coefficients[1]


pre.x <- seq(0,30,0.01)
pre.y <- b1*pre.x
line <- data.frame(x=pre.x,y=pre.y)

# curve
model2 <- lm(dist~speed+I(speed^2)+0, data = data)
summary(model2) 
a2 <- model2$coefficients[1]
b2 <- model2$coefficients[2]

pre.x <- seq(0,30,0.01)
pre.y <- a2*pre.x+b2*pre.x^2
curve <- data.frame(x=pre.x,y=pre.y)

sub1.text <- expression(paste("r"^"2","=0.896"," se = 16.26(line)"," r"^"2","=0.913"," se = 15.02(curve)"))
sub2.text <- expression(paste("dist = ","2.91xspeed(dashed line), dist = 1.24xspeed+0.0901xspeed"^"2"~"(solid line)"))

ggplot(data = data, aes(speed,dist)) +
  geom_point(size = 0.9) +
  geom_line(data = line, aes(x,y),linetype = 2)+
  geom_line(data = curve, aes(x,y),linetype = 1)+
  labs(title = "Fitting Lines: Speed v.s. Distance",
       subtitle = sub1.text,
       caption = sub2.text,
       x = "Speed, mph",
       y = "Distance, ft")+
  ylim(0,125)+
  xlim(0,30)+
  scale_y_continuous(breaks = seq(0, 125, 25))
