data("iris")
data = iris
index.s = which (data$Species == "setosa")
index.v = which (data$Species == "versicolor")
d.s = data$Sepal.Length[index.s]
d.v = data$Sepal.Length[index.v]

n1 <- length(d.s)
mu1 <- mean(d.s)
s1 <- sd(d.s)
se1 <- s1/sqrt(n1)

n2 <- length(d.v)
mu2 <- mean(d.v)
s2 <- sd(d.v)
se2 <- s2/sqrt(n2)

shapiro.test(d.s)
shapiro.test(d.v)

d = data[1:100,]

library(ggplot2)
ggplot(data = d, aes(Species,Sepal.Length))+
  geom_boxplot()+
  coord_flip()+
  geom_point()+
  labs(title = "Boxplot Sepal data",
       y = "Sepal Length (mm)",
       x = "Species")


library(BSDA)

z.test(d.v,d.s,
       alternative = "two.sided",
       sigma.x = s2,
       sigma.y = s1,
       mu = 0, 
       conf.level = 0.95
)
