#install.packages("BSDA")
library("BSDA")
library("ggplot2")

num.sample = 20
t.star = qt(0.95, df = num.sample - 1)
s.mean = c()
CIU = c()
CID = c()
color = c()
number = c(1:100)

for(i in 1:100)
{
  s.data = rnorm(20,50,10)
  s.mean[i] <- mean(s.data)
  s.sd <- sd(s.data)
  B <- t.star * s.sd / sqrt(num.sample)
  CIU[i] <- mean(s.data) + B
  CID[i] <- mean(s.data) - B
  if(CID[i] > 50 || CIU[i] < 50)
    color[i] = "red"
  else
    color[i] = "black"
}


df = data.frame(x = CID, y = CIU, z = color, n = number, m = s.mean)

pd <- position_dodge(1)
ggplot(data = df, aes(n,m)) +
  geom_errorbar(aes(ymin=x, ymax=y), color = color, width = 1) +
  geom_abline(intercept = 50, slope = 0,linetype = 2, color = "red") +
  geom_point(shape = 1, size = 1, color = color) +
  labs(title = "Understanding CL",
       x = "Number of sample",
       y = "Confidence Intervals")

