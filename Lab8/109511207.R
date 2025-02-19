set.seed(1)

typhoon.sim <- function(n){
  n.strike.zero <- 0
  for(i in 1:n){
    n.strike <- 0
    for (j in 1:20){
      typhoon <- sample(0:99,1)
      if(typhoon <= 9) 
        n.strike = n.strike + 1
    }
    if(n.strike == 0) 
      n.strike.zero = n.strike.zero + 1
  }
  pro <- n.strike.zero / n
  
  return (pro)
}
  
x <- 0
y <- 0
prob <- typhoon.sim(8)
prob

for(i in 3:15){
  x[i-2] = 2^i
  y[i-2] = typhoon.sim(2^i)
}

# theoretical probability pro.t
pro.t <- 0.9^20
title <- paste("Probability of Typhoon Strike Taiwan")
lpx <- c(0,2^15)
lpy <- c(pro.t,pro.t)
data.line <- data.frame(x=lpx, y=lpy)
library(ggplot2)
data <- data.frame(x=x, y=y)
ggplot(data = data, aes(x,y)) +
  geom_point() +
  labs(title = title,
       x = "Number of Random Sampling",
       y = "Probability of n.strike = 0") +
  geom_line(data = data.line, aes(x,y),
            linetype = 2,
            color = "black",
            size = 1.) +
  ylim(0.05,0.125) +
  scale_y_continuous(breaks = seq(0.05, 0.125, 0.025))
