set.seed(1)
math.score <- sample(0:100,120,replace = TRUE)
math.score

#(a)
mean(math.score)
sd(math.score)

#(b)
pass <- math.score[math.score >= 60]
length(pass)
pass.index <- which(math.score >= 60)
pass.index

#(c)
max(math.score)
min(math.score)
max.index <- which(math.score == max(math.score))
max.index
min.index <- which(math.score == min(math.score))
min.index

#(d)
decrease <- sort(math.score, decreasing = TRUE)
topTen <- decrease[1:10]
topTen
mean(topTen)
sd(topTen)

#(e)
s <- summary(math.score)
s["1st Qu."]
