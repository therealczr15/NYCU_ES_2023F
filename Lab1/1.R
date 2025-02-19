# create 100 random repeatble number
x <- sample(x = 1:100, size = 100, replace = TRUE)
x

# compute mean, variance, standard deviation
mean(x)
var(x)
sd(x)

# replace 20 numbers by not a number (NAN)
y <- x
y[sample(x = 1:100, size = 20, replace = FALSE)] <- NaN
y
mean(y)
mean(y, na.rm = TRUE)