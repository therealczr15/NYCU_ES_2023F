install.packages("rio")
library(rio)
getwd()

rio.txt <- import("C0S700_0708.csv")
head(rio.txt)

rain <- rio.txt$`precipitation(mm)`
max.index <- which(rain == max(rain))

maxRain <- rio.txt$`precipitation(mm)`[max.index]
maxDate <- rio.txt$Date[max.index]
maxHour <- rio.txt$hour[max.index]
maxWind <- rio.txt$`wind_speed(m/s)`[max.index]

cat("The data number:", length(max.index), file = "Output_rain.txt", sep = '\t', '\n')
cat("max rainfall per 1 hour is:", maxRain, "mm", file = "Output_rain.txt", sep = '\t', '\n', append = TRUE )
cat("Occurrence date and hour:", maxDate, maxHour, file = "Output_rain.txt", sep = '\t', '\n', append = TRUE )
cat("Wind speed (m/s):", maxWind, "m/s", file = "Output_rain.txt", sep = '\t', '\n', append = TRUE )
