
# Section 2.1


Period <- seq(1, 10)

SanMateo <- c(75.0,  72.0, 67.5, 63.3, 57.8, 
                47.5, 39.5, 15.6, 5.0, 0.0)


# Put the data into a data frame
data_ex <- data.frame(Period, SanMateo)

plot(data_ex$Period, data_ex$SanMateo, 
     ylim = c(0, 100), xlab = "Period", ylab = "Average contribution",
     type = "l", col = "blue", lwd = 2)


title("Average contribution to public goods game: without punishment")

legend("bottomleft", lwd = 2, lty = 1, cex = 1.2,
       legend = "San Mateo", col = "blue")




# Section 2.2


# 1

# This package provides useful functionality later.
library(tidyverse)
library(readxl)

# Set your working directory to the correct folder.
# Insert your file path for 'YOURFILEPATH'.
setwd("/Users/nextbee/Desktop/R GCockrum")

data_N <- read_excel("Public-goods-experimental-data.xlsx",
                     range = "A2:Q12")
data_P <- read_excel("Public-goods-experimental-data.xlsx", 
                     range = "A16:Q26")


# Use a loop for data_N

data_N$meanC <- 0

for (row in 1:nrow(data_N)) {
  data_N$meanC[row] <- rowMeans(data_N[row, 2:17])
}

df <- data.frame(data_N$meanC)
df

# Use the apply function for data_P
data_P$meanC <- apply(data_P[, 2:17], 1, mean)



df <- data.frame(data_P$meanC)
df



plot(data_N$Period, data_N$meanC, type = "l",
     col = "blue", lwd = 2, xlab = "Round",
     ylim = c(4, 14), ylab = "Average contribution")



lines(data_P$meanC, col = "red", lwd = 2)

title("Average contribution to public goods game")

legend("bottomleft", lty = 1, cex = 1.2, lwd = 2,
       legend = c("Without punishment", "With punishment"),
       col = c("blue", "red"))

# 2

temp_d <- c(data_N$meanC[1], data_N$meanC[10],
            data_P$meanC[1], data_P$meanC[10])
temp <- matrix(temp_d, nrow = 2, ncol = 2, byrow = TRUE)
temp


barplot(temp, 
        main = "Mean contributions in a public goods game",
        ylab = "Contribution",
        beside = TRUE, col = c("Blue", "Red"), 
        names.arg = c("Round 1", "Round 10"))
legend("bottomleft", pch = 1, col = c("Blue", "Red"),
       c("Without punishment", "With punishment"))


#3


data_N$varC <- apply(data_N[, 2:17], 1, var)

df <- data.frame(data_N$varC)
df

data_N$sdC <- apply(data_N[, 2:17], 1, sd)

df <- data.frame(data_N$sdC)
df


data_P$varC <- apply(data_P[, 2:17], 1, var)

df <- data.frame(data_P$varC)
df

data_P$sdC <- apply(data_P[, 2:17], 1, sd)

df <- data.frame(data_P$sdC)
df


citylist <- names(data_N[2:17])

plot(data_N$Period, data_N$meanC, type = "l",
     col = "blue", lwd = 2, xlab = "Round", 
     ylim = c(0, 20), ylab = "Average contribution")

# mean + 2 sd
lines(data_N$meanC + 2 * data_N$sdC, col = "red", lwd = 2)

# mean – 2 sd
lines(data_N$meanC - 2 * data_N$sdC, col = "red", lwd = 2)

for(i in citylist) {
  points(data_N[[1]], data_N[[i]])
}

title("Contribution to public goods game without punishment")

legend("bottomleft", legend = c("Mean", "+/- 2 sd"),
       col = c("blue", "red"), lwd = 2, lty = 1, cex = 1.2)


# for the punishment
citylist <- names(data_N[2:17])

plot(data_P$Period, data_P$meanC, type = "l",
     col = "blue", xlab = "Round",
     ylim = c(0, 22), ylab = "Average contribution")

# mean + 2 sd
lines(data_P$meanC + 2 * data_P$sdC, col = "red")

# mean – 2 sd
lines(data_P$meanC - 2 * data_P$sdC, col = "red")

for(i in citylist) {
  points(data_P[[1]], data_P[[i]])
}

title("Contribution to public goods game with punishment")

legend("bottomleft", legend = c("Mean", "+/- 2 sd"),
       col = c("blue", "red"), lty = 1, cex = 1.2)


# Question 4

range(data_N[1, 2:17])
range(data_N[10, 2:17])
range(data_P[1, 2:17])
range(data_P[10, 2:17])


temp <- apply(data_N[, 2:17], 1, range)
temp


data_N$rangeC <- temp[2, ] - temp[1, ]
temp <- apply(data_P[,2:17], 1, range)
data_P$rangeC <- temp[2, ] - temp[1, ]

df <- data.frame(data_N$rangeC)
df

df <- data.frame(data_P$rangeC)
df



plot(data_N$Period, data_N$rangeC, type = "l",
     col = "blue", lwd = 2, xlab = "Round",
     ylim = c(4, 14), ylab = "Range of contributions")

lines(data_P$rangeC, col = "red", lwd = 2)

title("Range of contributions to public goods game")

legend("bottomright", lwd = 2, lty = 1, cex = 1.2,
       legend = c("Without punishment", "With punishment"),
       col = c("blue", "red"))


# Question 5:


data_N$minC <- apply(data_N[, 2:17], 1, min)
data_N$maxC <- apply(data_N[, 2:17], 1, max)
data_P$minC <- apply(data_P[, 2:17], 1, min)
data_P$maxC <- apply(data_P[, 2:17], 1, max)


print("Public goods game without punishment")
round(data_N[c(1, 10), c(1, 18:23)], digits = 2)


options(signif = 2)

print("Public goods game with punishment")
round(data_P[c(1, 10), c(1, 18:23)], digits = 2)


# 2.3

p1_N <- t(data_N[1, 2:17])
p1_P <- t(data_P[1, 2:17])
t.test(x = p1_N, y = p1_P)

p1_N <- t(data_N[1, 2:17])
p1_P <- t(data_P[1, 2:17])
t.test(x = p1_N, y = p1_P, paired = TRUE)


# test on period 10
p1_N <- t(data_N[10, 2:17])
p1_P <- t(data_P[10, 2:17])
t.test(x = p1_N, y = p1_P, paired = TRUE)


