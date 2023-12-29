setwd("/Users/nextbee/Desktop/R GCockrum")

tempdata <- read.csv("/Users/nextbee/Desktop/R GCockrum/Land and Sea Temperature.csv",
                     skip = 1, na.strings = "***")

tempdata$Jan <- ts(tempdata$Jan, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$DJF <- ts(tempdata$DJF, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$MAM <- ts(tempdata$MAM, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$JJA <- ts(tempdata$JJA, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$SON <- ts(tempdata$SON, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$J.D <- ts(tempdata$J.D, 
                   start = c(1880), end = c(2022), frequency = 1) 


# Excersise 2

# Set line width and colour
plot(tempdata$Jan, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

# Add a title
title("Average temperature anomaly in January in the northern hemisphere (1880-2022)")

# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)

# Add a label to the horizontal line
text(2000, -0.25, "1951-1980 average") 


# Excersise 3

tempdata$DJF <- ts(tempdata$DJF,
              start = c(1880), end = c(2022), frequency = 1)
plot(tempdata$DJF, type = "l", col = "blue", lwd = 2,
     ylab = "Seasonal temperature anomalies", xlab = "Year")
# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)
title("Average temperature anomaly for \n December, January, and February (1880-2022)")


tempdata$MAM <- ts(tempdata$MAM,
              start = c(1880), end = c(2022), frequency = 1)
plot(tempdata$MAM, type = "l", col = "blue", lwd = 2,
     ylab = "Seasonal temperature anomalies", xlab = "Year")
# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)
title("Average temperature anomaly for \n March, April, and May (1880-2022)")


tempdata$JJA <- ts(tempdata$JJA,
                   start = c(1880), end = c(2022), frequency = 1)
plot(tempdata$JJA, type = "l", col = "blue", lwd = 2,
     ylab = "Seasonal temperature anomalies", xlab = "Year")
# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)
title("Average temperature anomaly for \n June, July, and August (1880-2022)")


tempdata$SON <- ts(tempdata$SON,
              start = c(1880), end = c(2022), frequency = 1)
plot(tempdata$SON, type = "l", col = "blue", lwd = 2,
     ylab = "Seasonal temperature anomalies", xlab = "Year")
# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)
title("Average temperature anomaly for \n September, October, and November (1880-2022)")


# Excersise 4

# Set line width and colour
plot(tempdata$J.D, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

# \n creates a line break
title("Average annual temperature anomaly \n in the northern hemisphere (1880-2022)")

# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)

# Add a label to the horizontal line
text(2000, -0.1, "1951-1980 average")



# Section 1.2


tempdata$Jun <- ts(tempdata$Jun, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$Jul <- ts(tempdata$Jul, 
                   start = c(1880), end = c(2022), frequency = 1) 
tempdata$Aug <- ts(tempdata$Aug, 
                   start = c(1880), end = c(2022), frequency = 1) 


subsetDataJ1  <- window(tempdata$Jun, start=c(1951), end=c(1980))
subsetDataJ2  <- window(tempdata$Jul, start=c(1951), end=c(1980))
subsetDataJ3  <- window(tempdata$Aug, start=c(1951), end=c(1980))

summerData <- c(subsetDataJ1, subsetDataJ2, subsetDataJ3)
breaks <- seq(-0.3, 1.05, 0.05)

x <- cut(summerData, breaks=breaks)
df <- data.frame(table(x))
df

hist(summerData, breaks=seq(-0.3, 1.05, by=0.05), xlab = "Range of Temperature anomaly", main = "Histogram of JJA Data 1951 - 1980")




subsetDataJ1V2  <- window(tempdata$Jun, start=c(1981), end=c(2010))
subsetDataJ2V2  <- window(tempdata$Jul, start=c(1981), end=c(2010))
subsetDataJ3V2  <- window(tempdata$Aug, start=c(1981), end=c(2010))

summerDataV2 <- c(subsetDataJ1V2, subsetDataJ2V2, subsetDataJ3V2)
breaksV2 <- seq(-0.3, 1.05, 0.05)

x <- cut(summerDataV2, breaks=breaksV2)
df <- data.frame(table(x))
df

hist(summerDataV2, breaks=seq(-0.3, 1.05, by=0.05), xlab = "Range of Temperature anomaly", main = "Histogram of JJA Data 1980 - 2010")






tempdata$Jan <- ts(tempdata$Jan,
                   start = c(1880), end = c(2022), frequency = 1)

tempdata$DJF <- ts(tempdata$DJF,
                   start = c(1880), end = c(2022), frequency = 1)

tempdata$MAM <- ts(tempdata$MAM,
                   start = c(1880), end = c(2022), frequency = 1)

tempdata$JJA <- ts(tempdata$JJA,
                   start = c(1880), end = c(2022), frequency = 1)

tempdata$SON <- ts(tempdata$SON,
                   start = c(1880), end = c(2022), frequency = 1)

tempdata$J.D <- ts(tempdata$J.D,
                   start = c(1880), end = c(2022), frequency = 1)


plot(tempdata$Jan, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

title("Average temperature anomaly in January in the northern hemisphere (1880-2022)")

plot(tempdata$J.D, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies (Celsius)", xlab = "Year")

abline(h = 0, col = "orange", lwd = 2)
text(2000, -0.1, "1951-1980 average")


title("Average annual temperature anomaly \n in the northern hemisphere (1880-2022)")
abline(h = 0, col = "orange", lwd = 2)
text(2000, -0.1, "1951-1980 average")



tempdata$Period <- 
  factor(NA, levels = 
           c("1921-1950", "1951-1980", "1981-2010"), 
         ordered = TRUE)

def <- data.frame(tempdata)
def

tempdata$Period[(tempdata$Year > 1920) &
                  (tempdata$Year < 1951)] <- "1921-1950"
tempdata$Period[(tempdata$Year > 1950) &
                  (tempdata$Year < 1981)] <- "1951-1980"
tempdata$Period[(tempdata$Year > 1980) &
                  (tempdata$Year < 2011)] <- "1981-2010"

# Combine the temperature data for June, July, and August
temp_summer <- c(tempdata$Jun, tempdata$Jul, tempdata$Aug)

def <- data.frame(temp_summer)
def

# Mirror the Period information for temp_sum
temp_Period <- 
  c(tempdata$Period, tempdata$Period, tempdata$Period)

# Repopulate the factor information 
temp_Period <- factor(temp_Period, 
                      levels = levels(tempdata$Period), 
                      labels = levels(tempdata$Period))
def <- data.frame(temp_Period)
def

hist(temp_summer[(temp_Period == "1951-1980")], 
     plot = FALSE)

# Load the library we use for the following command.
library(mosaic)

histogram(~ temp_summer | temp_Period, type = "count", 
          breaks = seq(-0.5, 1.3, 0.10), 
          main = "Histogram of Temperature anomalies", 
          xlab = "Summer temperature distribution")



# Select years 1951 to 1980
temp_all_months <- subset(tempdata, 
                          (Year >= 1951 & Year <= 1980))

# Columns 2 to 13 contain months Jan to Dec.
temp_51to80 <- unlist(temp_all_months[, 2:13])

# c(0.3, 0.7) indicates the chosen percentiles.
perc <- quantile(temp_51to80, c(0.3, 0.7))   

perc[1]
perc[2]

sum(temp_51to80<perc[1])/length(temp_51to80)
sum(temp_51to80>perc[2])/length(temp_51to80)

# Select years 1981 to 2010
temp_all_months <- subset(tempdata, 
                          (Year >= 1981 & Year <= 2010))

# Columns 2 to 13 contain months Jan to Dec.
temp_81to10 <- unlist(temp_all_months[, 2:13])

perc2 <- quantile(temp_51to80, c(0.3, 0.7))   


sum(temp_81to10<perc[1])/length(temp_81to10)
sum(temp_81to10>perc[2])/length(temp_81to10)


paste("Proportion smaller than p30")

mean(temp_81to10 < perc[1])

paste("Proportion larger than p70")

mean(temp_81to10 > perc[2])


paste("Mean of DJF temperature anomalies across periods")

mean(~DJF|Period,data = tempdata)


paste("Mean of MAM temperature anomalies across periods")

mean(~MAM|Period,data = tempdata)


paste("Mean of JJA temperature anomalies across periods")

mean(~JJA|Period,data = tempdata)

paste("Mean of SON temperature anomalies across periods")

mean(~SON|Period,data = tempdata)


paste("Variance of DJF anomalies across periods")

var(~DJF|Period,data = tempdata)


paste("Variance of MAM anomalies across periods")
var(~MAM|Period,data = tempdata)

paste("Variance of JJA anomalies across periods")
var(~JJA|Period,data = tempdata)

paste("Variance of SON anomalies across periods")
var(~SON|Period,data = tempdata)


plot(tempdata$DJF, type = "l", col = "blue", lwd = 2,
     ylab = "Annual temperature anomalies", xlab = "Year")

# \n creates a line break
title("Average temperature anomaly in DJF and JJA \n in the northern hemisphere (1880-2022)")

# Add a horizontal line (at y = 0)
abline(h = 0, col = "darkorange2", lwd = 2)
lines(tempdata$JJA, col = "darkgreen", lwd = 2) 

# Add a label to the horizontal line
text(1895, 0.1, "1951-1980 average")
legend(1880, 1.5, legend = c("DJF", "JJA"),
       col = c("blue", "darkgreen"), 
       lty = 1, cex = 0.8, lwd = 2)

# Section 1.3

CO2data <- read.csv("/Users/nextbee/Desktop/R GCockrum/Interpolated and Trend CO2.csv")


plot(CO2data$Interpolated, type = "l", col = "blue", lwd = 2,
     ylab = "Monthly mean CO2 level(mole fraction)", xlab = "Year")

# \n creates a line break
title("Trend and interpolated monthly mean CO2 (mole fraction)")


lines(CO2data$Trend, col = "orange", lwd = 2) 

legend(x = "topleft", legend = c("Interpolated", "Trend"),
       col = c("blue", "orange"), 
       lty = 1, cex = 0.8, lwd = 2)


CO2data_june <- CO2data[CO2data$Month == 6,]

names(CO2data)[1] <- "Year"
tempCO2data <- merge(tempdata, CO2data_june)


head(tempCO2data[, c("Year", "Jun", "Trend")])


plot(tempCO2data$Jun, tempCO2data$Trend, 
     xlab = "Temperature anomaly (degrees Celsius)", 
     ylab = "CO2 levels (trend, mole fraction)", 
     pch = 16, col = "blue")

title("Scatterplot for CO2 emissions and temperature anomalies June")

cor(tempCO2data$Jun, tempCO2data$Trend)

tempCO2data$Jun <- ts(tempCO2data$Jun, 
                      start = c(1958), end = c(2017), frequency = 1) 
tempCO2data$Trend <- ts(tempCO2data$Trend, 
                        start = c(1958), end = c(2017), frequency = 1) 

plot(tempCO2data$Jun, type = "l", col = "blue", lwd = 2,
     ylab = "June temperature anomalies", xlab = "Year")

title("June temperature anomalies and CO2 emissions")  


# Create extra margins used for the second axis
par(mar = c(5, 5, 2, 5))

plot(tempCO2data$Jun, type = "l", col = "blue", lwd = 2,
     ylab = "June temperature anomalies", xlab = "Year")

title("June temperature anomalies and CO2 emissions")  

# This puts the next plot into the same picture.
par(new = T)

# No axis, no labels
plot(tempCO2data$Trend, pch = 16, lwd = 2, 
     axes = FALSE, xlab = NA, ylab = NA, cex = 1.2) 
axis(side = 4)
mtext(side = 4, line = 3, 'CO2 emissions')

legend("topleft", legend = c("June temp anom", "CO2 emis"),
       lty = c(1, 1), col = c("blue", "black"), lwd = 2)



# Adding the CO2 trend data for two months



CO2data_January <- CO2data[CO2data$Month == 1,]

names(CO2data)[1] <- "Year"
tempCO2data <- merge(tempdata, CO2data_January)


head(tempCO2data[, c("Year", "Jan", "Trend")])


plot(tempCO2data$Jan, tempCO2data$Trend, 
     xlab = "Temperature anomaly (degrees Celsius)", 
     ylab = "CO2 levels (trend, mole fraction)", 
     pch = 16, col = "blue")

title("Scatterplot for CO2 emissions and temperature anomalies -  January")

cor(tempCO2data$Jan, tempCO2data$Trend)



CO2data_December <- CO2data[CO2data$Month == 12,]

names(CO2data)[1] <- "Year"
tempCO2data <- merge(tempdata, CO2data_December)


head(tempCO2data[, c("Year", "Dec", "Trend")])


plot(tempCO2data$Dec, tempCO2data$Trend, 
     xlab = "Temperature anomaly (degrees Celsius)", 
     ylab = "CO2 levels (trend, mole fraction)", 
     pch = 16, col = "blue")

title("Scatterplot for CO2 emissions and temperature anomalies - December")

cor(tempCO2data$Dec, tempCO2data$Trend)





