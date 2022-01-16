## Reads the data from dataset, filters by date
readDataPlot1 <- function() {
  library(lubridate)
  library(dplyr)
  data <- read.csv("household_power_consumption.txt", sep = ";")
  start <- ymd("2007-02-01")
  end <- ymd("2007-02-02")
  filter(data, dmy(Date) >= start, dmy(Date) <= end)
} 

#makes histogram Plot
makePlot1 <- function(data) {
  png(file="plot1.png", width=480, height=480)
  active_power = as.numeric(data$Global_active_power)
  hist(active_power[!is.na(active_power)], col="red", main = "Global Active Power", 
       xlab = "Global Active Power (kilowatts)")
  dev.off()
}