## Reads the data from dataset, filters by data and merges date and time to have it together.
readDataPlot4 <- function() {
  library(lubridate)
  library(dplyr)
  data <- read.csv("household_power_consumption.txt", sep = ";")
  start <- ymd("2007-02-01")
  end <- ymd("2007-02-02")
  data %>% filter(dmy(Date) >= start, dmy(Date) <= end) %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) 
} 

# makes 4 plots in the same graph
makePlot4 <- function(data) {
  png(file="plot4.png", width=480, height=480)
  par(mfrow = c(2,2))
  par(cex.lab = 0.8)
  par(mar = c(4,4,1,1))
  makeFirstPlot(data)
  makeSecondPlot(data)
  makeThirdPlot(data)
  makeFourthPlot(data)
  dev.off()
}

# makes global active power line plot which is the same as the one plot2.R
makeFirstPlot <- function(data) {
  active_power = as.numeric(data$Global_active_power)
  plot(data$DateTime, active_power, type="l", 
       xlab = "", ylab = "Global Active Power")
}

# makes voltage line plot
makeSecondPlot <- function(data) {
  voltage = as.numeric(data$Voltage)
  plot(data$DateTime, voltage, type="l", 
       xlab = "datetime", ylab = "Voltage")
}

# Makes third plot, which is the same as plot3.R
makeThirdPlot <- function(data) {
  with(data, 
       plot(DateTime, Sub_metering_1, 
            type="l", xlab = "", ylab = "Energy sub metering"))
  with(data, lines(DateTime, Sub_metering_2 , type = "l", col = "red"))
  with(data, lines(DateTime, Sub_metering_3 , type = "l", col = "blue"))
  legend("topright",
         col=c("black", "red","blue"),
         legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
         lty = 1, cex = 0.5)
}

# makes global reactive power line plot
makeFourthPlot <- function(data) {
  reactive_power = as.numeric(data$Global_reactive_power)
  plot(data$DateTime, reactive_power, type="l", 
       xlab = "datetime", ylab = "Global_reactive_power")
}
