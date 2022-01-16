## Reads the data from dataset, filters by data and merges date and time to have it together.
## Then selectes submetering values
readDataPlot3 <- function() {
  library(lubridate)
  library(dplyr)
  data <- read.csv("household_power_consumption.txt", sep = ";")
  start <- ymd("2007-02-01")
  end <- ymd("2007-02-02")
  data %>% filter(dmy(Date) >= start, dmy(Date) <= end) %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>% 
    select(DateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3)
} 

#makes line plot with legend
makePlot3 <- function(data) {
  png(file="plot3.png", width=480, height=480)
  with(data, 
       plot(DateTime, Sub_metering_1, 
            type="l", xlab = "", ylab = "Energy sub metering"))
  with(data, lines(DateTime, Sub_metering_2 , type = "l", col = "red"))
  with(data, lines(DateTime, Sub_metering_3 , type = "l", col = "blue"))
  legend("topright",
         col=c("black", "red","blue"),
         legend=c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
         lty = 1)
  dev.off()
}