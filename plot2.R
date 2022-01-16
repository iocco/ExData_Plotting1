## Reads the data from dataset, filters by data and merges date and time to have it together.
readDataPlot2 <- function() {
  library(lubridate)
  library(dplyr)
  data <- read.csv("household_power_consumption.txt", sep = ";")
  start <- ymd("2007-02-01")
  end <- ymd("2007-02-02")
  data %>% filter(dmy(Date) >= start, dmy(Date) <= end) %>%
    mutate(DateTime = dmy_hms(paste(Date, Time))) %>% 
    select(DateTime, Global_active_power)
} 

#makes line Plot
makePlot2 <- function(data) {
  png(file="plot2.png", width=480, height=480)
  active_power = as.numeric(data$Global_active_power)
  plot(data$DateTime, active_power, type="l", 
       xlab = "", ylab = "Global Active Power (kilowatts)")
  dev.off()
}