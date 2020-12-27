library(dplyr)
library(lubridate)

# read the data
data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
# filter data for just two days in Feb
sdata <- filter(data, Date %in% c("1/2/2007", "2/2/2007"))
# convert column to numeric
sdata$Global_active_power <- as.numeric(sdata$Global_active_power)
# add a new DateTime column by concating and paste Date and Time
sdata <- mutate(sdata, DateTime = dmy_hms(paste(Date, Time, sep=" ")))
# plot
with(sdata, plot(DateTime, Global_active_power, type="l", xlab="", ylab="Global Active Power (killowatts)"))
# copy to png
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()