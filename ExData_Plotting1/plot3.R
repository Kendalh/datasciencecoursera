library(dplyr)
library(lubridate)

# read the data
data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
# filter data for just two days in Feb
sdata <- filter(data, Date %in% c("1/2/2007", "2/2/2007"))

# convert column to numeric
sdata$Global_active_power <- as.numeric(sdata$Global_active_power)
sdata$Sub_metering_1 <- as.numeric(sdata$Sub_metering_1)
sdata$Sub_metering_2 <- as.numeric(sdata$Sub_metering_2)
sdata$Sub_metering_3 <- as.numeric(sdata$Sub_metering_3)
# add a new DateTime column by concating and paste Date and Time
sdata <- mutate(sdata, DateTime = dmy_hms(paste(Date, Time, sep=" ")))

# plot
with(sdata, plot(DateTime, Sub_metering_1, col = "black", xlab="", ylab="Energy sub metering", type="l"))
with(sdata, points(DateTime, Sub_metering_2, col = "red", type="l"))
with(sdata, points(DateTime, Sub_metering_3, col = "blue", type="l"))
legend("topright", lwd=1, col = c("black","red","blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# copy to png
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()