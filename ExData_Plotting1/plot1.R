library(dplyr)

# read the data
data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE)
# filter data for just two days in Feb
sdata <- filter(data, Date %in% c("1/2/2007", "2/2/2007"))
# convert column to numeric
sdata$Global_active_power <- as.numeric(sdata$Global_active_power)
# plot
hist(sdata$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
# copy to png
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
