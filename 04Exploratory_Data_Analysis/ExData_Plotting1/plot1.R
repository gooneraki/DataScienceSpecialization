source("loadData.R")

dev.copy(png,"plot1.png",width=480,height=480)

hist(house_consumption$Global_active_power, col="red",
     main="Global Active Power", xlab="Global Active Power (kilowatts)",
     breaks=12, cex.main=0.8, cex.lab=0.7, cex.axis=0.7)

dev.off()