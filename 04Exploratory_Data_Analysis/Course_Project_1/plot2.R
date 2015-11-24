source("loadData.R")

dev.copy(png,"plot2.png",width=480,height=480)

plot(house_consumption$Datetime,house_consumption$Global_active_power,type="l",
     xlab = "", ylab = "Global Active Power (kilowatts)",
     cex.lab=0.75, cex.axis=0.75)

dev.off()