source("loadData.R")

par(mfrow=c(2,2))

dev.copy(png,"plot4.png",width=480,height=480)

plot(house_consumption$Datetime,house_consumption$Global_active_power,type="l",
     xlab = "", ylab = "Global Active Power",
     cex.lab=0.75, cex.axis=0.75)

plot(house_consumption$Datetime, house_consumption$Voltage,
     type="l", xlab="datetime", ylab="Voltage",
     cex.lab=0.75, cex.axis=0.75)

plot(house_consumption$Datetime, house_consumption$Sub_metering_1,
     type="l", xlab="", ylab="Energy sub metering",
     cex.lab=0.75, cex.axis=0.75)
lines(house_consumption$Datetime,house_consumption$Sub_metering_2, col="red")
lines(house_consumption$Datetime,house_consumption$Sub_metering_3, col="blue")
legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black", "red", "blue"),
       pch="-", bty='n', cex=0.75)

plot(house_consumption$Datetime,house_consumption$Global_reactive_power,
     type="l", xlab="datetime", ylab="Global_reactive_power",
     cex.lab=0.75, cex.axis=0.75)

dev.off()

par(mfrow=c(1,1))