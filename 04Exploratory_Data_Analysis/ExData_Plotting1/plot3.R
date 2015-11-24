source("loadData.R")

dev.copy(png,"plot3.png",width=480,height=480)

plot(house_consumption$Datetime, house_consumption$Sub_metering_1,
     type="l", xlab="", ylab="Energy sub metering",
     cex.lab=0.75, cex.axis=0.75)

lines(house_consumption$Datetime, house_consumption$Sub_metering_2, col="red")

lines(house_consumption$Datetime, house_consumption$Sub_metering_3, col="blue")

legend("topright", legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       col=c("black", "red", "blue"), pch="-", cex=0.75)

dev.off()