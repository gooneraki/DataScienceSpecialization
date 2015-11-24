#download the file if it does not yetexist in the current folder
if (!file.exists("household_power_consumption.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip")
}

#unzip the file if it does not yet exist in the current folder
if (!file.exists("household_power_consumption.txt")){
  unzip("household_power_consumption.zip")
}

#load all data
house_consumption <- read.csv2("household_power_consumption.txt", as.is=T)

#state the relevant days for this excercise
dates_selection <-  c("1/2/2007","2/2/2007")

#filter only relevant days
house_consumption <- house_consumption[as.character(house_consumption$Date) %in% dates_selection,]

#convert all numeric values of the dataframe to numeric
house_consumption[,3:9] <- sapply(house_consumption[,3:9], as.numeric)

#create a datetime column
house_consumption$Datetime <- strptime(paste(house_consumption$Date, house_consumption$Time), "%d/%m/%Y %H:%M:%S")

