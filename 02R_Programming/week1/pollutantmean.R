pollutantmean <- function(directory, pollutant, id = 1:332) {
    result <- data.frame(matrix(ncol=4))
    names(result) = c("Date","sulfate","nitrate","ID")
    for (i in id) {
        j <- sprintf("%03d", i)
        if (i<1 || i>332) {
            next }
        df_temp <- read.csv(paste(directory,"/",j,".csv", sep=""))
        result <- rbind(result,df_temp) }
    if ( pollutant == "sulfate"){
        y <- result$sulfate }
    else {
        y <- result$nitrate }
    mean_y <- mean(y, na.rm = T)
    print(round(mean_y,3))
}
