complete <- function(directory, id = 1:332) {
    result <- data.frame(id=numeric(0), nobs=numeric(0))
    for (i in id) {
        j <- sprintf("%03d", i)
        if (i<1 || i>332) {
            next }
        df_temp <- read.csv(paste(directory,"/",j,".csv", sep=""))
        nobs_input <- sum(complete.cases(df_temp))
        temp_result <- data.frame(id=i, nobs=nobs_input)
        result <- rbind(result,temp_result)
    }
    result
}