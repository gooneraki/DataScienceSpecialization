corr <- function(directory, threshold = 0) {
    y <- complete(directory)
    z <- numeric(0)
    for (i in y$id) {
        j <- sprintf("%03d", i)
        if ( y$nobs[i] >= threshold ){
            df_temp <- read.csv(paste(directory,"/",j,".csv", sep=""))
            z <- c(z,round(cor(df_temp$sulfate,df_temp$nitrate, use="p"),digits=4))
        }
    }
    z
}


