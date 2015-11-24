rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia" ) {
        stop("invalid outcome") }
    
    ## For each state, find the hospital of the given rank
    temp_data <- data.frame(matrix(ncol=3))
    allstates <- unique(data$State)
    output <- numeric()
    result <- data.frame(Hospital.Name=character(0), State=character(0))
    for (state in allstates) {
        if (outcome == "heart attack") {
            temp_data <- data[data[,7]==state, c(2,7,11)] 
        } else if (outcome == "heart failure") {
            temp_data <- data[data[,7]==state, c(2,7,17)]
        } else if (outcome == "pneumonia") {
            temp_data <- data[data[,7]==state, c(2,7,23)] }
        temp_data[,3] <-  as.numeric(temp_data[,3])
        temp_data <- temp_data[complete.cases(temp_data),]
        temp_data <- temp_data[order(temp_data[,3],temp_data[,1]),]
        
        if (num=="best") {
            result <- rbind(result,temp_data[1,1:2])
        } else if (num=="worst") {
            result <- rbind(result,temp_data[nrow(temp_data),1:2])
        } else {
            output <- as.numeric(num)
            result <- rbind(result,temp_data[output,1:2])     
        }     
    
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    result <- result[order(result[,2]),]
    names(result) <- c("hospital","state")
    result
}
