rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia" ) {
        stop("invalid outcome") }
    if (!is.element(state, data$State)) {
        stop("invalid state") }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    useful_data <- data.frame(matrix(ncol=3))
    if (outcome == "heart attack") {
        useful_data <- data[data[,7]==state, c(2,7,11)] 
    } else if (outcome == "heart failure") {
        useful_data <- data[data[,7]==state, c(2,7,17)]
    } else if (outcome == "pneumonia") {
        useful_data <- data[data[,7]==state, c(2,7,23)] }
    useful_data[,3] <-  as.numeric(useful_data[,3])
    useful_data <- useful_data[complete.cases(useful_data),]
    useful_data <- useful_data[order(useful_data[,3],useful_data[,1]),]
    output <- numeric()
    if (num=="best") {
        useful_data[1,1]
    } else if (num=="worst") {
        useful_data[nrow(useful_data),1]
    } else {
        output <- as.numeric(num)
        useful_data[output,1]     
    }
   
    
}
