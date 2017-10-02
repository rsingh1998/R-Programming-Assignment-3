#Plot the 30-day mortality rates for heart attack

outcome <- read.csv("outcome-of-care-measures.csv", TRUE, ",", , , colClasses = "character")
head(outcome)

outcome[,11] <- as.numeric(outcome[,11]) #We introduced the data as character
hist(outcome[,11])


#Finding the best hospital in a state (11, 17, 23)

best <- function (state = character, disease = character) {
    
    #Read outcome data
    hospitalData <- read.csv("hospital-data.csv", TRUE, ",", , , colClasses = "character", na.strings = c("NA", "Not Available"))
    outcome <- read.csv("outcome-of-care-measures.csv", TRUE, ",", , , colClasses = "character", na.strings = c("NA", "Not Available"))
    viableDiseases <- list(x = "heart attack", y = "heart failure", z = "pneumonia")
    
    #Check if the state and outcome are valid
    if (!state %in% hospitalData$State | !disease %in% viableDiseases) {
        return(message("Invalid state or outcome"))
    }
    
    #Return hospital name in that state with lowest mortality for disease
    if (disease == viableDiseases$x){
        outcome2 <- outcome[outcome$State == state,]
        minimum <- min(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm = T)
        result <- outcome2[outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minimum, 2]
        x <- complete.cases(result)
        return (result[x])
    }
    
    if (disease == viableDiseases$y){
        outcome2 <- outcome[outcome$State == state,]
        minimum <- min(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.rm = T)
        result <- outcome2[outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minimum, 2]
        x <- complete.cases(result)
        return (result[x])
    }
    
    if (disease == viableDiseases$z){
        outcome2 <- outcome[outcome$State == state,]
        minimum <- min(outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.rm = T)
        result <- outcome2[outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minimum, 2]
        x <- complete.cases(result)
        return (result[x])
    }
}


#Ranking hospitals by state

rankhospital <- function (state = character, disease = character, rank) {
    
    #Read outcome data
    hospitalData <- read.csv("hospital-data.csv", TRUE, ",", , , colClasses = "character", na.strings = c("NA", "Not Available"))
    outcome <- read.csv("outcome-of-care-measures.csv", TRUE, ",", , , colClasses = "character", na.strings = c("NA", "Not Available"))
    viableDiseases <- list(x = "heart attack", y = "heart failure", z = "pneumonia")
    
    #Check if the state and outcome are valid
    if (!state %in% hospitalData$State | !disease %in% viableDiseases) {
        return(message("Invalid state or outcome"))
    }
    
    #Creating rank
    if (disease == viableDiseases$x){
        outcome2 <- outcome[outcome$State == state,]
        outcome2 <- na.omit(outcome2)
        rankx <- data.frame(Hospital.name = outcome2$Hospital.Name, Rate = outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Rank = 1)
        rankx <- rankx[order(rankx$Rate),]
        rankx$Rank <- 1:length(rankx[,1])
        
        if (rank == "best") {
            rank <- 1
        } else if (rank == "worst") {
            rank <- length(rankx)
        }
        
        return(rankx[rankx$Rank == rank, 1])
    }
    
    if (disease == viableDiseases$y){
        outcome2 <- outcome[outcome$State == state,]
        outcome2 <- na.omit(outcome2)
        rankx <- data.frame(Hospital.name = outcome2$Hospital.Name, Rate = outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Rank = 1)
        rankx <- rankx[order(rankx$Rate),]
        rankx$Rank <- 1:length(rankx[,1])
        
        if (rank == "best") {
            rank <- 1
        } else if (rank == "worst") {
            rank <- length(rankx)
        }
        
        return(rankx[rankx$Rank == rank, 1])
    }
    
    if (disease == viableDiseases$z){
        outcome2 <- outcome[outcome$State == state,]
        outcome2 <- na.omit(outcome2)
        rankx <- data.frame(Hospital.name = outcome2$Hospital.Name, Rate = outcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Rank = 1)
        rankx <- rankx[order(rankx$Rate),]
        rankx$Rank <- 1:length(rankx[,1])
        
        if (rank == "best") {
            rank <- 1
        } else if (rank == "worst") {
            rank <- length(rankx)
        }
        
        return(rankx[rankx$Rank == rank, 1])
    }
    
}
