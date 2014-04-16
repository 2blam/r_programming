#source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

best <- function(state, outcome){
    ##Read coutcome data
    csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
    
    ##Check that state and outcome are valid
    if (!(state %in% csv$State)){
        stop("invalid state")        
    }else{
        #filter with state records only
        csv <- csv[csv$State == state, ]        
    }
    
    validOutcome <- c("heart attack", "heart failure", "pneumonia")
    
    #check if it is valid outcome
    if (!(outcome %in% validOutcome)){        
        stop("invalid outcome")
    }else{
        #get index
        if (outcome == "heart attack"){
            idx = 11  
        }else if (outcome == "heart failure"){
            idx = 17    
        }else{
            idx = 23           
        }
    }
    
    ##Return hopsital name in that state with lowest 30-day death
    csv[, idx] <- as.numeric(csv[, idx]) #convert to numeric
    csv <- csv[!is.na(csv[, idx]), ] #remove na    
    
    minIdx <- which(csv[, idx] == min(csv[, idx]))[1] # get the 1st min Idx
    
    ##rate    
    csv$Hospital.Name[minIdx]
}