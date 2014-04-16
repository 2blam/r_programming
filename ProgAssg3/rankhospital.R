#source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

rankhospital <- function(state, outcome, num = "best"){
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
    
    csv[, idx] <- as.numeric(csv[, idx]) #convert to numeric
    csv <- csv[!is.na(csv[, idx]), ] #remove na 
    
    #order by rate and hospital.name    
    csv <- csv[order(csv[, idx],csv[, 2]), ]
        
    #check the best or specific rank num    
    #check if there is any valid value
    if (num == "best" & nrow(csv)>0){
        csv$Hospital.Name[1] #best
    }else if (is.numeric(num) & nrow(csv) > num){            
        csv$Hospital.Name[num] #rank num
    }else if(num == "worst"){
        csv$Hospital.Name[nrow(csv)] 
    }else{
        NA
    }         
    
    
}