rankall <- function(outcome, num="best"){    
    ##Read coutcome data
    csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # split w.r.t. state
    stateList <- split(csv, csv$State)
    # get the list of state names
    stateNames <- names(stateList)
    # mapply
    hospital <- mapply(rankhospital, stateNames, outcome, num)
    #create data frames
    df <- as.data.frame(hospital)
    #create state column
    df["state"] = stateNames
    
    df
}