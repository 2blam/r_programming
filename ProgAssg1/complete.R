complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

         # get file list of directory
        fl <- list.files(directory)
        result <-data.frame(matrix(0, nrow=length(id), ncol=2))
        colnames(result)  <- c("id", "nobs")
        # scan thru the list
        
        for (i in 1:length(fl[id])){        
            #get fn
            fn <- (fl[id])[i]

        	#generate the path
        	fp <- paste(directory, fn, sep="/")
        	#read the csv file
        	df <- read.csv(fp, header=TRUE)
        	#get the id
            id_val <- df[1, 4]
            
            #check how many complete case   
            nobs <- nrow(df[!is.na(df$sulfate)&!is.na(df$nitrate), ])
            result[i,] <- c(id_val, nobs)
        }

        result
        
        
}

##   id nobs
## 1  1  117
#complete("specdata", 1)
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
#complete("specdata", c(2, 4, 8, 10, 12))

##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
#complete("specdata", 30:25)

##   id nobs
## 1  3  243
#complete("specdata", 3)