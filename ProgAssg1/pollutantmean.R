pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

        # get file list of directory
        fl <- list.files(directory)
        s <- 0
        totalRecord <-0

        # scan thru the list
        for (fn in fl[id]){
        	#generate the path
        	fp <- paste(directory, fn, sep="/")
        	#read the csv file
        	df <- read.csv(fp, header=TRUE)
        	#get the value
        	col <- df[pollutant]        	
        	s <- s + sum(col[!is.na(col)])
        	totalRecord <- totalRecord + length(col[!is.na(col)])
        }
        
        s / totalRecord
}

#pollutantmean("specdata", "sulfate", 1:10) #4.064
#pollutantmean("specdata", "nitrate", 70:72) #1.706
#pollutantmean("specdata", "nitrate", 23) #1.281