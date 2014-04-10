corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        fl <- list.files(directory)
        result <- c()
        # scan thru the list
        
        for (i in 1:length(fl[id])){        
            #get fn
            fn <- (fl[id])[i]

            #generate the path
            fp <- paste(directory, fn, sep="/")
            #read the csv file
            df <- read.csv(fp, header=TRUE)
              
            #check how many complete case 
            df <- df[!is.na(df$sulfate)&!is.na(df$nitrate), ]

            if (nrow(df)> threshold){  
                #append the result              
                result <- append(result, cor(df$sulfate, df$nitrate))
            }
        }

        result
}

## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.0758
#cr <- corr("specdata", 150)
#head(cr)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
#summary(cr)

## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
#cr <- corr("specdata", 400)
#head(cr)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
#summary(cr)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
#cr <- corr("specdata", 5000)
#summary(cr)

## [1] 0
#length(cr)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
#cr <- corr("specdata")
#summary(cr)

## [1] 323
#length(cr)