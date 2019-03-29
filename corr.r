setwd("C:/Users/Ann Umadhay/Documents/UP ANN S14/4th year 2nd sem/Stat197/2nd activity/Ann Umadhay - specdata.zip") ## Setting working directory to the directory containing the specdata folder


## Creating a user-defined function to take directory of data files and threshold for complete cases 
## It calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold
corr <- function(directory, threshold = 0) {
    folderloc <- paste0(getwd(), "/", directory) ## Vector to store the location of the folder "specdata" the user would want to access
    crrltion <- NULL
    
    for (i in 1:332) {
        if (i < 10) {
            rwdata <- read.csv(paste0(folderloc, "/00", as.character(i), ".csv"),  ## To access csv files with folder names 001 to 009
                               as.is = TRUE,
                               header = TRUE)  
        }
        else if (i >= 10 & i < 100) {
            rwdata <- read.csv(paste0(folderloc, "/0", as.character(i), ".csv"),   ## Accessing files with names 010 to 099
                               as.is = TRUE,
                               header = TRUE)  
            
        }
        else {
            
            rwdata <- read.csv(paste0(folderloc, "/", as.character(i), ".csv"),    ## For files with folder names > 100
                               as.is = TRUE,
                               header = TRUE)  
        }
        newdata <- rwdata[complete.cases(rwdata), ]  ## A data frame with the computed number of cases with complete obserevations as the rows for the whole specdata directory
        
        if (nrow(newdata) > threshold) {             ## Checking if number of rows are greater than the defined value of threshold, input by user
            crrltion <- c(crrltion, cor(newdata[,"sulfate"], newdata[, "nitrate"]))   ## Concatenating crrltion and computed correlation between sulfate and nitrate for monitor locations into the variable crrltion
        }
    }
    return(crrltion)
}


## Demo

cr <- corr("specdata", 150)
head(cr, 6)
summary(cr)


cr <- corr("specdata", 400)
head(cr,6)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

