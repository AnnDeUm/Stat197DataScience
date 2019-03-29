setwd("C:/Users/Ann Umadhay/Documents/UP ANN S14/4th year 2nd sem/Stat197/2nd activity/Ann Umadhay - specdata.zip") ## Setting working directory to the directory containing the specdata folder


## Creating a user-defined function to read a directory full of files and reports the number of cases with complete observations in each data file
complete <- function(directory, id = 1:332) {
    folderloc <- paste0(getwd(), "/", directory) ## Vector to store the location of the folder "specdata" the user would want to access
    dframe <- data.frame() ## Creating an empty data frame to contain the information needed in the output
    
    for (i in id) {
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
        completeobs <- sum(complete.cases(rwdata))         ## Computing the total number of observations without any NAs
        newdata <- data.frame(id = i, nobs = completeobs)  ## Creating a data frame with id and nobs as the column header with observations user-input file i and its equivalent computed total number of complete observations 
        dframe <- rbind(dframe, newdata)                   ## Binding the two data frames by row to avoid the loss of data of the previously obtained information and to produce a new one with the results needed in the output
    }
    return(dframe)
}



## Demo
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
