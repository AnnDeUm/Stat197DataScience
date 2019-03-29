setwd("C:/Users/Ann Umadhay/Documents/UP ANN S14/4th year 2nd sem/Stat197/2nd activity/Ann Umadhay - specdata.zip") ## Setting working directory to the directory containing the specdata folder

## Creating a user-defined function which calculates the mean of pollutants across the list of monitors
pollutantmean <- function(directory, pollutant, id = 1:332) {
    folderloc <- paste0(getwd(), "/", directory) ## Vector to store the location of the folder "specdata" the user would want to access
    dframe <- data.frame() ## Creating an empty data frame to contain the information needed to calculate the mean
   
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
        dframe <- rbind(dframe, rwdata)                                        ## Binding the data frames by row to avoid loss of any data produced in the iteration
    }
    dframean <- mean(dframe[ , pollutant], na.rm = TRUE)      ## The mean of the data was calculated in the data frame now specified by column depending on what kind of pollutant the user wanted to have the mean solved 
                                                              ## Concerning the number of observations, those rows with NAs were removed from the total number of observations
    return(dframean)
}
    
    

## Demo

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
