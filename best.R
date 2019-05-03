library(lattice)

Init <- function(fileStr, workDirStr = "C:/Users/Ann Umadhay/Documents/UP ANN S14/4th year 2nd sem/Stat197/3rd activity/Ann Umadhay - CourseProject3-data") {
 setwd(workDirStr)
 retDfr <- read.csv(fileStr, colClasses = "character")
 return(retDfr)
}

best <- function(stateChr, outcomeChr) {
 outcomeDfr <- Init("C:/Users/Ann Umadhay/Documents/UP ANN S14/4th year 2nd sem/Stat197/3rd activity/Ann Umadhay - CourseProject3-data/outcome-of-care-measures.csv")

 suppressWarnings(outcomeDfr[, 11] <- as.numeric(outcomeDfr[, 11])) #Hospital 30-Day Death (Mortality) Rates from Heart Attack
 suppressWarnings(outcomeDfr[, 17] <- as.numeric(outcomeDfr[, 17])) #Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack
 suppressWarnings(outcomeDfr[, 23] <- as.numeric(outcomeDfr[, 23])) #Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure

 tableDfr <- data.frame(State = names(tapply(outcomeDfr$State, outcomeDfr$State, 
                                             length)), Freq = tapply(outcomeDfr$State, outcomeDfr$State, length))
 rownames(tableDfr) <- NULL
 inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                        Col = c(11, 17, 23))
 
 if (nrow(tableDfr[tableDfr$State == stateChr, ]) == 0) 
  stop("invalid state")
 if (nrow(inputDfr[inputDfr$Outcome == outcomeChr, ]) == 0) 
  stop("invalid outcome")
 # --- Return hospital name in that state with lowest THIRTY(30)-day death
 # rate Create a data frame with given ONE (1) state Determine the relevant
 # row and column
 stateDfr <- outcomeDfr[outcomeDfr$State == stateChr, ]
 colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
 rowNum <- which.min(stateDfr[, colNum])
 return(stateDfr[rowNum, ]$Hospital.Name)
}

best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("NY", "heart attack")
