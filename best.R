### Function Best takes two arguments: 2-character "State" name, and "outcom"
### returns the hospital w/ lower death rate in the outcome

#### Check if "State" in 54 valid states, otherwise STOP, error msg "invalid state"
#### Check if "Otcome" is valid, otherwise STOP, error msg "invalid outcom"
best <-function(state,outcome){
  
  #### read the file
  con <- file("outcome-of-care-measures.csv","r")
  hospData <- read.csv(con,colClasses = "character")
  close(con)
  ### Check that state is valid:
  validState <- unique(as.character(hospData[,7]))
  if(!state %in% validState){
    stop("invalid state")
  }
  ### Check that outcome is valid:
  validOutcome <- c("heart attack", "heart failure","pneumonia")
  if(!outcome %in% validOutcome){
    stop("invalid outcome")
  }
  
  ### Extract the state data
  s <- split(hospData,hospData$State)
  stateData <- s[state][[state]]
  
  if (outcome == "heart attack"){
    colNumber <- 11 #### 30-Day Death (Mortality) Rates from Heart Attack
  } else if(outcome == "heart failure"){
    colNumber <- 19 #### 30-Day Death (Mortality) Rates from Heart Failure
  } else {
    colNumber <- 23 #### 30-Day Death (Mortality) Rates from pneumonia
  }
 
    #### Remove NA and "Not Available"
    stateData <- stateData[!is.na(stateData[ ,colNumber]),]
    stateData <- stateData[!stateData[ ,colNumber] == "Not Available", ]
    
    ### find the winner
    minRate <- min(as.numeric(stateData[,colNumber]))
    winnerIndex <- as.numeric(stateData[,colNumber])==minRate
    winnerName <- min(stateData[winnerIndex,2])

    winnerName 
  
}
