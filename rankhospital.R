rankhospital <- function(state, outcome, num = "best"){
  #### read the file
  con <- file("outcome-of-care-measures.csv","r")
  hospData <- read.csv(con,colClasses = "character")
  close(con)
  #### Define "Best"
  if (num == "best"){
    num <- 1
  }
  
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
  
  #### Define "best" and "worst"
  
  if (outcome == "heart attack"){
    colNumber <- 11 #### 30-Day Death (Mortality) Rates from Heart Attack
  } else if(outcome == "heart failure"){
    colNumber <- 17 #### 30-Day Death (Mortality) Rates from Heart Failure
  } else {
    colNumber <- 23 #### 30-Day Death (Mortality) Rates from pneumonia
  }
  

    #### Remove NA and "Not Available"
    stateData <- stateData[!is.na(stateData[ ,colNumber]),]
    stateData <- stateData[!stateData[ ,colNumber] == "Not Available", ]
    #### Rankin the hospitals, and handling ties alephabetically
    ranked <- stateData[order(as.numeric(stateData[,colNumber]),stateData[ ,2]), ]
    ### define "Worst"
    if(num =="worst"){
      num <- nrow(ranked)
    }
    hospName <- ranked[num,2]
    hospName

}
