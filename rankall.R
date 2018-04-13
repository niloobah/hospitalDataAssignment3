rankall <- function(outcome="heart attack", num = "best") {
  source("rankhospital.R")
  #### read the file
  result=data.frame(hospital=character(), state=character())
  resultName <- names(result)
  
  con <- file("outcome-of-care-measures.csv","r")
  hospData <- read.csv(con,colClasses = "character")
  close(con)
  
  ### Check that outcome is valid:
  validOutcome <- c("heart attack", "heart failure","pneumonia")
  if(!outcome %in% validOutcome){
    stop("invalid outcome")
  }

  validState <- unique(as.character(hospData[,7]))
  validState <- validState [order(validState)]
  for (s in validState){
    hospName <- rankhospital (s, outcome, num)
    newData <- data.frame(hospital=hospName,state=s,row.names = s)
    result <- rbind(result,newData)
  }
  result
}

