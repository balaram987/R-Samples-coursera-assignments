setwd("D:\\users\\hrm\\Coursera_DataScience_R_programming\\code")
source("data-utils.R")
source("tester.R")
source("stateCode.R")
#
# Function: best
# 
# Input: (1) State code where hospital is located; (2) Outcome for measurement
# Output: List of hospitals that have the best outcome
# Description: 
# The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia".
# $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
# $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
# $ Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia 
#
best <- function(state_id, outcome){
  my_debug <- FALSE 
  
 
  
  if( outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia"){
    #    print("OK")
  } else {
    prt <- paste("Error in best","(", state_id, ",", outcome, ")", sep="")
    # print(prt)
    stop("invalid outcome")
  }
  
  # Generate valid states ID vectors
  valid_state <- buildStateCode()
  # check if the the passed in state is a member of the valid_state
  if ( checkStateCode(state_id) ) {
    
  } else {
    prt <- paste("Error in best","(", state_id, ",", outcome, ")", sep="")
    stop("invalid state")
  }
  
  
  filename <- ".\\outcome-of-care-measures.csv"
  lowest_outcome <- c()
 
    
  if (outcome == "heart attack"){
    lowest_outcome <- c(lowest_outcome, test_hosp_by_state_heart_attack(filename, state_id) )
    
    
    if( my_debug == TRUE ){
       lowest_outcome <- c(test_hosp_by_state_heart_attack(".\\smallset-hospital-data.csv", "AL") )
       lowest_outcome <- sort(lowest_outcome)
       print(lowest_outcome)
       str(lowest_outcome)
    }

  }
   
  
 if( outcome == "heart failure") {
   lowest_outcome <- c(lowest_outcome, test_hosp_by_state_heart_failure(filename, state_id))
 }
    
  
 if (outcome == "pneumonia") {
   lowest_outcome <- c(lowest_outcome, test_hosp_by_state_pneumonia(filename, state_id))
 }
    
  return(lowest_outcome)
    
}

