setwd("D:\\users\\hrm\\Coursera_DataScience_R_programming\\code")
source("data-utils.R")
source("tester.R")
source("stateCode.R")
#
# Function: rankhospital
# 
# Input: (1) State code where hospital is located; (2) Outcome for measurement; (3) num = "best", or the rank number
# Output: List of hospitals that have the  outcome that meets the criteria
# Description: 
# The function reads the outcome-of-care-measures.csv le and returns a character vector
# with the name of the hospitals that meet the mortality for the specied outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of "heart attack", "heart failure", or "pneumonia".
# $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 
# $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
# $ Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia 
#
rankhospital <- function(state_id, outcome, num = "best"){
  
  checkErrors(state_id,outcome)
  
  filename <- ".\\outcome-of-care-measures.csv"
  
  desired_outcome <- c()
  if (outcome == "heart attack"){
    if( num == "best"){
      desired_outcome <- c(desired_outcome, test_hosp_by_state_heart_attack(filename, state_id, -1))
    } else if( num == "worst"){
      desired_outcome <- c(desired_outcome, test_hosp_by_state_heart_attack(filename, state_id, -2))
    } else {
      desired_outcome <- c(desired_outcome, test_hosp_by_state_heart_attack(filename, state_id, num))
    }
    
  }
  
  return(desired_outcome)
  
}