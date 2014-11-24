buildStateCode <- function(){
  
  state_code <- c("AL", "AK", "AZ", "AR", "CA",
                  "CO", "CT", "DE", "FL", "GA",
                  "HI", "ID", "IL", "IN", "IA",
                  "KS", "KY", "LA", "ME", "MD",
                  "MA", "MI", "MN", "MS", "MO",
                  "MT", "NE", "NV", "NH", "NJ",
                  "NM", "NY", "NC", "ND", "OH",
                  "OK", "OR", "PA", "RI", "SC",
                  "SD", "TN", "TX", "UT", "VT",
                  "VA", "WA", "WV", "WI", "WY",
                  "AS", "DC", "FM", "GU", "MH",
                  "MP", "PW", "PR", "VI")
  
  return(state_code)

}

checkStateCode <- function(state_code){
  valid_state <- buildStateCode()
  dim <- length(valid_state)
  for (x in valid_state ){
    if (x == state_code)
      return(TRUE)
  }
  
  return(FALSE)
}