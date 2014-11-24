#
# Function: test_hosp_heart_attack
# For all states
#
#
test_hosp_heart_attack <-function(filename){
    df <- read.csv(filename)
    #  print(df)
    # Make sure the column for heart attacks is cleaned to ensure NA is added where needed
    # print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    new_col <- replaceNA(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    #print(new_col)
    # Update the read in data frame with the replaced NA cells
    df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- new_col
    #print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    # Find the clean boolean vector
    bool_heart_attack <- complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    # Using the clean boolean vector find the lowest value
    lowest_heart_attack <- min(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[bool_heart_attack])
    print("Lowest Heart Attack")
    print(lowest_heart_attack)
    
    # Create a new data frame with data which is cleaned
    complete_heart_attack_df <- data.frame(df[bool_heart_attack,])
    #print(complete_heart_attack_df)
    # Find the names of the hospitals with the lowest heart attacks from the cleaned set
    num_hospitals <- nrow(complete_heart_attack_df)
    print(num_hospitals)
    count <- num_hospitals
    i <- 1
    while (count > 0){
      
        if(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]) <= lowest_heart_attack){
          print(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]))
          print(complete_heart_attack_df$Hospital.Name[i])
        }
        i <- i + 1
        count <- count - 1
      }
}
#
# Function: test_hosp_by_state_heart_attack
# This function will find the hospital with the lowest 
# mortality rate from heart attacks
# Inputs: filename - string containing the name of the .csv file with hospital outcome of measures data
#         state - string with the 2 letter state code
#         state_rank_num - value indicating the required rank; default = -1 indicating lowest, = -2 is worst, otherwise use the num
# Output: lowest_hospital_names - vector of strings containing the names of the lowest mortality rate from heart attacks
#         within the state provided as input
#
test_hosp_by_state_heart_attack <-function(filename, state, state_rank_num = -1){
  my_debug <- FALSE
  
  if( my_debug == TRUE ){
    print("State")
    print(state)
  }
 
 # list_hospital_names <- c()
 list_hospital_names <- NULL
 bool_found_hospital <- FALSE
  
  read_df <- read.csv(filename)
  df <- data.frame(read_df[read_df$State == state,])
  #  print(df)
  # Make sure the column for heart attacks is cleaned to ensure NA is added where needed
  # print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  new_col <- replaceNA(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  #print(new_col)
  # Update the read in data frame with the replaced NA cells
  df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- new_col
  #print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  
  # Find the clean boolean vector
  bool_heart_attack <- complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  
  # Find lowest mortality rate if indicated by state_rank_num
  if(state_rank_num == -1 ){
    # Using the clean boolean vector find the lowest value
    lowest_heart_attack <- min(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[bool_heart_attack])
    
    if( my_debug == TRUE ){
      print("Lowest Heart Attack")
      print(lowest_heart_attack)
    }
  }
 # Find highest mortality rate if indicated by state_rank_num
 if(state_rank_num == -2 ){
   # Using the clean boolean vector find the lowest value
   highest_heart_attack <- as.numeric(max(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[bool_heart_attack]))
   
   if( my_debug == TRUE ){
     print("Highest Heart Attack")
     print(highest_heart_attack)
   }
 }
 
  # Create a new data frame with data which is cleaned
  complete_heart_attack_df <- data.frame(df[bool_heart_attack,])
  #print(complete_heart_attack_df)
  # Find the names of the hospitals from the cleaned set
  num_hospitals <- nrow(complete_heart_attack_df)
  
  if( my_debug == TRUE )
    print(num_hospitals)
  
  # Loop through the hospitals to find the hospital based on the
  # rank as specified by state_rank_num
  
  # first we sort the hospital list using order()
  complete_heart_attack_df <- complete_heart_attack_df[order(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[bool_heart_attack])),]
 
  # my debug
  mydf <- data.frame(complete_heart_attack_df)
#  print(mydf)
# sort the data frame
  mydf <- mydf[order(as.numeric(mydf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
# print(mydf)
# re-create the data frame again using the sorted one
 complete_heart_attack_df <- data.frame(mydf)
  
 
  count <- num_hospitals
  if (state_rank_num == -1 ){
    # Find the lowest rank hospital
    i <- 1
    while (count > 0){
      
      if(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]) <= lowest_heart_attack){
        list_hospital_names <- c(list_hospital_names, as.character(complete_heart_attack_df$Hospital.Name[i]))
        bool_found_hospital <- TRUE
        if ( my_debug == TRUE ){
          print(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]))
          print(complete_heart_attack_df$Hospital.Name[i])
        }
        
      }
      i <- i + 1
      count <- count - 1
    }
  } 
  else if(state_rank_num == -2){
    # Find the worst hospital
    i <- 1
    while (count > 0){
      
      if(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]) >= highest_heart_attack){
        list_hospital_names <- c(list_hospital_names, as.character(complete_heart_attack_df$Hospital.Name[i]))
        bool_found_hospital <- TRUE
        if ( my_debug == TRUE ){
          print(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]))
          print(complete_heart_attack_df$Hospital.Name[i])
        }
        
      }
      i <- i + 1
      count <- count - 1
    }
  }
  else {
 
    # Find the hospital with the requested rank
    i <- 1
    ranked_value <- 0
    while (count > 0){
      # we are using this looping logic since the ranking could have ties so we are just going to go down the
      # sorted list and find the rank by position in the sorted list and keep appending to the list
      
      
      if (i == state_rank_num ){
        list_hospital_names <- c(list_hospital_names, as.character(complete_heart_attack_df$Hospital.Name[i]))
        ranked_value <- as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i])
        bool_found_hospital <- TRUE
        if ( my_debug == TRUE ){
          print(as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]))
          print(complete_heart_attack_df$Hospital.Name[i])
        }
        
      }
      # if we have a ranked_value use it to find potentially other hospitals with the same value
      # and append to the hospital list. Remember later on we send back the sorted list.
      # the approach here is that the alphabetical sort wins
      if ( (i > state_rank_num) && 
             (ranked_value == as.numeric(complete_heart_attack_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i]) )){
        list_hospital_names <- c(list_hospital_names, as.character(complete_heart_attack_df$Hospital.Name[i]))
        bool_found_hospital <- TRUE
      }
      
      i <- i + 1
      count <- count - 1
    }
  }
 
   
  # sort the list to return it by alphabetical order
  if (bool_found_hospital == TRUE){
    list_hospital_names <- sort(list_hospital_names)
  } else {
    list_hospital_names <- NA
  }
  
  return(list_hospital_names)
}
#
#
test_hosp_by_state_heart_failure <-function(filename, state){
  
  my_debug <- FALSE
  
  if (my_debug == TRUE ){
    print("State")
    print(state)
  }

  
  lowest_hospital_names <- c()
  
  read_df <- read.csv(filename)
  df <- data.frame(read_df[read_df$State == state,])
  #  print(df)
  # Make sure the column for heart failures is cleaned to ensure NA is added where needed
  # print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  new_col <- replaceNA(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  #print(new_col)
  # Update the read in data frame with the replaced NA cells
  df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- new_col
  #print(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  
  # Find the clean boolean vector
  bool_heart_failure <- complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  
  # Using the clean boolean vector find the lowest value
  lowest_heart_failure <- min(as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[bool_heart_failure]))
  
  if( my_debug == TRUE ){
    print("Lowest Heart Failure")
    print(lowest_heart_failure)
  }

  
  # Create a new data frame with data which is cleaned
  complete_heart_failure_df <- data.frame(df[bool_heart_failure,])
  #print(complete_heart_attack_df)
  # Find the names of the hospitals with the lowest heart failure from the cleaned set
  num_hospitals <- nrow(complete_heart_failure_df)
  
  if (my_debug == TRUE )
    print(num_hospitals)
  
  count <- num_hospitals
  i <- 1
  while (count > 0){
    
    if(as.numeric(complete_heart_failure_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i]) <= lowest_heart_failure){
      
      lowest_hospital_names <- c(lowest_hospital_names, as.character(complete_heart_failure_df$Hospital.Name[i]))
      
      if( my_debug == TRUE ){
        print(as.numeric(complete_heart_failure_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i]))
        print(complete_heart_failure_df$Hospital.Name[i])
      }
 
    }
    i <- i + 1
    count <- count - 1
  }
  
  lowest_hospital_names <- sort(lowest_hospital_names)
  return(lowest_hospital_names)
}
#
#
test_hosp_by_state_pneumonia <-function(filename, state){
  
  my_debug <- FALSE
  
  if( my_debug == TRUE ){
    print("State")
    print(state)
  }

  
  lowest_hospital_names <- c()
  
  read_df <- read.csv(filename)
  df <- data.frame(read_df[read_df$State == state,])
  #  print(df)
  # Make sure the column for heart attacks is cleaned to ensure NA is added where needed
  # print(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  new_col <- replaceNA(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  #print(new_col)
  # Update the read in data frame with the replaced NA cells
  df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- new_col
  #print(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  # Find the clean boolean vector
  bool_Pneumonia <- complete.cases(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  
  # Using the clean boolean vector find the lowest value
  lowest_Pneumonia <- min(as.numeric(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[bool_Pneumonia]))
  
  if( my_debug == TRUE){
    print("Lowest Pneumonia")
    print(lowest_Pneumonia)
  }
 
  
  # Create a new data frame with data which is cleaned
  complete_Pneumonia_df <- data.frame(df[bool_Pneumonia,])
  #print(complete_Pneumonia_df)
  # Find the names of the hospitals with the lowest Pneumonia from the cleaned set
  num_hospitals <- nrow(complete_Pneumonia_df)
  
  if(my_debug == TRUE){
    print(num_hospitals)
  }
  
  count <- num_hospitals
  i <- 1
  while (count > 0){
    
    if(as.numeric(complete_Pneumonia_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i]) <= lowest_Pneumonia){
      
      lowest_hospital_names <- c(lowest_hospital_names, as.character(complete_Pneumonia_df$Hospital.Name[i]))
      
      if( my_debug == TRUE){
        print(as.numeric(complete_Pneumonia_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i]))
        print(complete_Pneumonia_df$Hospital.Name[i])
      }

    }
    i <- i + 1
    count <- count - 1
  }
  
  lowest_hospital_names <- sort(lowest_hospital_names)
  return(lowest_hospital_names)
}