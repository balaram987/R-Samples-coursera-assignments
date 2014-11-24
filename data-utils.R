#
# Function to convert numeric ID numbers to a 3 digit ID 
# for use in the sensor file specification
#
convertID <- function(sensor_id){
  
  if(sensor_id < 10){
    digitID <- paste("00",sensor_id,sep="")
    
  }
  if(sensor_id >= 10 && sensor_id < 100){
    digitID <- paste("0",sensor_id,sep="")
    
  }
  if(sensor_id >= 100 && sensor_id < 1000){
    digitID <- sensor_id
    
  }
  digitID
}
# Function to generate the list of files 
#
generateFileList <- function(root_folder, id_list){
  mydebug <- FALSE
  ids_3digit <- c()
  for(i in id_list){
    id <- convertID(i)
    ids_3digit <- c(ids_3digit,id)
  }
  
  if( mydebug == TRUE ){
    print("-- generateFileList --")
    print("Three digit IDs")
    print(ids_3digit)
  }
  
  data_file_names <- paste(root_folder, "\\", as.character(ids_3digit), ".csv", sep="")
  
  if( mydebug == TRUE ){
    print("-- generateFileList --")
    print(data_file_names)
  }
  data_file_names
}

#
# Function to read in files and combine into one dataframe and clean it
#
#
combineDataframe <- function(files){
  mydebug <- FALSE
  df <- data.frame()
  for (file_name in files){
    read_df <- read.csv(file_name)
    if (mydebug == TRUE){
      #      print("-- combineDataframe --")
      #      print(file_name)
      #      print(read_df)
    }
    df <- rbind(df, read_df)
  }
  # clean the data
  bool_clean_df <- complete.cases(df)
  clean_df <- df[bool_clean_df,]
  clean_df
}
#
# Function to read in a column in a data frame and replace "Not Available" with "NA"
# Return the newly created column
replaceNA <- function(orig_col){
  
  new_col <- c()
  col_length <- length(orig_col)
#  print(col_length)
  count <- col_length
  i <- 1
  while (count > 0){
   
    if ( orig_col[i] == "Not Available"){
#      print(orig_col[i])
      new_col <- c(new_col, NA)
    }
  else {

    val <- orig_col[i]
 
    new_col <- c(new_col, as.character(val))
  }
    count <- count - 1
    i <- i + 1
  }

# print(new_col)
  return(new_col)
  
}
#
# Error checking utility
#
checkErrors <- function(state_id, outcome){
  
  if( outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia"){
    #    print("OK")
  } else {
    prt <- paste("Error in best","(", state_id, ",", outcome, ")", sep="")
    print(prt)
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
  
  return(TRUE)
  
}
