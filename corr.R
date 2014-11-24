source("data-utils.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Broad overview of approach
  ## Look at each sensor data and determine the clean data from the sensor.
  ## If the length of the clean data is more than the threshold then this data should be processed
  ## Compute the correlation between nitrate and sulphate and store this value in a vector (the one to be returned)
  
  sensor_ids <- c(1:332)
  correlation_value_vector <- vector("numeric")
  for( tmp_id in sensor_ids){
    # Generate a single file name
    filename <- generateFileList(directory, tmp_id)
    # Read .csv into a data frame
    tmp_df <- read.csv(filename)
    # identify the number of clean rows
    bool_clean_df <- complete.cases(tmp_df)
    # create the clean data frame
    tmp_clean_df <- tmp_df[bool_clean_df,]
    # Identify the length of the clean data frame by finding the length of the first column
    num_obs <- length(tmp_clean_df[,1])
    # if the num_obs is greater than threshold then perform the computation otherwise skip
    if( num_obs > threshold){
      correlation_value <- cor(tmp_clean_df[,2], tmp_clean_df[,3])
      print(correlation_value)
      correlation_value_vector <- c(correlation_value_vector, correlation_value)
    }
    
  }
  return(correlation_value_vector)
}