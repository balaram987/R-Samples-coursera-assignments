source("data-utils.R")
##
## Function: complete
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
##
## NOTE: make sure you source pollutantmean.R, since it has some functions that are re-used here;
##        Later on we will bunch them into a utility file
##
complete <- function(directory, sensor_ids) {
  mydebug <- TRUE
  mydebug2 <- FALSE

# create a data frame with all the sensor ID, later we will remove the rows without any data
  df <- data.frame()

  for( tmp_id in sensor_ids){
# Generate a single file name
    filename <- generateFileList(directory, tmp_id)
    if (mydebug2 == TRUE ) print(filename)
# Read .csv into a data frame
    tmp_df <- read.csv(filename)
# identify the number of clean rows
    bool_clean_df <- complete.cases(tmp_df)
# create the clean data frame
    tmp_clean_df <- tmp_df[bool_clean_df,]
    if (mydebug2 == TRUE ) print(tmp_clean_df)
# Identify the length of the data frame by finding the length of the first column
    num_obs <- length(tmp_clean_df[,1])
    if (mydebug2 == TRUE ) print(num_obs)
# create a temp data frame with the data
    tmp_df2 <- data.frame(id = tmp_id, nobs = num_obs)
    if (mydebug2 == TRUE ) print(tmp_df2)
# append this temp data frame to main data frame
    df <- rbind(df, tmp_df2)
  }
  if (mydebug == TRUE ) print(df)
  
  return(df)
  
}