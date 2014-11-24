source("data-utils.R")
#
# Function: pollutantmean
# Inputs: 
#   specdata: string, location of the root folder containing the files
#   pollutant: string, name of the pollutant
#   ids: integers, list of sensor ID used to locate the exact file with sensor data
#
pollutantmean <- function(specdata, pollutant, ids = 1:332){
  mydebug <- FALSE
  
  if (mydebug == TRUE ){
    print("-- pollutantmean --")
    print("Location of data")
    print(specdata)
    print("Pollutant")
    print(pollutant)
    print("Sensor ID")
    print(ids)
  }
# 
# We don't use the col_num, rather we use the pollutant name itself for getting the approp. col.
# We are just performing this check here for sanity
  col_num <- 0
  if(pollutant == "sulfate"){
    col_num <- 2
  } 
  else if(pollutant == "nitrate"){
    col_num <- 3
  } else {
    print("ERROR - incorrect pollutant specified ")
    return
  }

  file_list <- generateFileList(specdata, ids)
  if (mydebug == TRUE ){  
    print("-- pollutantmean --")
    print(file_list)
  }
  # Read in the data from the files
  # Combine them into one data frame
  total_df <- combineDataframe(file_list)
 
  if (mydebug == TRUE ){  
    print("-- pollutantmean --")
    print(total_df)
  }

# calcluate the mean of the specified pollutant
 
#  pollutant_values <- total_df[,col_num]
  pollutant_values <- total_df[,pollutant]
  
  mean_value <- mean(pollutant_values)

if (mydebug == TRUE ){  
  print("-- pollutantmean --")
  print(pollutant)
  print(pollutant_values)
  print(mean_value)
}

  mean_value

}



