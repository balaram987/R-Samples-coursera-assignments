#
# Simple R program to read data and perform some computations
# TODO: Create variables and functions to make the code modular
#
data_file_name <- c("D:\\users\\hrm\\Coursera_DataScience_R_programming\\Assignments\\rprog-data-quiz1_data\\hw1_data.csv")
# read the .csv into a data frame
print(" Reading data from D:\\users\\hrm\\Coursera_DataScience_R_programming\\Assignments\\rprog-data-quiz1_data\\hw1_data.csv")
mydf <- read.csv(data_file_name)
# print the first 6 rows of the data
print("the first 6 rows of the data")
print(mydf[1:6,])
# generate a sequence such that only data without NA is present
good <- complete.cases(mydf)
# print the sequence
print("The logical sequence of elements that do not have missing data")
print(good)
# use the "good" sequence to work on the data frame
# Get the first 6 rows of data without NA values
print("the first 6 rows of the data without NA values in them - 
      note the differences between this one and the previous")
print(mydf[good, ][1:6, ])
# start building a clean data frame
bool_clean_mydf <- complete.cases(mydf)
# create a new data frame that is clean
clean_mydf <- mydf[bool_clean_mydf,]
print("Clean data frame - note the rows with NA have been eliminated")
print(clean_mydf)
# Create a subset where ozone is greater than 31
oz_gt_31 <- clean_mydf[clean_mydf[,1]>31,]
print("Show the data where Ozone > 31 ")
print(oz_gt_31)
# Use the Ozone > 31 data but now select data where temp > 90
oz_gt_31_temp_gt_90 <- oz_gt_31[ oz_gt_31[,4]>90 ,]
print("Shows the data for Ozone > 31 data && temp > 90")
print(oz_gt_31_temp_gt_90)
print("calculate the mean of the solar radiation data")
solar_r <- oz_gt_31_temp_gt_90[,2]
print(solar_r)
print("Mean of solar R")
print(mean(solar_r))
print("create month 6 data")
month_6_data <- clean_mydf[clean_mydf[,5]==6,]
print(month_6_data)
print("Temperature for month 6 and calcuate the mean")
print(month_6_data[,4])
print(mean(month_6_data[,4]))
print("create month 5 data")
month_5_data <- clean_mydf[clean_mydf[,5]==5,]
print(month_5_data)
print("find the maximum temperature for month 6")
print(max(month_5_data[,1]))
