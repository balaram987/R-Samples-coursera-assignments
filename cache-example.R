# Example to show caching of data between environments
#
# This function is passed in a vector and 
# returns a list which is a special vector
#
makeVector <- function(x = numeric()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list( set = set, get = get, setmean = setmean, getmean = getmean)
}
#
#The argument 'x' is created by calling makeVector() defined above
# 
cachemean <- function(x, ...){
  m <- x$getmean()
# Check if a mean has been cached and use it if available
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
# Since it was not cached we compute and store the mean
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
#
# If you create a vector myvec, call the testFunction
# and the append to myvec and call mean() directly
# then when you run testFunction() with myvec
# it will return the cached mean! This is because myvec
# mean field got updated when you called mean() directly
# So be careful.
#

testFunctionForCacheExample <- function(value = numeric()){
  vec1 <- makeVector(value)
  # first call will calculate mean
  cachemean(vec1)
  # second call will get the cached value
  cachemean(vec1)
  
}