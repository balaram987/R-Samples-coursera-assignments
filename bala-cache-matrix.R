## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix
# input argument: x - matrix
# return value: list containing methods for getting and setting the matrix and its inverse
#
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  setMatrix <- function(y){
    x <<- y
    inverse_matrix <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInverseMatrix <- function(inv){
    inverse_matrix <<- inv
  }
  getInverseMatrix <- function(){
    inverse_matrix
  }
  list( setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
# cacheSolve
# input argument: x - matrix
# return value: inverse of x
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  my_inverse <- x$getInverseMatrix()
  if(!is.null(my_inverse)){
    message("getting cached data")
    return(my_inverse)
  }
  my_matrix <- x$getMatrix()
  my_inverse <- solve(my_matrix, ...)
  x$setInverseMatrix(my_inverse)
  my_inverse
}
#
# Tester function
#
testFunctionForMatrixInverse <- function(matrix_value = matrix()){
  mat1 <- makeCacheMatrix(matrix_value)
  cacheSolve(mat1)
  print(mat1$getInverseMatrix())
  cacheSolve(mat1)
  print(mat1$getInverseMatrix())
}
