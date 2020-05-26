## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" - x, 
## that can cache its inverse. set ans/inverse "inverse"
## as NULL. It can get-set the matrix , inverse matrix 
## also create list of those

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix . 
## Validate if the matrix "x" is inversed or not 
## If "yes" then return the matrix or
## inverse it and then return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting Inversed Matrix ... !")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
