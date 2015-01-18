## Put comments here that give an overall description of what your
## functions do

## function: makeCacheMatrix
##
## This function receives 1 parameter:
## x: Is the matrix that will be used for the caching
##
## This function has 2 variables on it's environment:
## x: Stores the original matrix passed to it
## i: Stores the inverse matrix
##
## This function returns a list of 4 functions:
## set: Initialize x with the provided params and y with NULL
## get: Returns x
## setinverse: Stores the provided value to the variable i
## getinverse: Returns the values store in the variable i
##
## All 4 functions operates on the original makeCacheMatrix environment.
## It does so using the <<- operator
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function: cacheSolve
## 
## This functions has 2 parameters:
## x: Is the list created by the makeCacheMatrix function
## ...: Special parameter that will be passed to the solve function
##
## This function always returns the inverse of the matrix calculated by the function solve
##
## It works by first trying to access the method getinverse.
## In case it returns a value that's not NULL, it means that the inverse had been calculated before
## If so, it emits a message and returns the value of the inverse.
## In case it's the first time it tries to calculate, it means that the value will be NULL
## Therefore it will fetch the data using the get function and calculate the inverse
## After that it will use the function setinverse to store that value, so it can be accessed next time
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}
