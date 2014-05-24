##  CACHING THE INVERSE OF A MATRIX
##
## The following R functions provide the ability to cache potentially time-
## consuming matrix-inversion computations by storing the results until they 
## are needed rather than recomputing the calculation. 
##
## "makeCacheMatrix," creates a vector "list" from a function that sets the 
## value of the vector, retrieves the value of the vector, sets the value of 
## the matrix inverse, and obtains the value of the vector inverse. These 
## operations allow this function to create a special "matrix" with the 
## ability to cache its own inverse.
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## "cacheSolve" performs the inverse computations of the "vector" created with
## the function "makeCacheMatrix." Where the inverse computations have already
## been computed, assuming no changes have occurred to the matrix, the function
## "cacheSolve" will then obtain the inverse results from the cache.  

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
