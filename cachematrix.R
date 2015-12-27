# Mercedes Vega Villar.
## R PROGRAMMING CLASS. DATA SCIENCE SPECIALIZATION.
### ASSIGNMENT 2.


## This piece of code creates the object "makeCacheMatrix".
## This object consists of a list of functions that store the cached computation of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This creates a function that returns the inverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
