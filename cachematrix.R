## This is coursera R Programming Assignment 
## learning how to cache variables using "<<-" operator
## A pair of functions which cache the inverse of a matrix are written as below.

##Below function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## And the below next function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
