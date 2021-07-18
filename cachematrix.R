## Put comments here that give an overall description of what your
## functions do

#similar to makeVector, makeCacheMatrix will return a list of 
#functions that (1)set the matrix (2)get the matrix (3) set matrix inverse
#(4) get matrix inverse
#these will be used by the makeCacheMatrix function below
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function works very similarly to cachemean. It takes an object of the
#type produced by makeCacheMatrix. It will look for a cached value for the matrix
#inverse. In the event it is null, it will solve for one and cache that value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    matrix <- x$get()
    Inv <- solve(matrix, ...)
    x$setInverse(Inv)
    Inv
  }
