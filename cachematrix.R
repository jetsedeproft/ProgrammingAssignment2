## Put comments here that give an overall description of what your
## functions do

## Step 1. Write makeCacheMatrix: This will create a special object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # reset inverse cache
  }
  
  # get the matrix
  get <- function() x
  
  # set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # get the cached inverse
  getInverse <- function() inv
  
  # return list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks whether the inverse has already been computed.

# If yes → returns the cached inverse.

# If no → computes it using solve(), stores it, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    
    # compute inverse since not cached
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
  }

