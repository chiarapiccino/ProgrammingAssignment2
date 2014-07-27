################################################################
## Functions that Calculate and Cache the Inverse of a Matrix ##
################################################################

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                    ## Set inv variable as NULL by default
  set <- function(y) {           ## Function to set matrix
    x <<- y                      ## Assign value of y to x in global environment
    inv <<- NULL                 ## Set inv variable as NULL
  }
  get <- function() x            ## Function to get matrix -- retrieves x
  setInverse <- function(inverse) inv <<- inverse  ## Function to set inverse in global environment
  getInverse <- function() inv   ## Function to retrieve inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()          ## Set inv as NULL if it hasn't been calculated
  if(!is.null(inv)) {            ## Check if inv variable is not NULL (i.e. inv has been calculated)
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)              ## Set inv in global environment (no longer NULL)
  inv                            ## Print inverse  
}