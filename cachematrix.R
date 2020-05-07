## Compute the inverse of a matrix and cache the result
## using a pair of functions, namely makeCacheMatrix and cacheSolve

## Create a list containing various functions to operate on a matrix and get its inverse
## set : declare the matrix whose inverse is to be calculated and set its inverse as NULL
## get : get the matrix whose inverse is to be calculated
## setInverse : set the inverse of the given matrix
## getInverse : returns the inverse of the given matrix

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(x){
      mat <<- x
      inv <<- NULL
    }
    get <- function() mat
    setInverse <- function(inverse) inv <<- inverse  
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
