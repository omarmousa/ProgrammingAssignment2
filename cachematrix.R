## Name: Omar Mousa
## Date: 08/23/15
## CourseId: rprog-031
## =======================

## This function creates a special "matrix" object that can cache its inverse, which is really a list 
## containing a function to

##  1. set the value of a matrix
##  2. get the value of a matrix
##  3. set the inverse
##  4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inverse <<- inverse
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the
## Inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrive 
## the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    if (!.isnull(inverse)) {
        message("Retrieving Cached Data")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    
    x$setInverse(inverse)
    return(inverse)
    
}
