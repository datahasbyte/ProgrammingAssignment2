## Programming Assignment 2
## cacheMatrix.R

## Write a short comment describing this function
## the makeCacheMatrix function stores a matrix
##  in the cache to save computation expense

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(nrow, ncol, y=matrix()){
        x <<- matrix(y, nrow=nrow, ncol=ncol)
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get=get, setsolve = setsolve, getsolve=getsolve)
}


## works with makeCacheMatrix to retrive values from
## and to set values in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
}
