## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## x: a square invertible matrix
        ## returns functions for set matrix, get matrix, set inverse, and get inverse.
        
        inverseMatrix = NULL
        set = function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) inverseMatrix <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it 
## retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
