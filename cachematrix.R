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
        getinv = function() inverseMatrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it 
## retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x: output of makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculate the inverse 
        matrixData = x$get()
        inv = solve(matrixData, ...)
        
        # set the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        
        return(inv)
}
