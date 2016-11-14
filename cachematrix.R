## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Function to create a special "matrix" object that can cache its inverse
## it has 4 embeded functions
## set, get: setter and getter functions for the matrix object
## setinverse: getinverse: setter and getter functions for the inverse of the matrix object  
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the matrix is unchanged and its inverse
## has already been calculated, then the cached inverse should be returned else inverse of the matrix computed using the "solve" function and 
## cached for future use
## Assumption: matrix is always "invertible"
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # retrieve chached inverse for matrix x
        matinv <- x$getinverse()
        if(!is.null(matinv)) {
                message("retreiving cached inverse")
                # return the inverse of matrix x
                return(matinv)
        }
        
        # either inverse was not cached for given matrix x or matrix x has changed 
        matdata <- x$get()
        matinv <- solve(matdata,...)
        x$setinverse(matinv)
        matinv
}
