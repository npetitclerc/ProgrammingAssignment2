# R Programming - Assignment #2 - Caching the Inverse of a Matrix
#
# Define a special matrix (makeCacheMatrix) object that can cache its inverse 
# and a function (cacheSolve) that calculate or retrieve the matrix inverse 
# from cache if it exist.  

makeCacheMatrix <- function(x = matrix()) {
    # This function creates a special "matrix" object that can cache 
    # its inverse.
    #
    # Args:
    # x: an invertible matrix
    #
    # Returns:
    # a makeCacheMatrix object: a list of functions: set, get, setinverse 
    # and getinverse
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    #This function computes the inverse of the special "matrix" 
    #returned by makeCacheMatrix. If the inverse has already 
    #been calculated (and the matrix has not changed), then the cacheSolve 
    #retrieve the inverse from the cache.
    #
    # Args:
    # x: a makeCacheMatrix object
    #
    # Returns:
    # inv: the inverse of matrix x
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


