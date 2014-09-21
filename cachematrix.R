## makeCacheMatrix and cacheSolve are used in combination to
## calculate the inverse of an invertible matrix and cache the
## inverse for future use without the need to recalculate the inverse

## makeCacheMatrix creates a list of functions to set the invertible
## matrix, output the invertible matrix, cache the inverse of the 
## matrix and output the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks the cached inverse matrix to check if the value is NUlL.
## If it is NULL, then cacheSolve retrieves the invertible matrix from makeCacheMatrix
## and calculates the inverse matrix. This inverse matrix is cached in makeCacheMatrix.
## If the value is not NULL, cacheSolve returns the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

