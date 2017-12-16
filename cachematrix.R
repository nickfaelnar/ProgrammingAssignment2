## Create a matrix object that can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve will retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
       inverse <- x$getinverse()
       
       if (!is.null(inverse)) return (inverse)
       
       xMatrix <- x$get()
       inverse <- solve(xMatrix, ...)
       x$setinverse(inverse)
       inverse
}
