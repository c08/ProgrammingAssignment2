## This functions compute the inverse of a matrix and store the result in a cache.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(z) inverse <<- z
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data<- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
}
