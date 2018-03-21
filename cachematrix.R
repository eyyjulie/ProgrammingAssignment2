## Matrix inversion can be a costly computation, so there is some
## benefit to caching the invserse of a matrix rather than repeatedly
## computing it. These two functions accomplish matrix inversion and
## caching.
## 


## This function creates a matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## This function computes the inverse of the matrix returned 
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cachesolve retrieves 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
