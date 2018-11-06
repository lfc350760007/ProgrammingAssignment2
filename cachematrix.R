## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to 
## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

rm(list = ls())

makeCacheMatrix <- function(x = matrix()){
        if(nrow(x)!=ncol(x))
        {
                return("Not a valid matrix.")
        }
        else{
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
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

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
