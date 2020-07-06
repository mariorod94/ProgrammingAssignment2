# The following pair of functions are designed to calculate the inverse of a
## square matrix using the 'solve' function and caching its value to avoid
### computing it repeatedly



# 'makeCacheMatrix' gets and sets the value of a special matrix than can cache
## its inverse using the 'solve' function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setCacheInverse <- function(solve) m <<- solve 
        getCacheInverse <- function() m
        list <- list(set = set, get = get, setCacheInverse = setCacheInverse, 
                     getCacheInverse = getCacheInverse)

}


# 'cacheSolve' calculates de inverse of a matrix if there is no cached data
## from the 'makeCacheMatrix' function



cacheSolve <- function(x, ...) {
        m <- x$getCacheInverse
        if (!is.null(m)) {
                message("Getting matrix cached data")
                return(m)
        }
        dataMatrix <- x$get()
        m <- solve(dataMatrix, ...)
        x$setCacheInverse(m)
        m
}

