
## This function returns the inverse of a given matrix and
## stores the result of this highly-cost-computing task in cache
makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function manages the cache storage for the 
## method above in order to avoid repeating computations
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }

        x$setInverse(solve(x$get(), ...))
        m
}
