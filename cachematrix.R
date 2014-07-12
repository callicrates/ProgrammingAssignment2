## Put comments here that give an overall description of what your
## functions do

## Create a 'matrix' that can cache its own inverse
#
# This function is actually a little sneaky.  It creates an
# environment that contains two values -- 'x' and 'cachedInverse' --
# and then returns four closures that can access and modify those
# objects.
#
# Is this the best way to do this in R or is this a demonstration of
# scoping rules for the purpose of this class?

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        cachedInverse <<- NULL
    }
    get <- function() x

    setInverse <- function(inv) {
        cachedInverse <<- inv
    }
    getInverse <- function() cachedInverse

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a matrix 'x'.  This matrix must have been
## created using makeCacheMatrix.

cacheSolve <- function(x, ...) {
    cachedValue <- x$getInverse()
    if (!is.null(cachedValue)) {
        message("Returning cached matrix inverse")
        return(cachedValue)
    }
    newInverse = solve(x$get(), ...)
    x$setInverse(newInverse)
    newInverse
}
