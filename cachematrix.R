## Put comments here that give an overall description of what your
## functions do

## Create a 'matrix' that can cache its own inverse
#
# Usage: foo <- makeCacheMatrix(myMatrix)
#
# Parameters:
#    myMatrix: any R matrix
#
# Returns:
#    A list with four members, all functions:
#      get():           Return the current value of the matrix
#      set(newMatrix):  Supply a new matrix
#      getInverse():    Return the inverse of the current matrix
#      setInverse(inv): Set a new value for the inverse

# IMPLEMENTATION NOTES:
#
# This function is actually a little sneaky.  It creates an
# environment that contains two values -- 'x' and 'cachedInverse' --
# and then returns four closures that can access and modify those
# objects.
#
# Is this the best way to do this in R or is this a demonstration of
# scoping rules for the purpose of this class?
#
# Also, do we really need to return the setInverse method?  It doesn't
# make sense to expose that to the user.  The user doesn't care about
# the internal state; she only cares about getting the inverse of the
# matrix.

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
#
# Usage:
#
# foo <- makeCacheMatrix(myMatrix)
#
# myInverse <- cacheSolve(foo, ...)
#
# Returns:
#
#   Matrix inverse of the current value stored in foo
#
#
# Any extra arguments to cacheSolve beyond the first one will be
# passed along to the 'solve' call.

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
