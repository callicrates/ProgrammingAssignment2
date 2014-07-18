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
#
# IMPLEMENTATION NOTES:
#
# This function implements a form of object-oriented programming.
# Instead of an explicit object as we have in languages like C++,
# Python, Java and friends, we use the environment defined within the
# function's scope as a place to hold values.  Our "member functions"
# are closures -- functions + environments -- that are defined within
# this scope so that they have access to the x and cachedInverse
# values that are otherwise completely hidden from the user.

makeCacheMatrix <- function(x = matrix()) {

    # Initialize the inverse to an empty value since we haven't
    # computed it yet.
    cachedInverse <- NULL

    # Usage: mymatrix$set(newMatrix)
    #
    # Returns: nothing
    #
    # Supply a new value for the matrix and zero out any cached value
    # we might have for the inverse.  NOTE: This function does not
    # check to see if the new matrix is the same as the old one.

    set <- function(newMatrix) {
        # We use <<- here because x and cachedInverse are defined in
        # our parent environment.
        x <<- newMatrix
        cachedInverse <<- NULL
    }

    # Usage: mymatrix$get()
    #
    # Returns: matrix supplied to makeCacheMatrix() or the most recent
    # call to set()
    get <- function() x

    # Usage: mymatrix$setInverse(value)
    #
    # Returns: nothing
    #
    # Set a value for the matrix inverse.  This will typically be
    # called by cacheSolve() although you can call it yourself if you
    # really want to.
    setInverse <- function(inv) {
        cachedInverse <<- inv
    }

    # Usage: mymatrix$getInverse()
    #
    # Returns: saved value for matrix inverse or NULL if no value has
    # been set
    getInverse <- function() cachedInverse

    # Return from constructor: the entries in the list are closures
    # that can modify the entries in this environment.
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
    # If the matrix already has a cached inverse, return it and save
    # ourselves some work.  Recall that cachedInverse is set to NULL
    # when the object is created and any time the matrix changes, so
    # if it's non-NULL we trust that it corresponds to the current
    # matrix.
    cachedValue <- x$getInverse()
    if (!is.null(cachedValue)) {
        message("Returning cached matrix inverse")
        return(cachedValue)
    }

    # The matrix doesn't have a cached inverse.  Compute it (passing
    # along any extra parameters to solve() via the '...' argument)
    # and cache it inside our custom matrix class, then return our
    # shiny new value to the user.
    newInverse = solve(x$get(), ...)
    x$setInverse(newInverse)
    newInverse
}
