## The functions below create a special matrix object capable of caching its
## inverse and evaluate the inverse of such a matrix. The inverse computation
## is done only if the special matrix hasn't changed and its inverse has already
## been evaluated. In this case the inverse is not evaluated, but it's cached.
## It's assumed that the matrix supplied is always invertible.

## The function makeCacheMatrix creates a special matrix object that can cache
## its inverse. The special matrix object is a list containing functions to:
##  - set the value (the entries) of the matrix
##  - get the value (the entries) of the matrix
##  - set the value (the entries) of its inverse
##  - get the value (the entries) of its inverse
## The function takes as input a matrix (a default value is anyway assigned to
## the formal argument) containing the entries to be assigned at the special
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve computes the inverse of a special matrix created with
## the function makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the function caches the inverse
## skipping calcuations.
## The function takes as input the special matrix (created with makeCacheMatrix)
## whose inverse has to be evaluated (or cached).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinv(inverse)
    inverse
}
