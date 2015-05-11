## Two utility functions for caching the inverse of a matrix:
## - makeCacheMatrix: creates a matrix wrapper object (essentially a list of
##    functions), which provides get/set access to the original matrix and 
##    get/set access to its inverse.
## - cacheSolve: takes a matrix wrapper object (see makeCacheMatrix) and returns
##    its inverse with lazy caching.
##
## Example use:
## > my_matrix <- matrix(1:4, 2, 2)                   # base matrix
## > my_matrix_cached <- makeCacheMatrix(my_matrix)   # caching matrix
## > cacheSolve(my_matrix_cached)                     # inverse of matrix
## > my_matrix_cached$set(my_matrix)                  # set base matrix

## Returns a matrix wrapper object, in the form of a list of four functions, for
## setting and getting the given matrix, and its inverse, respectively.
## The inverse is not eagerly calculated, and must be set separately.
## If the base matrix is changed through a call to set(), any cached inverse is
## cleared.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if(!identical(x, y)) {
            x <<- y
            inv <<- NULL            
        }
    }
    get <- function() x
    setinverse <- function(s) inv <<- s
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes, and returns the inverse of, a custom matrix wrapper object (see makeCacheMatrix):
## - the first call for a given matrix will require the inverse to be calculated;
## - subsequent calls for the same matrix will be faster, as the inverse will have been cached;
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("cacheSolve: getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
