## cacheMatrix.R
##
## Date: 15 Oct 2017
## Author: Mike Matthews
## 
## Description: 
## Defines two functions: one for creating a cacheable matrix object, and another for
## computing and caching the inverse solution of the matrix object.
##


## Wraps a matrix object in a set of caching functions to be used by the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
    # create a variable to hold the cached inverse
    inv <- NULL
    # set function to set a new matrix 'x' and reset the cache variable
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the matrix
    get <- function() { x }
    # set the cached inverse value
    setInv <- function(solution) { inv <<- solution }
    # get the cached inverse value
    getInv <- function() { inv }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Solves for the inverse of a matrix, caching the result.
## If the cached result already exists for the matrix, the cache is returned.
## The matrix 'x' must have been created using the 'makeCacheMatrix' function
## for this caching to work.

cacheSolve <- function(x, ...) {
    # get the cached inverse
    inv <- x$getInv()
    # if it exists, return it
    if (!is.null(inv)) {
        return(inv)
    }
    # otherwise, calculate the inverse matrix and cache it
    matr <- x$get()
    inv <- solve(matr)
    x$setInv(inv)
    inv
}
