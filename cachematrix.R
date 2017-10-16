## cacheMatrix.R
##
## Date: 15 Oct 2017
## Author: Mike Matthews
## 
## Description: 
## Defines two functions: one for creating a cacheable matrix object, and another for
## computing and caching the inverse solution of the matrix object.

## Wraps a matrix object in a set of caching functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() { x }
    setInv <- function(solution) { inv <<- solution }
    getInv <- function() { inv }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Solves for the inverse of a matrix, caching the result.
## If the cached result already exists for the matrix, the cache is returned.
## The matrix 'x' must have been created using the 'makeCacheMatrix' function
## for this caching to work.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()

    if (!is.null(inv)) {
        return(inv)
    }

    matr <- x$get()
    inv <- solve(matr)
    x$setInv(inv)
    inv
}
