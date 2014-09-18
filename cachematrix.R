## Calulate and cache the inverse of a matrix.
## Future retrevals will get the cached version
## until the matrix is changed. The next call
## to get the inverse will reacalculate and cache
## the new inversion.

## factory that makes the functions for setting
## and getting the matrix, and setting and getting
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(mtx) inv <<- mtx
    getInv <- function() { inv }
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## takes the output of the factory above and uses the
## functions to either get the cached inverse, or
## calculate and cache a new one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 
        ## the matrix passed in to makeCacheMatrix()
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx)
    x$setInv(inv)
    inv
}
