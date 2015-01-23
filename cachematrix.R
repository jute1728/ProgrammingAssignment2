## cachematrix.R
##
## This script contains a pair of functions that cache the
## inverse of a matrix.
##
## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly.
##
## Example of use:
##    matobj <- makeCacheMatrx()        ## create matrix object
##    matobj$set(A)                     ## Set matrix to A
##    B <- cacheInverse(matobj)         ## Set B to the inverse of A
##
## The first 2 commands above may be combined by passing A as
## the optional parameter to makeCacheMatrix.





## makeCacheMatrix
## ---------------
## This function creates a special "matrix" object that can
## cache its inverse.
##
## Example of use:
##      matobj <- makeCacheMatrix(A)
##
makeCacheMatrix <- function(cachedMatrix = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        cachedMatrix  <<- y
        cachedInverse <<- NULL
    }
    get    <- function()    cachedMatrix
    setinv <- function(inv) cachedInverse <<- inv
    getinv <- function()    cachedInverse
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
    
}

## cacheSolve
## ----------
## This function computes the inverse of the "matrix" stored
## in the matrix object. (See "makeCachObject" for how to 
## create a matrix object).
##
## The inverse is remembered so that a second call to the 
## function does not have to actually calculate the inverse.
##
## Example of use:
##      B <- cacheSolve(matobj)
##
cacheSolve <- function(matrixObject, ...) {

    myInv <- matrixObject$getinv()    ## look for the inverse in cache
    
    if(!is.null(myInv)) {             ## if found, return it
        message("getting cached data")
        return(myInv)
    }

    ## the inverse still has to be calculated...
    myMat <- matrixObject$get()
    myInv <- solve(myMat, ...)
    matrixObject$setinv(myInv)
    myInv
}
