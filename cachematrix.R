##
## This file contains two utility functions which cache matrices inverses.
##
## - makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## - cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
##               If the inverse has already been calculated (and the matrix has not changed),
##               then the cachesolve retrieves the inverse from the cache.
##
## Sample use:
##
##    amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##    cacheSolve(amatrix)
##    amatrix$getinverse()
##
##    amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
##    cacheSolve(amatrix)
##

##
## makeCacheMatrix
## This function returns a list containing the following functions:
##
##  set: set the value of the matrix
##  get: returns the value of the matrix
##  setinverse: sets the value of the inverse (not meant to be externally called)
##  getinverse: returns the (maybe cached) inverse of the matrix
##
makeCacheMatrix <- function(mtrx = matrix())
{
    mtrxInverse <- NULL

    # sets a new value for the stored matrix and resets the cache
    set <- function(newMatrix)
    {
        mtrx <<- newMatrix
        mtrxInverse <<- NULL
    }

    # returns the stored matrix
    get <- function()
    {
        mtrx
    }

    # sets a new value for the matrix inverse
    setinverse <- function(inverse)
    {
        mtrxInverse <<- inverse
    }

    # returns the matrix inverse
    getinverse <- function()
    {
        mtrxInverse
    }

    # return a list with the functions defined for the given matrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##
## cacheSolve
## This function calculates, stores and returns the inverse of the matrix
## created with the makeCacheMatrix function.
## If the inverse of a matrix as already been calculated,
## returns the cached result and skips the computation.
##
cacheSolve <- function(mtrx, ...)
{
    ## get the cached inverse of the input matrix
    mtrxInverse <- mtrx$getinverse()
    if (!is.null(mtrxInverse))
    {
        # if a cached value is available, return it
        message("getting cached data")
        return(mtrxInverse)
    }

    # if no cached data is available, calculate it...
    data <- mtrx$get()
    mtrxInverse <- solve(data, ...)

    # ... update the cache...
    mtrx$setinverse(mtrxInverse)

    # ... and return the result
    mtrxInverse
}
