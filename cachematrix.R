## These functions are used for storing a matrix and its 
## inverse as one object. To use, the call "makeCacheMatrix"
## on an invertable matrix to store the matrix, then run
## "cacheSolve" on the produced list object. This will return
## the inverse of the matrix, and store it in the list object,
## it can then be returned again with the "getinverse" sub
## function of the list object, or by rerunning "cacheSolve",
## which will not recompute the inverse if it has already 
## been stored.


## This function sets up list of functions which allow for
## storing of a matrix and its inverse in the object the
## environment.
## - Calling the main function on a matrix stores the matrix 
##   as "x" and the sets the inverse matrix "inv" to Null.
## - The "set" sub function replaces the the curently stored
##   matrix with its argument, and sets the stored inverse
##   to Null, as it will need to be recomputed.
## - The "get" sub function returns the currently stored
##   matrix
## - The "setinverse" sub function stores its argument as
##   "inv" in the object environment - intended for storing
##   the inverse of the matrix.
## - The "getinverse" sub function returns the "inv" variable.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function operates on the list object produced by
## "makeCacheMatrix", if the inverse has not already stored it
## will be calculated, if it is already stored, it will not be
## recalculated and will just be returned.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
