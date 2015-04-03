# makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# It is built upon the example from assignment notes

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-  function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inverse <<- solve
    getInv <- function() inverse
    list(set=set, get=get,
         setInv = setInv,
         getInv = getInv)
}

# cacheSolve
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# It is built upon the example from assignment notes

cacheSolve <- function(x=matrix(), ...) {
    inverse <- x$getInv()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse) # if inverse already calculated, retrieve inverse
    }
    matrix <- x$get
    inverse <- solve(matrix, ...)
    x$setInv(inverse)
    inverse
}