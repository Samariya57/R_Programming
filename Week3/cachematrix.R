##The problem we try to solve is: computation of inverse 
## matrix is expensive. Let's try to storage this onfo for 
## some matrix.

## This function has to create a cpecial "vector" to help 
## storage inverse matrix and functions to work with


makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
          x <<- y
          invMat <<- NULL
        }
        get <- function() x
        setInv <- function(solve) invMat <<- solve
        getInv <- function() invMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Catch if inverse for this matrix already has been
##calculated. If not so, do it!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
        if(!is.null(invMat)) {
          message("getting cached data")
          return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setInv(invMat)
        invMat
}
