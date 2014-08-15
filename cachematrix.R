## These two functions together store a matrix and cache its inverse.
## The matrix is assumed to have an inverse.

## The first function, makeCacheMatrix, creates a list
## containing a function to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse
##  - get the value of the inverse

makeCacheMatrix <- function(A = matrix()) {
    C <- NULL
    set <- function(B) {
        A <<- B       ## in calling environment
        C <<- NULL
    }
    get <- function() {A}
    setinverse <- function(solve) {C <<- solve}
    getinverse <- function() {C}
    list(set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## The second function, cacheSolve, calculates the inverse of the matrix. First checks whether the matrix has been already calculated. If it has, takes the inverse from the cache. If not, calculates the inverse and stores it in the cache.

cacheSolve <- function(A, ...) {
    C <- A$getinverse()
    if(!is.null(C)) {return(C)} ## returns cached inverse
    B <- A$get()         ## get the matrix
    C <- solve(B, ...)   ## invert the matrix
    A$setinverse(C)      ## cach the matrix
    C                    ## ret the calculated inv
}

