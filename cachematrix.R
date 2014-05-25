## 'makeCacheMatrix' creates a special 'cachableMatrix' class that can cache
## the inverse of matrix.
## 
## 'cacheSolve' computes the inverse of the special "matrix"
## returned by the 'makeCacheMatrix' function. If the inverse has already been 
## calculated (and the matrix has not changed), then 'cacheSolve' retrieve 
## the inverse from the cache.
## 
## Example:
##> hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
##> A <- hilbert(4)
##> myMatrix <- makeCacheMatrix(A)
##> cacheSolve(myMatrix)
##     [,1]  [,2]  [,3]  [,4]
##[1,]   16  -120   240  -140
##[2,] -120  1200 -2700  1680
##[3,]  240 -2700  6480 -4200
##[4,] -140  1680 -4200  2800
##> cacheSolve(myMatrix)
##Getting cached result :
##     [,1]  [,2]  [,3]  [,4]
##[1,]   16  -120   240  -140
##[2,] -120  1200 -2700  1680
##[3,]  240 -2700  6480 -4200
##[4,] -140  1680 -4200  2800
##> 


## 'makeCacheMatrix' creates a special "matrix", which is a class named 
## 'cachableMatrix'containing following methods:
## setdata: set the value of the matrix
## getdata: get the value of the matrix
## setinverse: set the value of the inverse
## getinverse: get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setdata <- function(y) {
        x <<- y
        i <<- NULL
    }
    getdata <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    z <- list(setdata = setdata,
              getdata = getdata,
              setinverse = setinverse,
              getinverse = getinverse)
    class(z) <- "cachableMatrix"
    return(z)
}


## 'cacheSolve' calculates the inverse of the special "matrix" created
## with the 'makeCacheMatrix' function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix data and sets the inverse in the cache via the 'setinverse' method.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Getting cached result :")
        return(i)
    }
    data <- x$getdata()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
}