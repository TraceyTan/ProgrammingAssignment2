### The two functions below reduce processing time on calculating the inverse of
### a matrix, 'x'.  An error will be returned if the matrix is not invertible.

## The makeCacheMatrix function creates a list of matrix objects that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     invx <- NULL
     set <- function(y) {
          x <<- y
          invx <<- NULL
     }
     get <- function() x
     setinv <- function(inversex) invx <<- inversex 
     getinv <- function() invx
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


### This function checks if the inverse of a matrix already exists in the cache.
### If not, the inverse is computed and stored in the cache.
cacheSolve <- function(x, ...) {
     invx <- x$getinv()
     if(!is.null(invx)) {
          message("Getting cached data...")
          return(invx)
     }
     data <- x$get()
     invx <- solve(x)
     x$setinv(invx)
     invx
}