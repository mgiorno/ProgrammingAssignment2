## This function caches the inverse of the matrix provided if there
## is no current matrix value that has already been cached.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     get <- function() {
          x
     }
     
     setmatrix <- function(matrix) {
          m <<- matrix
     }
     
     getmatrix <- function() {
          m
     }
     
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}

## The cacheSolve function calculates and caches the inverse matrix of
## the original matrix supplied as the argument.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getmatrix()
     
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     data <- x$get()
     
     m <- solve(data, ...)
     
     x$setmatrix(m)
     
     m
}