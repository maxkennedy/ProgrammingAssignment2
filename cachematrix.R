## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## Included below are a pair of functions that cache the inverse of a matrix - makeCacheMatrix & cacheSolve.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It creates a special "vector", really a list containing a function to: setMatrix, getMatrix, setInverse,GetInverse

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse1 <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inverse1 <<- inverse
  getInverse <- function() inverse1
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse1 <- x$getInverse()
  if (!is.null(inverse1)) {
    message("getting cached data")
    return(inverse1)
  }
  matrix1 <- x$getMatrix()
  inverse1 <- solve(matrix1, ...)
  x$setInverse(inverse1)
  inverse1
}

