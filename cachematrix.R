## These are a set of functions that can create and cache a inverse of a matrix


## This function creates a matrix object that can cache the inverse

makeCacheMatrix <- function(matrix = matirx()) {
  matrixInverse <- NULL
  set <- function(y) {
    matrix <<- y
    matrixInverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This computes the inverse of the matrix created in makeCacheMatrix
## If the inverse has already been calculated then cacheSolve should return the cached inverse

cacheSolve <- function(specialMatrix, ...) {
  matrixInverse <- specialMatrix$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- specialMatrix$get()
  matrixInverse <- solve(data, ...)
  specialMatrix$setInverse(matrixInverse)
  matrixInverse
}
