## makeCacheMatrix() and cacheSolve() can be used to create a special matrix
## that allows to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse

  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(solve) {
    inverse <<- solve
  }
  
  getinverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  ## this function checks if the inverse of a matrix is already cached and returns it,
  ## otherwise it will calculate and store it for future calls.

  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
