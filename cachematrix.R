## this function makes a matrix that cahces its inverse

makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  setMatrix <- function(y) {
    x <<- y
    in <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) in <<- inv
  getInverse <- function() in
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setInverse,
       getinverse = getInverse)
}


## this function calculates the inverse of the above matrix; retrieves the inverse from cache if it has already been solved

cacheSolve <- function(a, ...) {
  in <- a$getInverse()
  if (!is.null(in)) {
    message(" cache inverse matrix")
    return(in)
  }
  data <- a$getMatrix()
  sol <- solve(data, ...)
  a$setinverse(sol)
  sol
}
