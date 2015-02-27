## Put comments here that give an overall description of what your
## functions do

## decorates an input object x with get/set methods for the inverse value
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the function accepts an cachable matrix object, which can save/restore its inverse value
## it checks if the x object allready has an inverse calculated and if so, it returns it, otherwise it solves it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
