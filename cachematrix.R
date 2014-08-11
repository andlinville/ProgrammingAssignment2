## Two functions to create an object that stores a matrix and caches its mean.

## Creates a matrix from the argument (defaulting to an empty, 1x1 matrix) 
## within a closure and returns a list of getter and setter functions to 
## modify both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix created by makeCacheMatrix.
## First checks to see if inverse has already been computed, and, if so,
## returns the cached inverse. Otherwise, computes the inverse and returns.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
