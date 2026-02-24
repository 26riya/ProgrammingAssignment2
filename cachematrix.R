## These functions create a special matrix object that can cache its inverse.
## If the inverse has already been computed, it will return the cached value
## instead of recalculating it.

## makeCacheMatrix creates a special matrix object that can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix.
## If the inverse is already cached, it retrieves it from memory.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
