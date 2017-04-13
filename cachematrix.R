## R Programming Week 3 Assignment
## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special vector to 
## set, get, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <- y
    invrs <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve checks if inverse is cached. 
## If cached, retrieve it. If not, calculate inverse. 

cacheSolve <- function(x, ...) {
  invrs <= x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mat <- x$get()
  invrs <- solve(mat, ...)
  x$setinverse(invrs)
  invrs
}
