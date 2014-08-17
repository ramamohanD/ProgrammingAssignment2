# Taking the inverse of a matrix is a time consuming operation. If the contents
# of a matrix are not changing <-> make sense to cache the value of the inverse.
# So that when we need it again, it can be looked up in the cache rather than recomputing. 
# ----- Using scoping rules of the R language -----

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The below function computes the inverse of the special "matrix" returned by
## the above function. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # checking if matrix in not null (returning getinverse - Cache)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # get the matrix
  data <- x$get()
  # solve function - for calculating inverse
  # setting the inverse
  inv <- solve(data, ...)
  x$setinverse(inv)
  # returning inverse (last statement)
  inv
}
