# Matrix inversion is usually a costly computation and there may be some
# benefit from caching the inverse of a matrix rather than to compute it repeatedly.
# So, these two functions were created to make this work.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL

  set <- function(value) {
    x <<- value
    cache <<- NULL
  }

  get <- function() x

  getInverse <- function(solve) cache <<- solve
  setInverse <- function() cache

  list(
       get = get
       set = set
       getInverse = getInverse
       setInverse = setInverse
       )
}

# This function calculates the inverse of the special "matrix" created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)

  inverse
}
