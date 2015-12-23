## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # It holds the cache value or NULL if nothing is cached or no previous work
  # Initially nothing is cached so it is set as NULL
  cache <- NULL
  # Store the matrix
  setMatrix <- function(newMatrix){
    x <<- newMatrix
    # Flush the cache as the matrix is assigned a new value
    cache <<- NULL
  }
  # Return the stored matrix
  getMatrix <- function(){
    x
  }
  # Cache the given argument
  cacheInverse <- function(solve){
    cache <<- solve
  }
  # Get the cached value
  getInverse <- function(){
    cache
  }
  # Return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Calculation for the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the cached value
  inv <- x$getInverse()
  # Check whethe the cached value exists and if so, return it
  if (!is.null(inv)){
    message("Acquiring cached data")
    return(inv)
  }
  # Otherwise get the matrix and find its inverse and store in cache
  y <- x$getMatrix()
  inv <- solve(y)
  x$cacheInverse(inv)
  inv       # Return the inverse
}
