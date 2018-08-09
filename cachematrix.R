## makeCacheMatrix creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL # Clear the cache
  }
  
# Define function to get the value of the matrix
  get <- function() x
# Define function to set the inverse.Only used when there is no cached inverse
  setinverse<- function(inverse) inv <<-inverse
# Define function to get the inverse
  getinverse <- function() inv
# Return a list with the above four functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix 
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv)) { 
  # If the cache was not empty, we return this message
    message("getting cached inverse matrix")
    return(inv)
  } else {
  # The cache was empty. We need to calculate it, cache it, and then return it.
  # Calculate the inverse
    inv_x <- solve(x$get())
    x$setinverse(inv)
  # Return the inverse
    return(inv)
  }
}
