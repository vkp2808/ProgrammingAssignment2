## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL        # inverse matrix
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Set the inverse matrix
  setinvm <- function(inverse) invm <<- inverse
  
  # Get the inverse Matrix
  getinvm <- function() invm
  
  # Return the matrix 
  list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  
  # If the inverse is already present, return from cache
  if (!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  
  # The inverse is not present, so we calculate it
  data <- x$get()
  invm <- solve(data, ...)
  
  # Cache the inverse
  x$setinvm(invm)
  
  # Return inverse matrix
  invm
       
  
}
