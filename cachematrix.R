## Together, the functions makeCacheMatrix and cacheSolve provide a way to solve the
## inverse of a matrix without performing redundant calculations. If the inverse of a 
## particular matrix has already been solved, it returns the cahced data in lieu of
## performing the calculation again.

## Establish set and get functions to establish and return the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## establish set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## establish get function
  get <- function() x
  
  ## establish setInverse function
  setInverse <- function(solve) m <<- solve
  
  ## establish getInverse function
  getInverse <- function() m
  
  ## organize the 4 functions into a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solve the inverse. If the inverse for the matrix has already been solved, return cached data

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  ##If the inverse for the matrix has already been solved, return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If the matrix does not match the cached data, solve the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
