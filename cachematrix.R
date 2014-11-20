## cachematrix.R contains two functions that create a matrix and determine its inverse
## with a statement indicating if the value is cached before returning results.

## makeCacheMatrix creates and sets default values for matrix and solution

makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL
  
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }

  get <- function() {m}
  
  setInverse <- function(inverse) {i <<- inverse}
  
  getInverse <- function() {i}
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve checks if the solution to a matrix is cached and indicates before returning

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data) %*% data  ## syntax for finding matrix inverse
  x$setInverse(m)
  
  m
  
}

