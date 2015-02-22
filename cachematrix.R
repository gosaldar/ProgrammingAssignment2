## Programming Assignment 2:
## Learn how to cache the value of expensive operation in R.

## This function set the value of an inverse matrix in as cache value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x  
  setInverse <- function(solve) m <<- solve  
  getInverse <- function() m  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)  
}


## This function check whether value of inverse matrix already cached; if not compute the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
