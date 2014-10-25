## Makes matrix which inverse can be cached internally

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    if ( is.na(x) == FALSE && all(x == y) ) {
      message("same data, keeping the cache")
    } else {
      inv <<- NULL
      x <<- y
      message("data was changed, clearing the cache")
    }
  }
  get <- function() x
  setinv <- function(inv_arg) {
    inv <<- inv_arg
  }
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x' using the cache if available

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  data <- x$get()
  if( !is.null(i) ) {
      message("getting cached data")
      return(i)
  }  
  i <- solve(data, ...)
  x$setinv(i)
  i
}
