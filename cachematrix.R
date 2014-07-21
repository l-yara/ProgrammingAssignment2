## Create a pair of function which together implement a "cache" - i.e. a structure which computes and stores 
## an inverse of a given matrix.

## Create a structure (vector) for a given matrix which stores a given matrix
## Should be used as a container only; the actual processing is provided 
## by the cacheSolve function.
## Accepts a matrix; returns a container object for cacheSolve operation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) m <<- inversion
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## Calculate an inversion of a matrix stored in the given container object and store it in the 
## container. If the function is called again on the same argument, use the cached value instead 
## of re-calculating.
## Accepts container object created by makeCacheMatrix function and additional parameters for 
## solve(...) function
## Returns the inversion of a matrix - either after calculation or taken from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
