##This program is useful if we want to calculate the inverse of a matrix multiple times.

## Makes a special matrix type
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function return the inverse of the previously defined special matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  print(data)
  inv <- solve(data)
  x$setinv(inv)
  inv
}
