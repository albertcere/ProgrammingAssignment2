## Custome cached matrix in order to avoid computing
## more than once the inverse of each matrix

## Custom matrix creation

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Solves the inverse if no cached inverse matrix is found,
## otherwise gets the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## Check the cached value
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## No cached value
  message("no cached inverse matrix")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
