## Saves time computing inverse of matricies by cacheing inverse 

## Creates matrix object that can cache its inverse 

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
    setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix returned by makeCacheMatrix. 
##If the inverse has already been calculated the inverse is retrevied 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting inverse")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
