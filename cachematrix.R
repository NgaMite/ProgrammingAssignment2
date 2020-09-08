## This function creates a special "matrix" object that can cache its inverse.

## creates special matrix object and calculate its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## cache the inverse of the matrix from above function. If there is already
## inverse, it takes. if not, it calculate itself

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message ("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
