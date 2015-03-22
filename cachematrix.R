## makeCacheMatrix: creates a special list from a matrix
##                  which can cache its inverse in itself
## x_inverse stores in the inverse of the matrix
## x stores the actual matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inverse <<- inverse
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: solves a matrix to find its inverse if it has not been
##             computed before and returns the cached version if it has
##             already been computed
## x is the special cache-able matrix
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
