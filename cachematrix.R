

makeCacheMatrix <- function(x = matrix()) {
  invt <- NULL
  set <- function(y) {
    x <<- y
  invt <<- NULL
}
get <- function() x
setInverse <- function(inverse) invt <<- inverse
getInverse <- function() invt


list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)

}



cacheSolve <- function(x, ...) {
  invt <- x$getInverse()
  if(!is.null(invt)) {
    message("getting cached data")
    return(invt)
  }
  data <- x$get()
  invt <- solve(data, ...)
  x$setInverse(invt)
  invt
        ## Return a matrix that is the inverse of 'x'
}


