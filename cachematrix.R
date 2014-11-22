makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inputMatrix <- x$get()
  inverse <- solve(inputMatrix, ...)
  x$setinverse(inverse)
  inverse
}