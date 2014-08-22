## makeCacheMatrix generates a 'vector' pointing to a list of functions, 
## to be used by cacheSolve to cache rhe inverse of a matrix.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will return the inverse of a matrix. 
## If the inverse has been previuosly calculated it will be retrieved from th cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
