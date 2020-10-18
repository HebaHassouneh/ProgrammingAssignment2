#**MakeCacheMatrix**

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
        #  set function
  set <- function(y) {
    x <<- y
    m <<- NULL
  } # set end
        
        #  get function
  get <- function() x
        
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#**CacheSolve**

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
         ## Return a matrix that is the inverse of 'x'
  x$setinverse(m)
  m
       
}
