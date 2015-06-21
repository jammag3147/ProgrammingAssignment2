##Functions that cache the inverse of a matrix

##Create matrix, which is a list containing functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


##Calculate inverse of the matrix created with above function, reusing cached result if available

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  m <- x$get()
  
  i <- solve(m, ...)
  
  x$setinverse(i)
  
  i
}