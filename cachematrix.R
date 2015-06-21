##Functions that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse
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

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.
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