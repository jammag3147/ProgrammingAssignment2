##Functions that cache the inverse of a matrix
##
##Example:
##  source('cachematrix.R')
##  v <- c(3,4,1,2)
##  a <- makeCacheMatrix(matrix(v, c(2,2)))
##  cacheSolve(a)
##          [,1] [,2]
##    [1,]    1 -0.5
##    [2,]   -2  1.5
##  cacheSolve(a)
##    Getting Cached Data
##          [,1] [,2]
##    [1,]    1 -0.5
##    [2,]   -2  1.5




##  This function creates a special "matrix" object, which is a list containing functions to:
##    set the value of the matrix (set)
##    get the value of the matrix (get)
##    set the value of the inverse matrix (setinverse)
##    get the value of the inverse matrix (getinverse)

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



##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated (and the matrix has not changed),
##    then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of "x"
  i <- x$getinverse()
  ##Checks for cached data - prints message and uses data if found
  if(!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  
  m <- x$get()
  
  i <- solve(m, ...)
  
  x$setinverse(i)
  
  i
}