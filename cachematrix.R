## These 2 functions work together to calculate the inverse of a
## matrix using solve(). To save time, the result can be cached and
## returned later without recalculating.

## makeCacheMatrix is a function containing a list to:
## - set the value of a matrix
## - get the value of the matrix
## - set the inverse of the matrix using solve()
## - get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    }


## The cacheSolve function calculates the inverse of a matrix using solve() 
## First, it checks if the inverse has already been calculated
## If it has, it returns the inverse from the cache instead

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
