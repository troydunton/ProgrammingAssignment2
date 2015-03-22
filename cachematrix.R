## Put comments here that give an overall description of what your
## functions do

##The functions makeCacheMatrix and cacheSolve cache the calculation
## for the inverse of a matrix and recall that calculation when the
## contents of the matrix have not changed

## makeCacheMatrix initializes the calculation for the inverse by setting
## the matrix and its inverse and getting the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv  <- function(solve) m <<- solve
  getinv  <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve first checks to see if an inverse has already been 
## calculated for the matrix, and if so, returns the value.  If
## it has not been calculated, it computes and stores the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null()) {
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
