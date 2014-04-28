# `makeCacheMatrix`: This function creates a "special matrix" object
#  that can cache the matrix and its inverse.
#  set function sets the value of the matrix(also sets inv Null for
#  change record)
#  get function gets the value of the matrix
#  setinv function sets and caches the inverse of matrix
#  getinv function gets the value of the inverted matrix
#  Assumption : the matrix x to the function is inversible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv <<- inverse
  getinv <- function() inv
  #Return the list of functions 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
##`cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  # Gets the original matrix 
  data <- x$get()
  if(length (which (is.na(m))) != 0) { stop("matrix data is NA") }
  # Compute inverse and cache it.
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Usage :
## source("cachematrix.R")
## sm <- makeCacheMatrix(matrix())
## cacheSolve(sm)
## sm <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(sm)
