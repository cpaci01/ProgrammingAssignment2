## When run together, this pair of functions both calculates the inverse
## of a given matrix and caches the values of all already calculated 
## inverses in the parent environment. Caching them in the parent 
## environment makes it possible to refer back to the results of past
## calculations so they do not need to be performed more than once.

## Given a matrix x, makeCacheMatrix sets up a sequence of four functions:
## 1. set: Defines the matrix based on your input
## 2. get: Retrieves the matrix to perform calculations on it
## 3. setinverse: Defines the calculation to be performed (solving for the inverse)
## 4. getinverse: Performs the calculation (solve for the inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(solve) {
    inv <<- solve
  }
  getinverse <- function() {
    inv
  }
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve first looks to see whether the inverse of a given matrix
## x is already stored in the parent environment. If it is, the inverse
## is returned. If not, the sequence created by makeCacheMatrix is used
## to calculate the inverse.

cachesolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
