#  Use of Lexical Scoping to Create Cashed Objects
### In this example an inverted matrix is cached
## Description
### This code uses lexical scoping to evaluate the inverse of a matrix the very
### first time it is asked to do so, it evaluates it and caches the inverted matrix
### in the variable "inv", subsequent requests to evaluate the inverse of that matrix,
### result in the function returning the cached inverted matrix.
### It consists of two related functions

## First Function
### The first function creates and returns a list of functions that operate on
### the selected matrix, and caches inverted matrix. 
### This function needs to be called before the second function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Second Function
### This is the function that is called every time the inverted matrix is required
### This function calculates the inverse matrix using the solve() function but 
### only when the inverted matrix is not already cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data");
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setinverse(inv)
  inv
}
