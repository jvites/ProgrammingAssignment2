#  Use of Lexical Scoping to Create Cashed Objects
### In this example an inverted matrix is cached
## Description
### This code uses lexical scoping to evaluate the inverse of a matrix the very
### first time it is asked to do so, it evaluates it and caches the inverted matrix
### in the variable "inv", subsequent requests to evaluate the inverse of that matrix,
### result in the function returning the cached inverted matrix.
### It consists of two related functions

## First Function
### The first function "makeCacheMatrix" 
### creates and returns a list of functions that operate on
### the selected matrix, and caches inverted matrix. 
### This function needs to be called before the second function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   #### initializing the cached value
  set <- function(y) {  ## This function
          x <<- y   #### stores matrix passed as argument (y) into local variable x
          inv <<- NULL    #### initializes the cache for the inverted matix
  }
  get <- function() x  #### it has no arguments and returns the "current" matrix x
  setinverse <- function(inverse) inv <<- inverse    #### sets the cache to the inverse of current matrix
  getinverse <- function() inv    #### returns the inversed of the matrix from cache
  
  #### This function ends by returning the four functions that operate on the matrix and its inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Second Function
### This is the function that is called every time the inverted matrix is required
### This function calculates the inverse matrix using the solve() function but 
### only when the inverted matrix is not already cached.
cacheSolve <- function(x, ...) {
  #### Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  #### obtains the inverse from cache
  if(!is.null(inv)) {    #### checks if the value has been store in chache
    message("getting cached data");  #### if in cache, warns of cached matrix
    return(inv)    #### returns cached matrix and ends
  }
  #### If it id NULL, means the inverse has not been calculated
  mtrx <- x$get()    #### retrieves the matrix stored in X
  inv <- solve(mtrx)  #### uses R function solve() to evaluate the inverse
  x$setinverse(inv)   #### sets the cache to the value of the inverse matrix
  inv     #### returns the inverse of the matrix.
}
