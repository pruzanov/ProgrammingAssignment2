#  This code is based
#  on the mean-calculating example provided in the
#  instructions to Assignment 2 of Week 3 
#  R Programming course 
#  (https://www.coursera.org/learn/r-programming)

## makeCacheMatrix
#  This function build a special object
#  with functions for getting or setting either matrix
#  or inverse of the matrix. Will cache
#  inverse if it is available already

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
#  This function takes in special 'matrix' object 
#  and returns either cached or newly solved inverse
#  of the matix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Testing code 
# create a square matrix and build a custom object from it
mtest<-matrix(c(1,23,2,4,8,15,10,3,14), nrow = 3, byrow=FALSE)
m<-makeCacheMatrix(mtest)

# Calculate the inverse of matrix using special object m
inverse<-cacheSolve(m)
inverse

# This should use the cached value and skip the calculation
# A message will pop up saying that cache is being used
inverse<-cacheSolve(m)
inverse
