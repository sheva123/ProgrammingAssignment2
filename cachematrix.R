## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y = matrix()) {
    x <<- y 
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invVal) {
    inv <<- invVal 
    return(inv)
  }
  
  getInverse  <- function() inv
  list(set=set, get=get, setinv=setinv, getInverse=getInverse)
}

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
  calInv <- x$getInverse() 
  
  if(!is.null(calInv) && is.matrix(calInv)) { 
    message("Found cached data")
    return(calInv)
  }
  
  matrixToSolve <- x$get()  
  message("Setting the value of inverse to:") 
  x$setinv(calInv)
}
