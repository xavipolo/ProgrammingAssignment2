## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list with
# set -> set the value of the matrix
# get -> get the value of the matrix
# setInverse -> set the of inverse of the matrix
# getInverse -> get the of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  invMat <- NULL
  
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) invMat <<- inverse
  
  getInverse <- function() invMat
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Return the inverse of a matrix. If the result is not cached, then it's computed.
cacheSolve <- function(x, ...) {
  
  #get cache
  invMat <- x$getInverse()
  
  if(is.null(invMat)) {
    #not cached - calculate inverse matrix
    message('solve it')
    
    invMat <- solve( x$get() )
    x$setInverse(invMat)
    
  }
  
  return(invMat)
  
}

# m = makeCacheMatrix( rbind( c(1,1,0), c(1,0,1), c(0,1,0) ) )
# m$get()
# cacheSolve(m)
# cacheSolve(m)
