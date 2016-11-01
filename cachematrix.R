## Function Creates a matrix object that can  cache
## the inverse of a matrix.

## The function includes four sub-functions:
## set, get, setinverse, getinverse, 

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) m<<- solve
  getinverse<- function() m
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function calculates the inverse of the matrix created 
## with "makeCacheMatrix'. It will use the cached version
## if the inverse has been already calculated

cacheSolve <- function(x, ...) {
        m<- x$getinverse()
      if(!is.null(m)){
        message("Getting CACHED data")
        return(m)
      }
      data<- x$get()
      m<- solve(data, ...)
      x$setinverse(m)
      m
}
