

## This function sets the value of matrix, gets the value of matrix, 
## sets the value of inverse, gets the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setinv<-function(solve) i<<-solve
  
  getinv<-function() i
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Checks if inverse is present in cache. If present, retrieves it else 
##computes the inverse and stores it in the cache.

cacheSolve<- function(x, ...) {
  i<-x$getinv()
  if(!is.null(i)){
    message("showing cached inverse matrix")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinv(i)
  i
}
