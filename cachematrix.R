##  Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function(){x}
  setInverse<-function(inverse)(inv<<-inverse)
  getINverse<-function()(inv)
  list(set=set,get=get,setInverse=setInverse,getINverse=getINverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("Please wait while we retreive your cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
