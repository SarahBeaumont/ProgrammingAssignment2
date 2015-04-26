## Two functions for matrix inversion caching and retrieval

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){ ##assign matrix value
  x<<-y
  m<<-NULL
}
get<-function() x ##gets the matrix above
setmatrix<-function(solve) m<<- solve ##assigns inverse value
getmatrix<-function() m ##retrieves inverse
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix() ##gets value of inverse matrix created
    if(!is.null(m)){
      message("getting cached data") ##own inverse
      return(m) ##returns own inverse
    }
    matrix<-x$get()
    m<-solve(matrix, ...) ##calculate inverse
    x$setmatrix(m) ##cache inverse
    m ##return inverse
}
