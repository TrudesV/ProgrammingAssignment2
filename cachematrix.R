##The first function creates a special "matrix" that can cache its inverse.  The
##second function computes the inverse of the special "matrix" if it had not been
##calculated; otherwise it retrieves the inverse from the cache.

#makeCacheMatrix creates the special "matrix".

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get<-function() x
      setinverse<-function(inverse) m<<- inverse
      getinverse<-function() m
      list(set=set, get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}
#cacheSolve calculates the inverse of the special "matrix" created with 
#makeCacheMatrix. If the inverse has already been calculated it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates the inverse
#of the matrix and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m<-x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      matrix<-x$get()
      m<-solve(matrix, ...)
      x$inverse(m)
      m
}