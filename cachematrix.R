## Put comments here that give an overall description of what your
## functions do

## This function MakeCacheMatrix creates a special "matrix", wich is
## really a list

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates the inverse of the special "matrix"
## created above. It first checks if the inverse of the matrix 
## has already been calculatedf so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the 
## cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
