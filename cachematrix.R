## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix create a special matrix to chache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
      x<-y
      i<-NULL
    }
    get <- function() x
    setinv <- function(inverse) i<<-inverse
    getinv <- function()i
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## cacheSolve calculate the inverse of the matrix and store it in the cache. if you need to recall it it won't compute again.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    mat <- x$get()
    i <- solve(mat,...)
    x$setinv(i)
    i
}





