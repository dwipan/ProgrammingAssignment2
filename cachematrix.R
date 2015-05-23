## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes an invertible matrix as argument. variable i stores the inverse using setinverse() & returns
## the same using getinverse(). set() can be used to change the matrix x

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve take as argument matrix x created with makeCacheMatrix(x), it seraches for the inverse in the cache i.e.
## i variable using getinverse(), if not found it calculates using solve() method and stores in cache using setinverse()
## if inverse found in cache it returns the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)){
     message("inverse found in cache")
     return(i)
   }
   data <- x$get()  
   i <- solve(data)
   x$setinverse(i)
   i
}