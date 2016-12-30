## makeCacheMatrix () is a function which returns a list with 4 functions
## get() - returns the matrix from the main function argument into the makeCacheMatrix ()
## set() - sets the value of the vector
## getinverse() - stores the value of the inverse to be returned to the calling function
## setinverse() - stores the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
  
}


## cacheSolve() is a function that calculates the inverse of a matrix
## it first checks the makeCacheMatrix to see if there is a inverse already calculated
## if it finds one, its returned.
## otherwise the function calculates the inverse, and sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  getdata <- x$get()
  m <- solve(getdata, ...)
  x$setinverse(m)
  m
}
