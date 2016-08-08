## Cache the inverse of a square matrix.  Use the cached version of the matrix
##    when calculating the inverse is not necessary (saving significant processing time)
##
## Functions "makeCacheMatrix" and "cacheSolve" 

## Function "makeCacheMartix" builds a list of functions and is input 
##    to the "cacheSolve" function

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    InverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InverseMatrix <<- inverse
  getInverse <- function() InverseMatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function "cacheSolve" inverses matrix "x" or returns the already 
##    inversed matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getInverse()
  if(!is.null(InverseMatrix)) {
    message("getting cached data")
    return(InverseMatrix)
  }
  data <- x$get()
  InverseMatrix <- solve(data, ...)
  x$setInverse(InverseMatrix)
  InverseMatrix
}
