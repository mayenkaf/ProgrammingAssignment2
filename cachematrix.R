## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse
## set value of the matrix
## get value of the matrix
## set value of the inverse of matrix
## get value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve=getsolve)
}


## Write a short comment describing this function
## Computes the inverse of the special matrix returned by makeCacheMatrix,
## if matrix inverse was previously calculated, it is returned, otherwise,
## calculate the inverse and cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached inverse matrix")
    return(m)
  }
  matrix_data <- x$get()
  m <- solve(matrix_data,...)
  x$setsolve(m)
  m
}
#MFCh.