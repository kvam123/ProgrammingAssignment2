## The program below cache the value of inverse of a matrix rather than compute it repeatedly
## There are two key functions defined in the program:
## 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: solves for the inverse of the input matrix
## The input of the first function will be a square matrix
## The output of the first function will be the input to the second
## See individual function comments for more details

makeCacheMatrix <- function(x = matrix()) {
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## create a list of functions that does the following:
## 1. sets the value of the input matrix in cache
## 2. gets the value of the input matrix from cache
## 3. sets the value of the inverse matrix in cache
## 4. gets the value of the inverse matrix from cache
##
## Args:
## x: a square matrix. Note that the function needs a square matrix for calculation of inverse
## 
## Returns:
## a list of special functions created inside. See function comments for more details
  
  m <- NULL
  
  # set a new input matrix
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  
  # get the original input matrix
  get <- function() x
  
  # set a new inverse matrix in cache
  setinverse <- function(mat_inv) m <<- mat_inv
  
  # return the inverse from cache
  getinverse <- function() m
  
  # return a list of functions created above
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



cacheSolve <- function(x, ...) {
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve function retrieves the inverse from the cache.
##
## Args:
## x: a list of functions returned from the makeCacheMatrix function
## 
## Returns:
## inverse of the input matrix. See function comments for more details
  
  # read the inverse from cache
  m <- x$getinverse()
  
  # read the original matrix from cache
  x_old <- x$get()
  
  # check if isn't null or same as before
  if(!is.null(m) && x != x_old){
    
    # the inverse was previously calculated and the input matrix is same as before
    # returning the matrix from cache
    
    message("getting inverse of matrix from cache")
    return(m)
  }
  
  # if condition failed, so calculating a new inverse
  
  data <- x$get()
  m <- solve(data,...)
  
  # storing inverse in cache by calling the setinverse function
  x$setinverse(m)
  
  # return the inverse
  m
  
}
