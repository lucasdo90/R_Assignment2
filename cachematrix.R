## This file creates a list which contains funtions to get 
## the value of a matrix and return the value of inversion of that matrix 

## this function get the value of the matrix and its inversion 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv 
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function return the value of the inversion. If the inversion was
## already computed, then the function will return the message and the result. 
## If not, the inversion matrix will be computed and returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()

  if (!is.null(inv)){
  message("getting cached data")
  return(inv)
  }
  mat_data = x$get()
  inv = solve(mat_data,...)
  x$set_inv(inv)
  return(inv)
}

## The model to try the function 
x <- matrix(1:4,2,2)
m = makeCacheMatrix(x)
m$get()
a <-cacheSolve(m)
a


## The results:

## m$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## a <-cacheSolve(m)
## a
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


