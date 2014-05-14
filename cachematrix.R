## R Programming - Programming Assignment 2 - Author: Ruben Adad

# Function makeCacheMatrix receives a square inversible matrix as argument 
# and builds a list of 4 functions: set, get, setsolve, getsolve

## Function set: could be use to setup de square matrix to be inverted. This function is only
## needed if we call makeCacheMatrix with empty arguments, i.e. m1 <- makeCacheMatrix(). In 
## this case we can use m1$set(mat1) to setup de matrix to be inverted (mat1 is a square matrix 
## with non-null values). The function stores the matrix to be inverted in variable "x". 

## Function get: is used to read the matrix to be inverted.

## Function setsolve: is used to call the "solve" function to invert the matrix.

## Function getsolve: is used to read a previously inverted matrix from cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function cacheSolve uses as argument a list build by the function makeCacheMatrix. It first
## checks if the inverted matrix is already cached, in this case it simply retrieves it; if the
## inverted matrix is not cached it invokes the "get" function to read the original matrix,
## then uses the "solve" function to invert it, finally it uses the function "setsolve" to
## store the inverted matrix in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
