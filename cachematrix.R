# Matthew Rodriguez
# 7/31/22
#Coursera Assignment 2

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  INVERSE<- NULL
  set <- function(y){
    x <<- y
    INVERSE <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) INVERSE<<- solveMatrix
  getInverse <- function() INVERSE
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INVERSE<- x$getInverse()
  if(!is.null(INVERSE)){
    message("getting cached data")
    return(INVERSE)
  }
  data <- x$get()
  INVERSE<- solve(data)
  x$setInverse(INVERSE)
  INVERSE     
}
