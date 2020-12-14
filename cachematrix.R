##These functions can use for inverse the matrix.These two functions can cache the inverse of a matrix.
##This function creates a special matrix that can cache its inverse.
##set the value, get the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  j <- NULL
  ## Method to set the matrix

  set <- function(y){
  x <<- y
  j <<- NULL
  }
  ## Method the get the matrix
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  ## Method to get the inverse of the matrix
  getInverse <- function() j
  ## Return a list of the methods
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

##This function computes the inverse of the matrix created with aforementioned function (makeCacheMatrix).
## Need first check if the inverse has already been calculated if so it can get the inverse from the cache and can skip the computation.
##Otherwise would be required to compute the inverse of the matrix and set the value of the inversion.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
 ## Get the matrix from our object
  mat <- x$get()
  j <- solve(mat,...)
## Set the inverse
  x$setInverse(j)      
## Return matrix
  j
}
