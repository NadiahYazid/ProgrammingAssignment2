# R Programming Language, Programming Assignment 2
#
# #Matrix inversion is usually a costly computation and there may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss here). 
# Given assignment was to write a pair of functions that cache the inverse of a matrix.

# 1. makeCacheMatrix: This function creates a special "matrix" object that 
#can cache its inverse.

makeCacheMatrix <- function(d = matrix ()){
  inverseMatrix <- NULL #Set inverseMatrix to NULL
  
  set <- function (i){
    d <<- i
    inverseMatrix <<- NULL
  }
  
  get <- function() d 
  setInverse <- function(inverse) inverseMatrix <<- solve(d) #To calculate the Inverse
  
  getInverse <- function() inverseMatrix
  
  list(set = set,
       get = get,
       setInverse =setInverse,
       getInverse = getInverse)
}

# # 2. cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inverseMatrix <-x$getInverse()
  if(!is.null(inverseMatrix)){
  message("Retrieving cached data!")
  return(inverseMatrix) ## Return a matrix that is the inverse of 'x'
  }
  
  storingData <- x$get()
  inverseMatrix <- solve(storingData)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

