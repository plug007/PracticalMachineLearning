## makeCacheMatrix function stores/sets special matrix and contains functions setMatrix,
## getMatrix, myCacheInverse, getCacheInverse as a list

## setMatrix : sets value to matrix
## getMatrix : return the value of matrix
## myCacheMatrix : set inverse value of the matrix
## getCacheInverse : returns inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # MyCashe holds cashed data or be NULL
  MyCache <- NULL
  
  # store a matrix
  setMatrix <- function(newData)
  {
    x <<- newData
    # since the matrix is assigned a new value, flush the cache
    MyCache <<- NULL
  }
  
  # returns Matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  myCacheInverse <- function(save) {
    MyCache <<- save
  }
  
  # get the cached value
  getCacheInverse <- function() {
    MyCache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       myCacheInverse = myCacheInverse, 
       getCacheInverse = getCacheInverse)
}


## cacheSolve function calculates inverse of the matrix, but checks for already available cache,
## if no data exists, it calculates inverse. And updates cashe with latest calculation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getCacheInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- save(data, ...)
  x$myCacheInverse(i)
  i
}
