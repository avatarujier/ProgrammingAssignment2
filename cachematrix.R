## Caching the Inverse of a Matrix through makeCacheMatrix and cachaSolve
## (Cammel convention followed)

## Calculates the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  ## cache variable to store inverse matrix
  ## initialization = NULL for chacheSolve is.null checks
  inverseMatrix <- NULL
  
  ##As any language object oriented, we need set/get methods for access
  
  ##Matrix
  setMatrix <- function(matrix){
    ##store the matrix and clear the inverse
    x <<- matrix
    inverseMatrix <<- NULL
  }
  getMatrix <- function(){
    ##send it back
    x
  }
  
  ##Inverse
  setInverse <- function(inverse){
    ##store the result 
    inverseMatrix <<- inverse
  }
  
  getInverse <- function(){
    ##send it back
    inverseMatrix
  }
  ## expose the methods through coercion
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## Get the inverse of the matrix through cache rather than compute it repeatedly
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  ## check if the cache returns an existent matrix
  if (!is.null(inverse)) {
    print("Inverse Matrix found in cache")
    return(inverse)
  } else {
    ## compute the Inverse through the given Matrix
    inverse <- solve(x$getMatrix())
    ## pair it with makeCacheMatrix
    x$setInverse(inverse)
    return(inverse)
  }
}