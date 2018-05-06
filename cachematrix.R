## makeCacheMatrix() 
## 

## Return a list of functions to set/get an input matrix and its inverse

makeCacheMatrix <- function(matrx = matrix()) {
  inverse <- NULL
  setInput <- function(z) {
    matrx <<- z
    inverse <<- NULL
  }
  getInput <- function() matrx 
  setOutput <- function(inv) inverse <<- inv
  getOutput <- function() inverse  
  list(getMatrix = getInput, setMatrix = setInput, 
       setInverse = setOutput, getInverse = getOutput)
}

## Return the inverse of acach matrix; compute only if needed

cacheSolve <- function(mcm, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- mcm$getInverse()
  if(!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  data <- mcm$getMatrix()
  i <- solve(data, ...)
  message("returning computed inverse")
  mcm$setInverse(i)
  i
}
