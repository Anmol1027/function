## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## matrix starts from scratch
  set <- function(y){
    x <<- y  
    ## assigned from different environment using <<-
    inv <<- NULL  
    ## assigned from different environment using <<-
  }
  get <- function() x
  sInv <- function(solveMatrix) inv <<- solveMatrix
  gInv <- function() inv
  list(set = set, get = get, sInv = sInv, gInv = gInv)
  ## gets values and caches the new matrix
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$gInv()
  if(!is.null(inv))  ## Checks if matrix is not null
    {
    message("getting cached data")
    return(inv)   ## Matrix is returned if it's noot null
  }
  data <- x$get()
  inv <- solve(data)  ## Solves inverse of the checked matrix
  x$setInverse(inv)   ## Sets inverse matrix and is outputted
  inv      
}