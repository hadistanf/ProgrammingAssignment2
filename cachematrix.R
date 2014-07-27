## cachematrix.R makes use of a pair of functions to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
    
  ## Input:
  ##        x: a "square" numeric matrix that is "invertible"
  ## Output:
  ##        a special matrix object that can cache its value. It's a list containing:
  ##          - set function to set the value of the matrix
  ##          - get function to get the value of the matrix
  ##          - setInverse function to set the inverse matrix
  ##          - getInverse function to get the inverse matrix
  
  inverse_matrix <- NULL
  set <- function(y) {
    
    x <<- y
    inverse_matrix <<- NULL
  
  }
  
  get <- function() x
  setInverse <- function(inverse) inverse_matrix <<- inverse
  getInverse <- function() inverse_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" (x) returned by makeCacheMatrix()
## If the inverse has already been calculated (and the matrix has not changed), then the
##  cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Input: 
  ##       x: special matrix object (output from makeCacheMatrix)
  ## Output:
  ##       the inverse of the special matrix x
  
  inverse_matrix <- x$getInverse()
  
  if(!is.null(inverse_matrix)) {
  
    message("getting cached matrix")
    return(inverse_matrix)
    
  }
  
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
  
}