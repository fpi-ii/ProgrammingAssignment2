## Computing the inverse of a matrix, if the inverse was previously computed
## it retrieves the inverse from a cached variable.

## Creates a special matrix data structure (a LIST) which stores the matrix x and 
## provides a cache variable for its inverse (invrs). 
## Example: 
##    m1 <- makeCacheMatrix() // creates special matrix m1 with invrs NULL
##    m1$set(matrix(1:9, 3, 3)) // stores a 3x3 matrix in m1, invrs is still NULL
##

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  
  setinvrs <- function(solve) invrs <<- solve
  getinvrs <- function() invrs
  
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)

}


## Computes the inverse of the matrix stored in the parameter structure (LIST)
## Parameter: the 1st parameter is a list of the type 'makeCacheMatrix'
## If the structure's cache component is NULL, the function will compute the inverse, 
## store it in the cache component (invrs), and return the inverse. Otherwise 
## it will return the cached inverse.
## Example:
##    cacheSolve(m1) // will compute the inverse and store it in invrs, return the matrix inverse
##    cacheSolve(m1) // will return the inverse previously stored in invrs

cacheSolve <- function(x, ...) {
  
  invrs <- x$getinvrs()
  
  if(!is.null(invrs)) {
    message("Getting cached data ...")
    return(invrs)
  }
  mat <- x$get()
  
  ## check if the data stored in x is of the correct type
  if(class(mat)!='matrix' || ncol(mat)!=nrow(mat)) {
    message("Not a matrix, or not a square matrix")
    return(NULL)
  }
  
  invrs <- solve(mat, ...)
  x$setinvrs(invrs)
  invrs
  
}
