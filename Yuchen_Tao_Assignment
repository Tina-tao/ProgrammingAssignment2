# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# It provides functions to set and get the matrix, as well as set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(mat) {
    matrix <<- mat
    inv <<- NULL
  }
  
  get <- function() matrix
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed, it retrieves the inverse from the cache.
# It utilizes the solve function in R for matrix inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) return(inv)
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  
  inv
  
}
