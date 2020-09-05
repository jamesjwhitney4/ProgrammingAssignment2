## Put comments here that give an overall description of what your
## functions do

## This function creates a "matrix" object that can cache its inverse.

#Example uses:
#jwmatrix <- makeCacheMatrix(matrix(1:4,2,2))
#jwmatrix$get()
#cacheSolve(jwmatrix)
#jwmatrix$getInv()
#cacheSolve(jwmatrix)

#And this matrix multiplication produces identity matrix
#jwmatrix$get() %*% cacheSolve(jwmatrix)

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv_mat <<- inverse
  getInv <- function() inv_mat
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getInv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setInv(inv_mat)
  inv_mat
}
