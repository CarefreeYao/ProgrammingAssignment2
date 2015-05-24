## This function solve the matrix for you
## it cache the inverse of maxtrix

## example
# > source('cachematrix.R')
# m <- makeCacheMatrix(matrix(c(5, 3, 0, 1), c(2, 2)))
# cacheSolve(m)
# [,1] [,2]
# [1,]  0.2    0
# [2,] -0.6    1

## Create a function matrix which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) i <<- inv
    get_inverse <- function() i
    list(
      set = set,
      get = get,
      set_inverse = set_inverse,
      get_inverse = get_inverse
    )
  }

## compute the inverse of the created matrix
## reusing cached result if it is available
cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$set_inverse(i)
  i
}
