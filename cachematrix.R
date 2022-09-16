## The functions creates a special "matrix" object 
## which caches its inverse. The inverse is 
## calculated with cacheSolve. They take advantage
## on Lexical Scoping.

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse, that is calculated with
## cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(set) inverse <<- set
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve either retrieves the pre-calculated inverse
## from the cache, or it calculates the inverse of the matrix
## and then prints it.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse 
}
