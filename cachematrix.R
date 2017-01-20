## The functions make a special object i.e. a matrix and then
## caches its own inverse in order to save time.

## This function creates a special object being a matrix which can 
## caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
get = function() x
setinv = function(inverse) inv <<- inverse
getinv = function() inv
list(set = set,
     get = get,
     setinv = setinv,
     getinv = getinv)
}


## This function is used to calculate the inverse of the special
## object created by the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }

  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}